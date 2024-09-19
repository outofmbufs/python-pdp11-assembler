# MIT License
#
# Copyright (c) 2024 Neil Webber
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import operator
from astokens import TokenID
from asx import _UndefinedSymbol, _SymbolTypeMismatch


# XNodes, and subclasses, represent:
#
#    Constant values                  :  17
#    Registers                        :  r3
#    Symbol references                :  foo
#    Instruction opcodes and formats  :  mov, 2-operand, opcode=010000
#    Individual operand encodings     :  (rN)+, expr(rN), etc.
#    Fully parsed inst w/operands     :  mov something,r0
#    Mathematic Expressions           :  something = banana + 2; banana = 1
#    Numeric labels                   :  support for 1f/1b etc labels
#    Pseudo-operations                :  .if, .byte, etc
#
# The semantics of unix v7 'as' regarding bizarre combinations of these
# types are, ummm, bizarre. This implementation matches most, BUT NOT ALL,
# of those semantics. It matches the "useful" ones, where "useful" as been
# arbitrarily defined as: whatever got used in mch.s for the unix bootstrap
# and in the assembler sources (as11.s .. as29.s) (the assembler itself was
# written in assembler). Weird corner cases like "foo = mov + .if" may not
# match 'as' semantics; don't use them.
#
# CARET
#
# The caret operator allows special instructions to be defined as
# being "just like this other instruction, only different opcode"
# For example:
#
#       mfpi = 006500^tst
#
# defines the mfpi instruction which will then accept the same single
# operand that tst does, but use 006500 as the base opcode (into which
# the 6 bits of the operand will be placed). Notice that this line not
# only establishes an opcode for mfpi, but makes this sort of thing work:
#
#      mfpi -(sp)
#
# because mfpi inherits the parse method from "tst".
#
# ADJACENCY IMPLIES ADDITION
#
# Beyond CARET, another way special instructions were defined is by
# simple addition. This is done for 'spl' in the boot code for example:
#
#    spl = 230
#
# which then allows statements such as:
#
#    spl  7
#
# taking advantage of the expression semantic that two adjacent expression
# elements with no intervening operator are equivalent to addition.
# The above sequence puts a 237 into the instruction stream.
#
# XNode ATTRIBUTES/METHODS:
#
#    relseg            -- None for ABSOLUTE, else a text/data/bss segment
#
#    parse()           -- if this symbol can start a statement, then
#                         this method exists and implements the parse.
#
#    symrefs()         -- symbols referenced (for loop detection)
#
#    resolve()         -- If a value can be computed, return it (still as
#                         an XNode of some type). This will often, BUT NOT
#                         ALWAYS, be a Constant, and if there is a non-None
#                         relseg it needs to be interpreted in a relative way.
#
#    clone()           -- Return a duplicate object but with the new
#                         provided value. This is key to type-propagation,
#                         which is part of how the CARET operator works.
#
#    nbytes            -- the size, in bytes, of the byteseq the node will
#                         produce in the second pass. NOTE: can be zero.
#
#    byteseq()         -- NOTE: Results only correct/valid after the
#                         segments have been sized and had their offsets
#                         established. Returns a bytes() object, in
#                         PDP-11 byte order.
#


#
# SUBCLASSES
#   Subclasses for pure expressions are defined here.
#   Additional subclasses for instruction formats are defined in asparse
#   Symbol-table specific XNode subclasses are in symtab
#

class XNode:
    """Generic base class for expression nodes."""

    NOVALUE = object()

    def __init__(self, value, *, relseg=None, tok=None, **kwargs):
        """Generic XNode. Attributes defined here:
          value -    the (type-specific) value of the node. If the node has
                     nothing that fits this concept, specify XNode.NOVALUE
                     at init and this attribute will not even be created.

          relseg -   None for ABSOLUTE, else a segment the value should
                     be considered "relative to"

          tok    -   Primarily for debug or possibly error messages.
                     If not None, a Token "primarily causing" this XNode
        """

        # If a subclass wants to super().__init__ but does not
        # have the concept of a simple "value", use the NOVALUE
        # sentinel (because None might be a legit value)
        if value is not self.NOVALUE:
            self.value = value
        self.relseg = relseg
        self.tok = tok

        # subclasses can (usually) leverage the generic clone() method
        # provided here if they pass in their extra kwargs for saving...
        self._kwargs = kwargs

    def clone(self, altvalue):
        """Generic clone; return object of same type but new value."""
        return self.__class__(
            altvalue, relseg=self.relseg, tok=self.tok, **self._kwargs)

    def resolve(self):
        """Generic resolve; resolves it itself."""
        return self

    def symrefs(self):
        """Generic symref; no symbols referenced."""
        return []

    def __repr__(self):
        s = f"{self.__class__.__name__}("
        sep = ""
        for v in vars(self):
            if not v.startswith('_'):
                if v == 'relseg':
                    if self.relseg is None:
                        continue
                    vs = self.relseg.segID
                else:
                    vs = str(getattr(self, v))
                s += f"{sep}{v}={vs}"
                sep = ", "
        s += ")"
        return s

    @staticmethod
    def __littlegen(words):
        """Return a sequence of 8-bit values (as native integers)

        'words' should be a sequence of values 0..65535.
        Returns a sequence of values 0..255 representing the same
        values, in little-endian order.
        """
        for w in words:
            if w < 0 or w > 65535:
                raise ValueError(f"Illegal 16-bit value: {w}")
            yield w & 0o377
            yield (w >> 8) & 0o377

    @classmethod
    def _w2b(cls, words):
        """Convert a sequence of words to (little-endian!) python bytes().

        words can be: a sequence, a naked integer, or None.
       """
        try:
            return bytes(cls.__littlegen(words))
        except TypeError:
            # words was an integer, or, at least, not iterable
            return bytes(cls.__littlegen((words,)))

    # this default works for many subclasses; others will override.
    nbytes = 2

    # this default works for any subclass that:
    #    is zero length (sets nbytes to zero)
    # OR has a resolve() that results in anything integer-like with a .value
    # OR has a resolve() that returns None
    #
    # Obviously, subclasses with different requirements can override.
    def byteseq(self):
        if self.nbytes == 0:
            return bytes()

        c = self.resolve()
        if c is None:
            return bytes()
        val = getattr(c, 'value', None)
        if val is None:
            return bytes()

        # If the resolved value is segment-relative, adjust it.
        if c.relseg is not None:
            val += c.relseg.offset
        return self._w2b(val & 0xFFFF)


class Constant(XNode):
    """A direct constant appearing in an expression."""
    # Turns out everything at the generic layer works perfectly as
    # "just a constant". This class exists for notational/type reasons.
    pass


class Register(XNode):
    """A constant (0..7) representing r0..r7 registers."""

    # Registers with values outside 0..7 can happen from expressions like
    #    foo = r5 + 5
    # because the type propagation rules will preserve the "Register" type.
    # Indeed this sequence:
    #    foo = r5 + 5
    #    bar = foo - 6
    #    tst bar
    # assembles to 'tst r4' in 'as'.
    # Thus, no custom logic to limit __init__ values here. Instead,
    # Register values can be arbitrary values. However, out of
    # range values become a naked Constant when resolved. This matches 'as'
    # semantics which makes, e.g., "rts pc+1" illegal but "rts pc-1" ok.
    def resolve(self):
        if self.value > 7:   # it "can't" be <0 that will wrap around
            return Constant(self.value)
        else:
            return self


# all expressions are "binary"; the few unary operations are expressed
# as binary equivalents typically with a zero second operand

class BinaryExpression(XNode):
    def __init__(self, left, op, right, /, **kwargs):
        super().__init__(self.NOVALUE, **kwargs)
        self.left = left
        self.op = op
        self.right = right

    def symrefs(self):
        return self.left.symrefs() + self.right.symrefs()

    BINOPS = {
        TokenID.PLUS: operator.add,
        TokenID.MINUS: operator.sub,
        TokenID.STAR: operator.mul,
        TokenID.VSLASHES: operator.floordiv,
        TokenID.AMPERSAND: operator.and_,
        TokenID.BAR: operator.or_,
        TokenID.RSHIFT: operator.rshift,
        TokenID.LSHIFT: operator.lshift,
        TokenID.PERCENT: operator.mod,
        TokenID.CARET: lambda a, b: a,

        # BANG NOTE: The 'as' documentation says that a!b is
        # implemented as "a or (not b)" where "not b" means ones-complement.
        # HOWEVER, testing 'as' and examining the source of 'as' reveals it
        # is actually "a + (not b)". Here's the source:
        #
        #      exnot:
        #        jsr r5,combin; 0          / this is for the type propagation
        #        com r1                    / "not b"
        #        add r1,r2                 / ADD (instead of "OR")
        #
        # Hence this lambda, with "+" not "|". In practice, BANG is
        # generally used as a unary ('a' is zero) making this detail moot.
        TokenID.BANG: lambda a, b: (a + ~b) & 0xFFFF,
    }

    def _compatsegs(self, a, b):
        """Return True if two XNodes have 'compatible' segments"""
        # to be compatible, one or the other must be ABSOLUTE (None)
        # or else they must match
        return a.relseg is None or b.relseg is None or a.relseg == b.relseg

    def resolve(self):
        lc = self.left.resolve()
        rc = self.right.resolve()

        lc_abs_untyped = lc.relseg is None and isinstance(lc, Constant)
        rc_abs_untyped = rc.relseg is None and isinstance(rc, Constant)

        # 'as' has some unusual rules for combining types, some of which
        # even the 'as' paper admits are of little use (e.g., what
        # type should "foo = mov + clr" be?).  This emulates the
        # important cases and makes an error out of the rest.
        if self.op == TokenID.CARET:
            # CARET is special; force the right hand side to be the type
            cloner = rc
        elif isinstance(lc, Constant) and lc_abs_untyped:
            cloner = rc              # op with abs/untyped preserves other
        elif isinstance(rc, Constant) and rc_abs_untyped:
            cloner = lc              # op with abs/untyped preserves other
        # segrelative MINUS segrelative results in absolute
        elif lc.relseg == rc.relseg and self.op == TokenID.MINUS:
            cloner = Constant(0)  # initial value is irrelevant
        elif type(lc) == type(rc) and self._compatsegs(lc, rc):
            # this is almost always something silly, like foo = r1 + r2
            # but 'as' allows it so ...
            cloner = rc
        else:
            raise _SymbolTypeMismatch("incompatible types in expression")

        # compute the new value
        altvalue = self.BINOPS[self.op](lc.value, rc.value) & 0xFFFF

        # and return it as a new object of the appropriate type
        # NOTE: "resolve" this too, in case the newly-build object
        #       has resolve semantics (some do, e.g., Register)
        return cloner.clone(altvalue).resolve()


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):
        def test1(self):
            c = Constant(18)
            self.assertEqual(c.resolve().value, 18)

        def test2(self):
            c = Constant(18)
            c2 = c.clone(3)
            self.assertEqual(c2.resolve().value, 3)

        def test3(self):
            seg = object()
            c = Constant(42, relseg=seg)
            c2 = c.clone(17)
            self.assertEqual(c2.resolve().value, 17)
            self.assertEqual(c2.resolve().relseg, seg)

        def test_plus_absabs(self):
            left = Constant(42)
            right = Constant(17)
            x = BinaryExpression(left, TokenID.PLUS, right)
            r = x.resolve()
            self.assertEqual(r.value, 42 + 17)

        def test_plus_abstext(self):
            text = object()        # just faking  a text segment

            # test it in both orders (abs+text, text+abs)
            for left, right in ((Constant(42), Constant(17, relseg=text)),
                                (Constant(42, relseg=text), Constant(17))):
                x = BinaryExpression(left, TokenID.PLUS, right)
                r = x.resolve()
                self.assertEqual(r.value, 42 + 17)
                self.assertEqual(r.relseg, text)

        def test_minus_twosegs(self):
            # subtracting two segment-relatives becomes absolute
            text = object()
            left = Constant(42, relseg=text)
            right = Constant(17, relseg=text)
            x = BinaryExpression(left, TokenID.MINUS, right)
            r = x.resolve()
            self.assertEqual(r.value, 42 - 17)
            self.assertTrue(r.relseg is None)

        def test_preserved_type(self):
            # adding an ABSOLUTE to another type preserves that type

            # need something that isn't a Constant and don't want to
            # go outside this module, therefore:
            class Foo(XNode):
                pass

            for left, right in ((Constant(42), Foo(17)),
                                (Foo(42), Constant(17))):
                x = BinaryExpression(left, TokenID.PLUS, right)
                r = x.resolve()
                self.assertTrue(isinstance(r, Foo))
                self.assertEqual(r.value, 42 + 17)

        def test_bang(self):
            # see the comment in BinaryExpression for the BANG lambda.
            # Although the 'as' documentation says ! is "a or (not b)"
            # it is, in fact, "a + (not b)" (verified running v7 'as').
            # This tests that.
            left = Constant(4)
            right = Constant(1)
            x = BinaryExpression(left, TokenID.BANG, right)
            r = x.resolve()
            self.assertEqual(r.value, (4 + (~1)))

        def test_caret(self):
            left = Constant(3)
            right = Register(7)
            x = BinaryExpression(left, TokenID.CARET, right)
            r = x.resolve()
            self.assertTrue(isinstance(r, Register))
            self.assertEqual(r.value, 3)

        def test_illegal_segs(self):
            ts = object()
            ds = object()
            left = Constant(17, relseg=ts)
            right = Constant(42, relseg=ds)
            x = BinaryExpression(left, TokenID.PLUS, right)
            with self.assertRaises(_SymbolTypeMismatch):
                _ = x.resolve()

    unittest.main()
