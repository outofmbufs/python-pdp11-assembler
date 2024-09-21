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

from expression import XNode, Constant
from astokens import TokenID, STMT_ENDS


class PseudoOp(XNode):
    # most pseudo-ops do not have a value
    def __init__(self, value=XNode.NOVALUE, **kwargs):
        super().__init__(value, **kwargs)

    # 'as' resolves all pseudo-ops as zero if used in expressions
    def resolve(self):
        return Constant(0)


class PSSegment(PseudoOp):
    def __init__(self, segname, **kwargs):
        super().__init__(**kwargs)
        self.value = segname

    def parse(self, az):
        az.curseg = az.segments[self.value]
        return None


class PSByte(PseudoOp):

    def parse(self, az):
        vals = list()
        while True:
            x = az.parseexpr()
            if x is None:
                break
            vals.append(x.resolve().value & 0xFF)
            if not az._tk.peekif_IDmatches(TokenID.COMMA):
                break
            az._tk.gettok()        # eat the comma, do the next byte

        if len(vals) == 0:
            az.synerr("missing .bytes expression")
        else:
            return BytesBlob(bytes(vals))


class EvenBlob(PseudoOp):

    # should never get here because parse() turns .even into BytesBlob
    @property
    def nbytes(self):
        raise TypeError("EvenBlob nbytes botch")

    def parse(self, az):
        return BytesBlob(bytes(az.curseg.dot() & 1))


class PSIf(PseudoOp):

    def parse(self, az):
        """Process a .if directive."""

        # get the expression
        if (x := az.parseexpr(errmsg="missing expression")) is None:
            return None

        # must not be any other gunk left in this statement
        if not az._tk.peekif_IDmatches(STMT_ENDS):
            az.synerr_unexpected()

        # if the expression evaluates to true, .if is a no-op
        if x.resolve().value:
            return None

        # The expression evaluated false, so eat tokens wildly until
        # a corresponding .endif ... note that have to nest .if tokens
        # encountered along the way (if they are at a statement start)
        #
        # the unix 'as' implementation recognizes a .endif ANYWHERE.
        # For example:
        #          bozo=0
        #          .if bozo
        #            mov r0,.endif
        #          clr r1
        #          .endif
        #          clr r2
        #
        # puts both clr r1 and clr r2 into the output, even though
        # the .endif appears in an implausible spot. Note also that
        # extra .endif statements are benign/ignored.
        #
        # However, .if statements seem to only be recognized at the
        # start of a statement.
        #
        # Tokenization of false .if branches still seems to happen as
        # completely illegal lexical constructions still cause errors.
        # And, of course, a .endif in a string literal is ignored.
        # This implementation attempts to match all the subtle semantics
        # of the unix v7 'as' in these regards.

        endifs_needed = 1
        at_stmt_boundary = True
        while endifs_needed > 0 and not az._tk.at_eof():
            t = az._tk.gettok()
            if t.id == TokenID.IDENTIFIER and t.value in az.symtab:
                node = az.symtab[t.value]
                if isinstance(node.value, PSEndif):
                    endifs_needed -= 1
                elif isinstance(node.value, PSIf) and at_stmt_boundary:
                    endifs_needed += 1
            at_stmt_boundary = t.id in STMT_ENDS
        return None


class PSEndif(PseudoOp):
    # if these appear in statement position not during a .if gobble
    # they are ignored

    def parse(self, az):
        return None


class PSIgnore(PseudoOp):
    # for things like .globl that will be accepted but just ignored.
    def parse(self, az):
        while not az._tk.peekif_IDmatches(STMT_ENDS):
            az._tk.gettok()


class BytesBlob(XNode):
    """Arbitrary number of bytes, as a blob."""
    @property
    def nbytes(self):
        return len(self.value)

    def byteseq(self):
        return self.value


class _SegmentOps(PseudoOp):
    """The .org and .boundary directives"""
    nbytes = 0

    def parse(self, az):
        x = az.parseexpr()
        if x is None:
            return None
        node = self.clone(x)
        node.segment = az.curseg
        return node

    def pass2start(self, az):
        """Called during the BEGINNING of the second pass."""
        self.segment.origindirective(
            self.value.resolve().value, roundup=self.ROUNDUP)
        return True


class Org(_SegmentOps):
    ROUNDUP = False

    @classmethod
    def implicit_org(cls, val, az):
        org = az.symtab['.org']
        node = org.value.clone(val)
        node.segment = az.curseg
        return node


class Boundary(_SegmentOps):
    ROUNDUP = True
