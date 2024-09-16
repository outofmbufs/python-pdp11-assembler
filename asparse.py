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

# The heart of the assembler: parser/byte stream generator

from enum import Enum
import itertools
import functools

import asconstants
import asx
import opnodes
import pseudops

from astokens import Token, TokenID, STMT_ENDS
from tokutil import TokStreamEnhancer
from expression import XNode, Constant, Register, BinaryExpression
from segment import Segment
from symtab import SymbolTable


# An ASMParser parses a stream of tokens, usually from ASMTokenizer.tokens()
# Typical:
#      tkns = ASMTokenizer(open('foo', 'r')).tokens()
#      az = ASMParser(tkns)
#
class ASMParser:
    """PDP-11 'as' assembly parser."""

    STAGE = Enum('STAGE', ['INITIAL', 'PASS1', 'COMPLETE', 'ERROR'])

    def __init__(self, tokens):
        """Parser for a token stream, usually from ASMTokenizer.tokens()"""

        self.completedstage = self.STAGE.INITIAL

        # public attributes for reporting errors and warnings
        self.errors = []
        self.warnings = []

        self.symtab = self._buildsymtab()   # populate all the built-ins

        # standard TEXT/DATA/BSS segments w/.text as current/default
        self.segmentnames = ['.text', '.data', '.bss']
        self.segments = {k: Segment(k) for k in self.segmentnames}
        self.curseg = self.segments[self.segmentnames[0]]

        self._uniquegen = itertools.count()    # for temp labels

        self._tweens = []                      # callbacks "between" p1/p2

        # TokStreamEnhancer adds pushback, mark/accept, and allows
        # for an implied token (NEWLINE in this case) at EOF which helps
        # because Unix v7 'as' treats an EOF *anywhere* as an implied newline.
        # Subclass (defined in this file) TokensPlus adds peekif_IDmatches().
        NLtok = Token(TokenID.NEWLINE, "\n", "EOF-induced-newline")
        EOFtok = Token(TokenID.EOF, None, "EOF")
        self._tk = TokensPlus(tokens, lasttok=NLtok, eoftok=EOFtok)

    def firstpass(self):
        """Do first pass of assembler and return True if no errors.

        If there were errors, text is in .errors attribute.

        Warnings do not affect return value but will be found in .warnings
        """

        if self.completedstage != self.STAGE.INITIAL:
            self.warnings.append("firstpass method called multiple times")
            return len(self.errors) == 0

        # In the past there were bugs where some syntax errors would
        # cause no token consumption, resulting in an infinite loop here.
        # 'unchanged' protects against that just in case.
        unchanged = object()               # matches nothing
        while not self._tk.peekif(lambda t: t is unchanged, eofmatch=True):
            unchanged = self._tk.peektok()
            try:
                self.toplevelparse()
            except asx._UndefinedSymbol as e:
                if e.cyclic:
                    msg = "cyclic undefined references"
                else:
                    msg = f"undefined reference -- {e.symname}"
                self.synerr(msg)
            except asx._SymbolTypeMismatch as e:
                self.synerr(e.msg)

        if len(self.errors) > 0:
            self.completedstage = self.STAGE.ERROR
        else:
            self.completedstage = self.STAGE.PASS1
        return len(self.errors) == 0

    def secondpass(self):
        """Return a sequence of tuples containing machine code segments.
        Returns None if there are errors.

        Each segment is a tuple:
              (baseaddress, bytes-sequence)
        and there is one such tuple for each contiguous chunk of addresses.
        For example, this code:
              .text
              .org 10000
              mov r1,r2
              .data
              .org 20000
               255.
               512.
        will return:
           [(0o10000, bytes([66, 16])), (0o20000, bytes([255, 0, 0, 2]))]
        """

        # There are many places an _Alignment or others can be discovered...
        try:
            return self._secondpass()
        except asx._ASX as e:
            self.errors.append(repr(e))
            return None

    def _secondpass(self):
        if self.completedstage != self.STAGE.PASS1:
            if self.completedstage == self.STAGE.COMPLETE:
                self.errors.append("secondpass repeated")
            else:
                self.errors.append("need firstpass before secondpass")
            return None

        # make sure there are no undefined symbols remaining
        for sym in self.symtab.undefs():

            try:
                v = self.symtab[sym].resolve()
            except asx._UndefinedSymbol as e:
                # if it's a tempsymbol name, parse out the original ref.
                # NOTE: For everything else, this is a no-op
                if (origref := self.parse_templabel_name(sym)):
                    self.synerr(f"No target found for: {origref}f")
                else:
                    self.synerr_undef(sym, e.cyclic)
            else:
                raise TypeError("BOTCH: see undefs in secondpass")

        if self.errors:
            return None

        textseg = self.segments['.text']
        dataseg = self.segments['.data']
        bssseg = self.segments['.bss']

        # '..' handling. Here's what the 'as' docs say:
        #    Thus the value of ‘‘..’’ can be taken to mean the starting
        #    core location of the program. In UNIX systems with relocation
        #    hardware, the initial value of ‘‘..’’ is 0. The value of ‘‘..’’
        #    may be changed by assignment.
        #
        # Grepping the entire unix v7 source base, it appears the only
        # place this is used is in cmd/standalone/mtboot.s to set the
        # program to run in high memory. This implementation of dotdot
        # matches the *necessary* 'as' semantics but does not match the
        # full-implementation detail semantics.
        dotdot = self.symtab['..'].value.resolve().value
        if dotdot != 0:
            textseg.origindirective(dotdot)

        self.squishbranches(textseg)
        # believe it or not, sometimes code is in the dataseg...
        self.squishbranches(dataseg)

        # The SegmentOps get called now, sort of "between" passes
        # (more like "during" the second, but so be it)
        for tween in self._tweens:
            if not tween():
                self.errors.append(f"tweener problem")

        # now that the segment sizes and any .org/.boundary are known,
        # combine the segments with appropriate offsets.

        try:
            textseg.setoffset(0)           # could be higher if .org used
            dataseg.setoffset(textseg.offset + textseg.dot())
            bssseg.setoffset(dataseg.offset + dataseg.dot())
        except ValueError:
            self.errors.append(f"Conflicting segment sizes/directives")
            return None

        # last, but not least: generate the bytes for each segment!
        rslt = []
        for seg in (textseg, dataseg, bssseg):
            rslt.append((seg.offset, seg.byteseq()))

        self.completedstage = self.STAGE.COMPLETE
        return rslt

    def squishbranches(self, seg):
        # Pseudo-branches (jbr, jne, etc) are initially assumed to
        # be in their long-form: 6 bytes for jne etc, 4 for jbr.
        # Find every pseudo-branch and see if it can be reduced to
        # a regular branch. Make repeated passes through the segment
        # until no more squishing happens. Was there a better way?
        #
        # Note that this algorithm can miss optimizations in edge cases.
        # For example, consider this sequence:
        #   start:
        #      jbr X000
        #      jbr X001
        #      ... enough to make X000 (below) be far enough away for
        #          none of the jbr's to be squishable to plain br's
        #   X000: jbr start
        #   X001: jbr start
        #   X002: ... etc
        #
        # In this sequence, until some of the jbr's are squished, none of them
        # are squishable, but (depending on the exact number in the sequence)
        # potentially ALL of them are squishable since they keep getting closer
        # to their destinations as more get squished.
        #
        # "In real life" those pathological cases don't happen; in the off
        # chance that they do, the assembler still "works" it just doesn't
        # guarantee optimal squishings of j/br branches.
        #
        optimized_any = True         # prime the loop
        while optimized_any:
            optimized_any = False
            for xn in seg:
                if isinstance(xn, opnodes.JBranch):
                    targval = xn.target.resolve().value
                    here = seg.dot(after=xn)
                    dist = targval - here
                    if dist >= -256 and dist <= 254:
                        br = opnodes.Branch(
                            xn.opcode, target=xn.target, relseg=xn.relseg)
                        seg.replacenode(xn, br)
                        optimized_any = True

    def toplevelparse(self):
        """Called at a statement boundary. Parse one statement."""

        # skip blank lines or statements (extra ';' are possible)
        while self._tk.peekif_IDmatches(STMT_ENDS):
            self._tk.gettok()

        if self._tk.at_eof():
            return

        t0, t1 = self._tk.peektoks(2)

        # If t1 is a COLON, accept a label definition.
        # Act as if there is an implicit STMT_END here so that
        #   lab1: foo
        # is parsed as if it were
        #   lab1:
        #         foo
        #
        # which makes this construction also work:
        #
        #   lab1: lab2: lab3: foo
        #
        # or
        #   lab1:
        #   lab2:
        #   lab3: foo
        #
        if t1.id == TokenID.COLON:
            self.labelcolon(t0)
            return

        # There are four possibilities
        #      1) something = expr
        #      2) <string>
        #      3) anything lacking a .parse() that parseexpr() accepts
        #      4) something with a .parse()
        #

        if t1.id == TokenID.EQ and t0.id == TokenID.IDENTIFIER:
            self._tk.gettoks(2)     # eat up the identifier and '='
            self.assign(t0)
            node = None
        elif t0.id == TokenID.STRING:       # <string>
            node = StringData(t0.value, tok=t0)
            self._tk.gettok()
        elif not self.hasparsemethod(t0.value):
            node = self.parseexpr()
        else:
            self._tk.gettok()
            sym = self.symtab[t0.value]
            node = sym.value.parse(self)
            if node is not None:
                node.tok = t0          # this is recorded for debugging only

        # there shouldn't be anything left
        if not self.end_of_statement():
            node = None
        if node is not None:
            self.curseg.addnode(node)

    def hasparsemethod(self, s):
        """Return True if s is in symtab and has a .parse method"""
        try:
            return hasattr(self.symtab[s].value, 'parse')
        except KeyError:
            return False

    def tmpsymconvert(self, digit):
        return f':{digit}:'

    def labelcolon(self, t0):
        # before being called, this parse sequence was verified already
        t0, colon = self._tk.gettoks(2)
        if colon.id != TokenID.COLON:
            raise ValueError("internal parser error, colon not found here")

        match t0.id:
            case TokenID.IDENTIFIER:
                if self.symtab.sym_defined(t0.value):
                    self.synerr(f"'{t0.value}' redefined")
                    return
                lbldef = LabelDef(t0.value, relseg=self.curseg, tok=t0)
            case TokenID.CONSTANT if t0.value in range(10):
                lbldef = self.templabel(self.tmpsymconvert(t0.value), tok=t0)
            case _:
                self.synerr(f"invalid label: '{t0.label}'")
                return

        self.curseg.addnode(lbldef)
        self.symtab.add_symbol(lbldef.name,
                               LabelRef(lbldef, tok=t0, relseg=self.curseg))

    # In unix v7 as, forward references only are allowed one level forward.
    # This code is illegal:
    #     foo = bar
    #     bar = baz
    #     baz = 1
    #
    # This code actually has to work harder (lol) to make that restriction.
    # Variable STRICTFWD controls that. It defaults to True to match unix v7.

    STRICTFWD = True

    def assign(self, dst):
        # dst.value is actually the target variable name;
        # this variable is to make code less confusing about that.
        name = dst.value

        if (x := self.parseexpr(errmsg="missing expression")) is None:
            return

        if name == '.':
            try:
                # the expression must not contain forward references
                # and when evaluated must be greater than the current dot()
                # The current segment will be padded with zeros accordingly.
                val = x.resolve().value
                dot = self.curseg.dot()
                if val < dot:
                    raise ValueError(". cannot go backwards")
                if val > dot:
                    self.curseg.addnode(
                        pseudops.BytesBlob(bytes([0] * (val - dot))))
            except ValueError as e:
                self.synerr(str(e))
            else:
                return

        # By default, the 'as' semantic is that only one level of forward
        # referencing is allowed. This enforces that unless STRICTFWD
        # has been turned off.
        if self.STRICTFWD:
            try:
                x = x.resolve()
            except asx._UndefinedSymbol:
                # dst is not currently resolvable.
                # Look to see if it is in the symbol table already as
                # an UNDEFINED (expr is None) value; if so, it's a double
                # forward reference and is not allowed.
                try:
                    prior_undef = self.symtab[name].value is None
                except KeyError:
                    prior_undef = False
                if prior_undef:
                    self.synerr("multiple forward reference")
                    return
        self.symtab.add_symbol(name, x)

    # Temporary (meta) labels.
    #
    # Temporary labels are used for things like:
    #     0: mov (r0)+,(r1)+
    #        bne 0b
    #
    # in which case the metaname will be an int (not string) digit 0 .. 9
    # This same mechanism is also used for implementing '.' which, in effect
    # is just a variation of a temporary label.
    #
    # Whatever the metaname happens to be, it is always something that does
    # not lex as a user label, and so is a safe token to use for naming
    # TempMetaLabel objects. Each TempMetaLabel object has two names in it:
    #       name  -- the LabelDef created for this TempMetaLabel when it
    #                was seen. Backward ('b') refs to this temporary label
    #                become references to this.
    #   nextname  -- The next 'name' this TempMetaLabel will have; used
    #                for forward ('f') references.
    #
    # The name and nextname are kept unique from all user symbols by
    # extending the observation that numeric values cannot appear as symbols
    # in user code; they are created as 100 + a unique (counting) value.

    def templabel(self, metaname, *, tok=None):
        # if this is the first time this one is being seen, start it off
        try:
            nextname = self.symtab[metaname].value.nextname
        except KeyError:
            nextname = self.make_templabel_name(metaname)

        seg = self.curseg
        newtml = TempMetaLabel(
            nextname, self.make_templabel_name(metaname), relseg=seg, tok=tok)
        self.symtab.add_symbol(metaname, newtml)
        return LabelDef(newtml.name, relseg=newtml.relseg, tok=tok)

    def make_templabel_name(self, refinfo):
        # NOTE: see parse_templabel_name, which is used for producing
        #       better human-readable error messages. It knows this format.
        return f':{100 + next(self._uniquegen)}/{refinfo}'

    def parse_templabel_name(self, name):
        # Given a name expected to be from make_templabel_name, recover
        # the "refinfo" that was encoded into it. Used SOLELY for producing
        # better human-readable error messages for thing like "br 1f"
        # when there is no following "1:" found anywhere. Basically this
        # recovers the original symbol causing the templabel.

        # If this symbol was made for a reference such as "3f", it
        # will look something like ":101/:3:" ... recover the "3"
        # as the second symbol in the tuple returned
        syms = name.split('/')
        if len(syms) < 2:
            return None
        elif syms[1][0] == ':' and syms[1][-1] == ':':
            # the expected case
            return syms[1][1:-1]
        return None

    # References to dot leverage the temporary label mechanism. For example:
    #   mov .,r0
    # is implemented as if written:
    #    9: mov 9b,r0
    # except that instead of using a numeric label name ('9') an internal
    # object is used as the 'name' (from the symbol table perspective)
    # to avoid any potential user-symbol clash.
    _DOTLABEL = ':.:'

    def dot_ref(self):
        # just for debugging purposes it's nice to know where this came from
        tok = self._tk.peektok()
        last = self.curseg.last()     # re-use a label if there is already
        if not isinstance(last, LabelDef):
            self.curseg.addnode(self.templabel(self._DOTLABEL, tok=tok))
            last = self.curseg.last()
            self.symtab.add_symbol(last.name,
                                   LabelRef(last, tok=tok, relseg=self.curseg))
        return self.symtab.ref_symbol(last.name)

    # all the operators that can appear in an expr
    EXPROPERATORS = (
        TokenID.PLUS,
        TokenID.MINUS,
        TokenID.STAR,
        TokenID.VSLASHES,
        TokenID.AMPERSAND,
        TokenID.BAR,
        TokenID.PERCENT,
        TokenID.BANG,
        TokenID.CARET,
        # See: https://github.com/outofmbufs/python-pdp11-assembler/issues/2
        # These are not implemented in unix v7 as because: reasons.
        # They are not implemented here for the same reasons.
        # TokenID.RR,
        # TokenID.LL,
    )

    # Per the 'as' manual:
    #   * An expression is a sequence of symbols representing a value.
    #   * Its constituents are identifiers, constants, temporary symbols,
    #     operators, and brackets.
    #   * All operators are fundamentally binary in nature; if an operand
    #     is missing on the left a 0 of absolute type is assumed.
    #   * All operators have EQUAL PRECEDENCE and expressions are evaluated
    #     strictly left to right except for the effect of brackets.
    #   * It follows from all this that this:
    #         1 + + + 2
    #     "works" and means 1 + 0 + 0 + 2 ... this behavior has been
    #     verified in real v7 unix; send complaints to /dev/null.
    #   * ADJACENCY: exprs separated only by whitespace imply addition:
    #        1 2 4
    #     has the value 7. Note this is fully-general, so
    #        2 [ 3 * 4 ]
    #     "works" and means 2 + [ 3 * 4 ]. Complaints to v7 'as' authors.
    #
    # Note that the ! ("not") operator is defined as binary:
    #    a ! b is "a + (not b)"  (the docs say "or" but 'as' code says '+')
    #
    # On top of all that, there is a special case for the '^' operator.
    # This is implemented a lot more gracefully in 'as' (because of how
    # 'as' integrates parser dispatch into the symbol table) but here...
    # well, if there is an expression containing a '^' operator it has to
    # be handled specially, to turn it back into a (constructed) instruction
    #

    def parseexpr(self, *, errmsg=None):
        """Recursive descent parser for unix v7 'as' expressions."""
        # it improves error processing if failure to parse an expression
        # leaves the token stream at the point where the expression starts.
        with self._tk.tokmark() as ctx:
            x = self._parseexpr()
            if x is None:
                if errmsg is not None:
                    self.synerr(errmsg)
                return None
            ctx.acceptmarks()           # got something - do not unwind it
        return x

    def _x_operand(self):
        """Return the next operand, which might be a [bracketed] expr."""
        t0 = self._tk.peektok()
        node = None
        match t0.id:
            case TokenID.LBRA:
                self._tk.gettok()          # eat the '['
                node = self._parseexpr()
                if node is None:
                    return None
                if not self._tk.peekif_IDmatches(TokenID.RBRA):
                    self.synerr("missing ]")
                    return None
                self._tk.gettok()             # eat the ']'
            case TokenID.CONSTANT:
                node = Constant(t0.value, tok=t0)
                self._tk.gettok()
            case TokenID.IDENTIFIER if t0.value == '.':
                node = self.dot_ref()
                self._tk.gettok()
            case TokenID.IDENTIFIER:
                # The semantic is that if there is already a definition of
                # this symbol, even if incomplete, it gets "preserved" in
                # this reference (so even if overwritten later this
                # reference uses the now-current expr).
                try:
                    node = self.symtab[t0.value].value
                except KeyError:
                    node = None
                if node is None:
                    node = self.symtab.ref_symbol(t0.value)
                self._tk.gettok()
            case TokenID.TEMPLABREF:
                fb = t0.value[-1]
                digit = int(t0.value[:-1])
                name = self.tmpsymconvert(digit)
                if name not in self.symtab:
                    # For better human error messages, nip undefined
                    # backwards refs in the bud right here (e.g., a "br 1b"
                    # if there hasn't been a "1:" yet). Bad forward refs are
                    # human-error-decoded elsewhere.
                    if fb == 'b':
                        self.synerr(f"'{t0.value}' without preceding target")
                    # this is, e.g., a 0f before any 0 has been seen
                    self.templabel(name, tok=t0)   # set this one up
                metalabel = self.symtab[name].value
                if fb == 'f':
                    metaname = metalabel.nextname
                else:
                    metaname = metalabel.name
                node = self.symtab.ref_symbol(metaname)
                self._tk.gettok()

        return node

    def _parseexpr(self):
        expr_token = self._tk.peektok()      # this is for debugging/reporting

        # this would be a clean/recursive descent parse but unix v7 'as'
        # is left-associative so .. this way

        left = None
        op = None
        right = None

        # if the expression starts with an operator, there needs to be
        # a dummy zero supplied on the left. E.g., !b becomes 0 ! b
        if self._tk.peekif_IDmatches(self.EXPROPERATORS):
            left = Constant(0)

        while True:
            if self._tk.peekif_IDmatches(self.EXPROPERATORS):
                if op is None:
                    op = self._tk.gettok().id    # normal case
                else:
                    # Something like a + !b ... this is messy because
                    # unary is essentially right-associative with an
                    # implied zero left. Bleh.
                    uop = self._tk.gettok().id
                    if (unarand := self._x_operand()) is None:
                        unarand = Constant(0)
                    right = BinaryExpression(Constant(0), uop, unarand)
            elif (right := self._x_operand()) is not None:
                if op is None:
                    op = TokenID.PLUS
            else:
                break
            if right:
                if not left:
                    left = right
                else:
                    left = BinaryExpression(left, op, right, tok=expr_token)
                op = right = None

        if op is not None or left is None:
            return None

        return left

    def end_of_statement(self):
        """Return True if at statement end; record syntax error if not."""
        if self._tk.peekif_IDmatches(STMT_ENDS):
            return True
        self.synerr_unexpected()
        # pick up parsing at next boundary
        while not self._tk.peekif_IDmatches(STMT_ENDS, eofmatch=True):
            self._tk.gettok()
        return False

    def tweener(self, cb):
        """Register a callback to be invoked during secondpass."""
        self._tweens.append(cb)

    def synerr(self, info):
        """Report a syntax error."""

        if self.completedstage == self.STAGE.PASS1:
            msg = "Parsing complete - "
        elif self._tk.at_eof():
            msg = "End of file - "
        else:
            t0 = self._tk.peektok()
            if t0.location.lineno is not None:
                if t0.location.sourcename is not None:
                    msg = f"'{t0.location.sourcename}' - "
                    msg += f"line {t0.location.lineno}: "
                else:
                    msg = f"Line {t0.location.lineno}: "
            elif t0.value is not None:
                msg = f"At {t0.value}: "
            else:
                msg = ""
        msg += info
        self.errors.append(msg)

    def synerr_unexpected(self, bogon=None):
        if bogon is None:
            bogon = self._tk.peektok()
        self.synerr(f"'{bogon.value}' unexpected")

    def synerr_undef(self, sym, cyclic):
        cycstr = " (cyclic)" if cyclic else ""
        self.errors.append(f"Undefined symbol: {sym}" + cycstr)

    def _buildsymtab(self):
        symtab = SymbolTable()

        # convenience so builtin doesn't have be specified every time
        builtin = functools.partial(symtab.add_symbol, builtin=True)

        opcodes_and_xnodes = (
            (asconstants.REGISTERS, Register),
            (asconstants.SINGLEOPERANDS, opnodes.OneOper),
            (asconstants.DOUBLEOPERANDS, opnodes.TwoOper),
            (asconstants.D63OPERANDS, opnodes.TwoOper63),
            (asconstants.D36OPERANDS, opnodes.TwoOper36),
            (asconstants.ZEROOPERANDS, Constant),  # Constant suffices!
            (asconstants.BRANCH_CODES, opnodes.Branch),
        )

        for codes, xn in opcodes_and_xnodes:
            for name, opcode in codes.items():
                builtin(name, xn(opcode))

        # 'sys' is really a TRAP instruction. Adjacency-addition for trap #
        builtin('sys', Constant(0o104400))

        # SOB and RTS are their each their own special thing
        builtin('sob', opnodes.SOB(0o077000))
        builtin('rts', opnodes.RTS(0o000200))

        # the pseudo-branches. For each pseudo-branch there is:
        #     * an "optimized" opcode -- the br-equivalent form
        #     * a "negated" opcode -- used for the "br around a jmp" form

        # this maps jbr names to the negated br-equivalent
        jb_negated = {
            'jbr': None,         # NOTE: this is a special case
            'jne': 'beq',
            'jeq': 'bne',
            'jpl': 'bmi',
            'jmi': 'bpl',
            'jvc': 'bvs',
            'jvs': 'bvc',
            'jcc': 'bcs',
            'jhis': 'bcs',
            'jcs': 'bcc',
            'jes': 'bcc',
            'jlo': 'bcc',
            'jge': 'blt',
            'jgt': 'ble',
            'jle': 'bgt',
            'jhi': 'blos',
            'jlos': 'bhi'
        }

        for jb, bneg in jb_negated.items():
            # figure out the underlying branch name equivalent, get opcode
            if jb == 'jbr':
                bopcode = asconstants.BRANCH_CODES['br']
            else:
                bopcode = asconstants.BRANCH_CODES['b' + jb[1:]]
            builtin(jb, opnodes.JBranch(bopcode, bneg))

        # Pseudo operations
        for segname in ('.text', '.data', '.bss'):
            builtin(segname, pseudops.PSSegment(segname))
        builtin('.byte', pseudops.PSByte())
        builtin('.even', pseudops.EvenBlob())
        builtin('.if', pseudops.PSIf())
        builtin('.endif', pseudops.PSEndif())

        # these two are not in 'as' but are helpful standalone without 'ld'
        builtin('.org', pseudops.SegmentOps(0))
        builtin('.boundary', pseudops.SegmentOps(1))

        # See secondpass; '..' is essentially a .org in .text
        # It starts at zero. It can be assigned. In unix v7 the only
        # known place this is done is in the machine-dependent boot code.
        builtin('..', Constant(0))
        return symtab


class StringData(XNode):
    """Arbitrary <strings> put into a segment."""
    @property
    def nbytes(self):
        return len(self.byteseq())

    def byteseq(self):
        return bytes(self.value, encoding="utf-8")


class LabelDef(XNode):
    nbytes = 0

    """Placed into a segment when there is a label: seen."""
    def __init__(self, s, /, *, relseg, tok=None):    # note segment REQUIRED
        super().__init__(s, relseg=relseg, tok=tok)

    # notational clarity
    @property
    def name(self):
        return self.value

    def resolve(self):
        return Constant(
            self.relseg.dot(after=self), relseg=self.relseg)


# A temporary (meta) label has a current name (just like a LabelDef), which
# is the automatically-generated name defined for its backward ('b')
# references, and a 'next' name which is defined for its forward references.

class TempMetaLabel(LabelDef):
    def __init__(self, s, s2, /, *, relseg, tok=None):   # relseg REQUIRED
        super().__init__(s, relseg=relseg, tok=tok)
        self.nextname = s2

    # these aren't normally resolved but need to be resolvable
    # just so that secondpass won't call them undefined
    def resolve(self):
        return self


class LabelRef(XNode):

    """A *reference* to a label definition (i.e., use of the label)."""
    def __init__(self, ldef, /, *, tok=None, **kwargs):
        super().__init__(ldef, tok=tok, **kwargs)
        self.ldef = ldef

    def resolve(self):
        return self.ldef.resolve()


class TokensPlus(TokStreamEnhancer):
    _NOTGIVEN = object()

    # peeking at a token just to check its ID happens enough that
    # this is a nice notational convenience.
    def peekif_IDmatches(self, tkid, /, *, eofmatch=_NOTGIVEN):
        """Return (peek) a token, or None.
           The token is returned if its tokenID is in tkid, or == tkid
           If eofmatch is given, it is returned if at_eof()
        """
        if eofmatch is not self._NOTGIVEN and self.at_eof():
            return eofmatch
        t = self.peektok()
        try:
            return t if t.id in tkid else None
        except TypeError:
            return t if t.id == tkid else None
