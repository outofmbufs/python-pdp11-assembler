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

from expression import XNode, Constant, Register
from astokens import TokenID
import asconstants
import asx


# These are instruction format XNode types that typically do not appear
# in expressions - though they *can*:
#         mov $mov,r0             / put 010000 into r0
# but are definitely found in segment streams. These appear in the
# symbol table as stub values under their mnemonics, or in segments
# as fully-populated pseudocode for a parsed instruction.
#

class _Opcode(XNode):
    """Common attributes/methods for instructions."""
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.relseg and (self.relseg.dot() & 1):
            raise asx._Alignment(node=self)

    # for notational clarity, this alias
    @property
    def opcode(self):
        return self.value


class _OperN(_Opcode):
    """Generic instruction with src,dst Operands of various formats."""

    def __init__(self, opcode, *ops, **kwargs):
        super().__init__(opcode, **kwargs)
        self.ops = ops

    def clone(self, altvalue):
        return self.__class__(altvalue, *self.ops, **self._kwargs)

    def _genparse(self, az, srcbits=6, dstbits=6):
        """Parse a fully-general one- or two-operand statement."""

        # srcbits and dstbits refer to whether the mode fields
        # are fully general (6 bit operands) or register only (3 bit):
        #       6,6      fully general two-operand (each mode 6 bits)
        #       3,6      src must be register only
        #       6,3      dst must be register only
        #       0,6      only one operand, fully 6-bit general
        #       0,3      only one operand, must be register only
        #
        # NOTE: the "flipping" of src/dst implied by configuration 6,3 is
        #       handled in TwoOper63, not here.
        #
        # If there are two operands, then this parses both, with the
        # intervening COMMA token. Else just one operand is parsed.

        src = Operand._parse1op(az, srcbits, False)
        if srcbits > 0:
            if src is None:
                return None

            t = az._tk.gettok()
            if t.id != TokenID.COMMA:
                az.synerr(f"expected COMMA after first operand")
                return None

        prior_indexword = src is not None and src.idw is not None
        dst = Operand._parse1op(az, dstbits, prior_indexword)
        if dst is None:
            return None

        if src:
            ops = [src, dst]
        else:
            ops = [dst]        # i.e., just the one operand

        # should not be anything else left in this statement
        if not az.end_of_statement():
            return None

        inst_node = self.__class__(self.opcode, *ops, relseg=az.curseg)
        for opnode in ops:
            opnode.instruction_node = inst_node

        return inst_node

    @property
    def nbytes(self):
        return 2 + sum(op.nbytes for op in self.ops)

    def byteseq(self):

        modes = 0
        opbytes = bytes()
        for op in self.ops:
            opbytes += op.byteseq()
            modes <<= 6
            modes |= op.mode
        return self._w2b(self.opcode + modes) + opbytes


class TwoOper(_OperN):
    """All the general two-operand instructions: MOV, CMP, ADD, etc."""
    def __init__(self, opcode, op1=None, op2=None, **kwargs):
        super().__init__(opcode, op1, op2, **kwargs)

    def parse(self, az):
        return self._genparse(az)


class TwoOper63(TwoOper):
    """two-operand with dst restricted to register-direct."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        # flip the operand order so underlying TwoOper code will suffice
        self.ops = [self.ops[1], self.ops[0]]

    def parse(self, az):
        return self._genparse(az, srcbits=6, dstbits=3)


class TwoOper36(TwoOper):
    """two-operand with src restricted to register-direct."""
    def parse(self, az):
        return self._genparse(az, srcbits=3, dstbits=6)


class OneOper(_OperN):
    """one-operand instructions: TST, INC, NEG, etc."""
    def __init__(self, opcode, op1=None, **kwargs):
        super().__init__(opcode, op1, **kwargs)

    def parse(self, az):
        return self._genparse(az, srcbits=0, dstbits=6)


class RTS(OneOper):
    def parse(self, az):
        return self._genparse(az, srcbits=0, dstbits=3)


class Operand(XNode):
    """A single 6-bit operand with optional additional 16-bit data."""

    def __init__(self, mode, prior_indexword, /, *,
                 idw=None, pcrel=False, relseg=None):
        super().__init__(self.NOVALUE, relseg=relseg)
        self.mode = mode
        self.idw = idw
        self.pcrel = pcrel
        self.prior_indexword = prior_indexword

        if self.pcrel and not self.idw:
            raise TypeError("pc-relative botch")

    @classmethod
    def _parse1op(cls, az, modelim, prior_indexword):
        # modelim is 0, 3, or 6 depending on what types of operand are ok
        if modelim == 0:
            return None

        star_m = 0
        dollar_present = False
        star_paren = False
        mode = None
        pcrel = False
        idw = None

        # there could be a star at the front
        if az._tk.peekif_IDmatches(TokenID.STAR):
            star_m = 0o10
            az._tk.gettok()          # eat the '*'

            # take note of *(rN) format possibility (see later)
            star_paren = az._tk.peekif_IDmatches(TokenID.LPAREN)

        # there could also be a dollar at the front
        if az._tk.peekif_IDmatches(TokenID.DOLLAR):
            dollar_present = True
            az._tk.gettok()          # eat the '$'

        # if there's an expression at this point, it's the outer expr
        outer_expr = az.parseexpr()

        # at this point the remaining additional legal forms are:
        # (rX)     ... requires "not dollar_present"
        # (rX)+    ... requires "not dollar_present and not outer_expr"
        # -(rX)    ... requires "not dollar_present and not outer_expr"

        inner_expr = None
        if az._tk.peekif_IDmatches(TokenID.MINUS):
            az._tk.gettok()             # eat the '-'
            if not az._tk.peekif_IDmatches(TokenID.LPAREN):
                az.synerr("invalid autodecrement")
                return None
            if outer_expr:
                az.synerr("cannot have autodecrement w/offset")
                return None
            mode = 0o40 | star_m

        if az._tk.peekif_IDmatches(TokenID.LPAREN):
            if dollar_present:
                az.synerr("unknown mode format")
                return None
            az._tk.gettok()             # eat the '('
            inner_expr = az.parseexpr()
            if inner_expr is None:
                az.synerr("expression expected")
                return None

            if not az._tk.peekif_IDmatches(TokenID.RPAREN):
                az.synerr("right paren missing")
                return None
            az._tk.gettok()             # eat the ')'

            rX = cls._getregval(inner_expr)
            if rX is None:
                az.synerr("register expected")
                return None

            if az._tk.peekif_IDmatches(TokenID.PLUS):
                if outer_expr:
                    az.synerr("cannot have autoincrement w/offset")
                    return None
                if mode is not None:
                    az.synerr("autoincr with autodecr")
                    return None
                mode = 0o20 | star_m
                az._tk.gettok()         # eat the '+'

            # *(inner_expr) becomes *0(inner_expr)
            if outer_expr is None:
                if not dollar_present and mode is None:
                    if star_paren:
                        idw = Constant(0)
                        mode = 0o70
                    elif not star_m:
                        mode = 0o10
            else:
                # outer_expr(inner_expr)
                mode = 0o60 | star_m
                idw = outer_expr

            mode |= rX

        # if it hasn't been figured out as auto incr/decr or *(rX) ...
        if mode is None:

            if outer_expr and not inner_expr:
                if dollar_present:
                    mode = 0o27 | star_m
                    idw = outer_expr
                else:
                    # The two ambiguous cases: naked-foo and *naked-foo
                    # If the outer_expr is already known as a register
                    # then mode 0o00/0o10 else oo67/0o77
                    rX = cls._getregval(outer_expr)
                    if rX is not None:
                        mode = 0o00 + (star_m + rX)
                    else:
                        pcrel = True
                        mode = 0o67 + star_m
                        idw = outer_expr

        if mode is None:
            az.synerr("could not parse operand")
            return None

        if modelim == 3 and ((mode & 0o70) != 0):
            az.synerr("must be register direct")
            return None

        return Operand(mode, prior_indexword,
                       idw=idw, pcrel=pcrel, relseg=az.curseg)

    @classmethod
    def _getregval(cls, x):
        try:
            resolved = x.resolve()
        except asx._UndefinedSymbol:
            return None
        if isinstance(resolved, Register):
            return resolved.value
        return None

    @property
    def nbytes(self):
        return 0 if self.idw is None else 2

    def byteseq(self):
        if self.idw is not None:
            node = self.idw.resolve()
            val = node.value
            if node.relseg and not self.pcrel:
                val += self.relseg.offset
            if node.relseg and self.relseg and (node.relseg != self.relseg):
                val += (node.relseg.offset - self.relseg.offset)
            if self.pcrel:
                # need to add 4 to get to the end of the first index word.
                # (which is guaranteed to exist because self.idw is not None)

                adj = self.relseg.dot(before=self.instruction_node) + 4

                # finally, if this is actually the second index word...
                if self.prior_indexword:
                    adj += 2
                val -= adj
            return self._w2b(val & 0xFFFF)
        else:
            return bytes()


class Branch(_Opcode):
    """True branch operations. Never a jbr/jne/etc."""

    def __init__(self, opcode, /, *, target=None, **kwargs):
        super().__init__(opcode, **kwargs)
        self.target = target

    def _distance(self):
        # Branching across segments is disallowed
        if self.target.resolve().relseg != self.relseg:
            raise asx._IllegalBranch("cross-segment branch", node=self)

        there = self.target.resolve().value
        here = self.relseg.dot(after=self)
        return there - here

    def byteseq(self):
        d = self._distance()
        if d & 1:
            raise asx._IllegalBranch(
                "branch location odd", distance=d, node=self)
        if d > 254 or d < -256 or (d & 1):
            raise asx._IllegalBranch(distance=d, node=self)
        d >>= 1
        if d < 0:
            d += 256
        return self._w2b(self.opcode | (d & 0xFF))

    def parse(self, az):
        if (x := az.parseexpr(errmsg="missing branch target")) is None:
            return None

        # branch targets cannot have type ABSOLUTE
        return Branch(self.opcode, target=x, relseg=az.curseg)


class JBranch(Branch):
    """A 'branch negative around a jmp to distant target' construction."""

    # HOW jbr OPTIMIZATION WORKS:
    #   squishbranches() replaces these nodes, when it can, with
    #   appropriate Branch nodes. Therefore, the code here has nothing
    #   to do with calculating the optimization. Everything here is
    #   what is needed for the non-optimized branch-around-a-jump case.

    def __init__(self, opcode, negopname, /, *, target=None, **kwargs):
        super().__init__(opcode, target=target, **kwargs)

        # the negated form; NOTE: None for jbr
        self.negopname = negopname

        # NOTE: Inherited from Branch:
        #     opcode  -- will be the Bxx form e.g., beq opcode for jeq
        #     target  -- a branch-compatible operand (always a pc-rel offset)
        #     relseg -- same as in Branch

    @property
    def nbytes(self):
        return 4 if self.negopname is None else 6

    def byteseq(self):

        # The pseudo-instruction jxx is used like this (for example):
        #
        #     tst r0
        #     jne notzero
        #     ...
        #
        # which would mean "test r0, and if not zero branch to 'notzero'
        # If 'notzero' is within branch-instruction range, this becomes:
        #
        #     tst r0
        #     bne notzero
        #     ...
        #
        # However, if it is out of range then it becomes:
        #
        #     tst r0
        #     beq 1f
        #     jmp notzero
        # 1:  ...
        #
        # This is called the "branch negated jump" form.
        #
        # During analysis at the very beginning of phase 2, any JBranch
        # elements that are not too far are reduced to Branch() objects.
        # Therefore, being here implies that the target is too far and
        # that the jump-around format must be generated.
        #
        # Note that 'jbr' is a special (simplified) case and just turns
        # into a naked JMP instruction at this point. Everything else turns
        # into a branch negated jump.
        d = self._distance()             # uses Branch _distance() method
        if d & 1:
            raise asx._IllegalBranch("jmp location odd", distance=d, node=self)
        if d < 0:
            d += 65536

        # The JMP instruction with an ABSOLUTE operand ... although PC-rel
        # seems a better choice, absolute is what v7 'as' does in this case.
        jmp = 0o000100 | 0o37
        locn = self.target.resolve().value
        if self.negopname is None:          # the jbr case
            return self._w2b([jmp, locn])
        else:
            # branch-negated around jmp to target.
            # NOTE: the +2 on negop is the displacement needed to
            #       get over the JMP and the operand (4 bytes total)
            negop = asconstants.BRANCH_CODES[self.negopname]
            return self._w2b([negop + 2, jmp, locn])

    def parse(self, az):
        if (x := az.parseexpr(errmsg="missing jbranch target")) is None:
            return None
        return JBranch(
            self.opcode, self.negopname, target=x, relseg=az.curseg)


class SOB(Branch):

    def parse(self, az):
        regopr = Operand._parse1op(az, 3, False)
        if regopr is None:
            return None
        reg = regopr.mode

        t = az._tk.gettok()
        if t.id != TokenID.COMMA:
            az.synerr(f"expected COMMA after SOB register operand")
            return None

        if (x := az.parseexpr(errmsg="missing sob target")) is None:
            return None
        return SOB(self.opcode + (reg << 6), target=x, relseg=az.curseg)

    def byteseq(self):
        d = self._distance()
        # SOB branches MUST be backwards and can be up to -126 away
        if d > 0 or d < -126:
            raise asx._IllegalBranch(distance=d, node=self)
        # 0 .. -126 becomes 0 .. 63
        d = (-d) >> 1
        return self._w2b(self.opcode | (d & 0o77))
