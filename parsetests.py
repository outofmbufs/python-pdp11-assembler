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

import unittest
import itertools
import io
import asconstants

import asparse as AP

from astokens import ASMTokenizer, Token, TokenID
from expression import BinaryExpression, XNode, Constant
from opnodes import Operand, OneOper
from asx import _UndefinedSymbol


class TestMethods(unittest.TestCase):

    # just so don't have to fill in the extra args
    def _TK(self, tokenID, val=None):
        return Token(tokenID, val, location=(0, 0, 0))

    # convenience to check firstpass automatically
    def firstpass_and_check(self, az):
        self.assertTrue(az.firstpass())

    # when byte sequences miscompare, interpreting the printed
    # output is a pain, to say the least. ENTIRELY FOR HUMAN READABILITY
    # reasons, this takes two byte sequences, turns them into a
    # PDP-11 style space-separate string of 6-octal-digit instruction words
    # and compares THAT rather than the raw byte sequences.
    def _words_compare(self, b1, b2):
        def bs2wstr(b):
            wstr = ""
            while len(b) > 1:
                v = (b[1] * 256) + b[0]
                wstr += f"{oct(v)[2:]:0>6}"
                wstr += " "
                b = b[2:]
            if len(b) > 0:
                wstr += f"xxx{oct(b[0])[2:]:0>3}"
            return wstr
        ws1 = bs2wstr(b1)
        ws2 = bs2wstr(b2)
        self.assertEqual(ws1, ws2)

    # full-assembly and check the results structure
    def full_asm_check(self, s, rslts):
        with self.subTest(s=s):
            az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
            self.firstpass_and_check(az)
            chunks = az.secondpass()
            # there must be at least as many chunks as in rslts
            # rslts can be shorter as some tests only look at .text
            self.assertTrue(len(rslts) <= len(chunks))
            for t0, t1 in zip(rslts, chunks):
                self.assertEqual(t0, t1)

    # convenience for fully-assembling a string and checking byte sequence
    # ignoring segments/offsets.
    # NOTE: expected_words is 16-bit values which will be interpreted
    #       (regardless of native machine architecture) as big-endian.
    def simple_asm_check(self, s, expected_words):
        with self.subTest(s=s):
            az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
            self.firstpass_and_check(az)
            chunks = az.secondpass()
            if not chunks and az.errors:
                print(f"\nsimple_asm_check({s}):\n{az.errors}")
                self.assertFalse("Got errors in second pass")
            bseq = bytes(itertools.chain.from_iterable(x[1] for x in chunks))
            self._words_compare(bseq, XNode._w2b(expected_words))

    def test_undefsym(self):
        s = "mov $foo,r0\n"
        az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
        self.firstpass_and_check(az)
        rslt = az.secondpass()
        self.assertEqual(rslt, None)
        self.assertTrue(len(az.errors) == 1)

    def test_undefsym2(self):
        # in STRICTFWD mode this is illegal (also illegal in 'as')
        s = "a = b + b\nb = c\nmov $b,r1\nc = 1\n"
        az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
        self.assertFalse(az.firstpass())

        # but it becomes legal if STRICTFWD is turned off
        az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
        az.STRICTFWD = False
        self.assertTrue(az.firstpass())

    def test_expr_add(self):
        tokens = (self._TK(TokenID.CONSTANT, 2),
                  self._TK(TokenID.PLUS),
                  self._TK(TokenID.CONSTANT, 3))
        az = AP.ASMParser(iter(tokens))
        rslt = az.parseexpr()
        c = rslt.resolve()
        self.assertTrue(isinstance(c, Constant))
        self.assertEqual(c.value, 5)

    def test_expr_brackets(self):
        tokens = (self._TK(TokenID.CONSTANT, 2),
                  self._TK(TokenID.STAR),
                  self._TK(TokenID.LBRA),
                  self._TK(TokenID.CONSTANT, 8),
                  self._TK(TokenID.PLUS),
                  self._TK(TokenID.CONSTANT, 0o1000),
                  self._TK(TokenID.PLUS),
                  self._TK(TokenID.CONSTANT, 1),
                  self._TK(TokenID.RBRA),
                  self._TK(TokenID.STAR),
                  self._TK(TokenID.LBRA),
                  self._TK(TokenID.CONSTANT, 100),
                  self._TK(TokenID.MINUS),
                  self._TK(TokenID.CONSTANT, 98),
                  self._TK(TokenID.RBRA))
        az = AP.ASMParser(iter(tokens))
        rslt = az.parseexpr()
        self.assertTrue(isinstance(rslt, BinaryExpression))
        self.assertEqual(rslt.resolve().value, 0o4044)

    # For reasons surpassing understanding, adjacent exprs
    # imply addition (this is document in the unix v7 'as' manual
    # and confirmed by experiement with running system). So ... test
    def test_expr_adj(self):
        testvecs = (
            # result, (tokens)
            (7, (self._TK(TokenID.CONSTANT, 1),
                 self._TK(TokenID.CONSTANT, 2),
                 self._TK(TokenID.CONSTANT, 4))),
            (14, (self._TK(TokenID.CONSTANT, 2),
                  self._TK(TokenID.LBRA),
                  self._TK(TokenID.CONSTANT, 3),
                  self._TK(TokenID.STAR),
                  self._TK(TokenID.CONSTANT, 4),
                  self._TK(TokenID.RBRA))))
        for ans, toks in testvecs:
            with self.subTest(ans=ans, toks=toks):
                az = AP.ASMParser(iter(toks))
                rslt = az.parseexpr()
                self.assertEqual(rslt.resolve().value, ans)

    def test_operandmodes(self):
        # expected operand_mode, tokens,
        testvecs = (
            # r3: mode 0, register 3
            (0o03, (self._TK(TokenID.IDENTIFIER, 'r3'),)),
            # (r3)
            (0o13, (self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),
            # *r3
            (0o13, (self._TK(TokenID.STAR),
                    self._TK(TokenID.IDENTIFIER, 'r3'))),
            # (r3)+
            (0o23, (self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN),
                    self._TK(TokenID.PLUS))),
            # *(r3)+
            (0o33, (self._TK(TokenID.STAR),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN),
                    self._TK(TokenID.PLUS))),
            # -(r3)
            (0o43, (self._TK(TokenID.MINUS),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),

            # *-(r3)
            (0o53, (self._TK(TokenID.STAR),
                    self._TK(TokenID.MINUS),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),

            # 14.(r3)
            (0o63, (self._TK(TokenID.CONSTANT, 14),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),

            # *14.(r3)
            (0o73, (self._TK(TokenID.STAR),
                    self._TK(TokenID.CONSTANT, 14),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),
            # *(r3)
            (0o73, (self._TK(TokenID.STAR),
                    self._TK(TokenID.LPAREN),
                    self._TK(TokenID.IDENTIFIER, 'r3'),
                    self._TK(TokenID.RPAREN))),
            # 177776
            (0o67, (self._TK(TokenID.CONSTANT, 0o177776),)),

            # $17
            (0o27, (self._TK(TokenID.DOLLAR),
                    self._TK(TokenID.CONSTANT, 0o177776))),

            # *$177776
            (0o37, (self._TK(TokenID.STAR),
                    self._TK(TokenID.DOLLAR),
                    self._TK(TokenID.CONSTANT, 0o177776))),
            # *177776
            (0o77, (self._TK(TokenID.STAR),
                    self._TK(TokenID.CONSTANT, 0o177776))),
            )
        for mode, toks in testvecs:
            with self.subTest(mode=mode, toks=toks):
                az = AP.ASMParser(iter(toks))
                rslt = Operand._parse1op(az, 6, False)
                self.assertTrue(isinstance(rslt, Operand))
                self.assertEqual(rslt.mode, mode)

    def test_string_operandmodes(self):
        # Same as test_operandmode but using string input.
        # A lot easier to express the data this way, but then
        # this test is also implicitly testing the tokenizer.
        testvecs = (
            (0o03, "r3"),
            (0o13, "(r3)"),
            (0o13, "*r3"),
            (0o23, "(r3)+"),
            (0o33, "*(r3)+"),
            (0o43, "-(r3)"),
            (0o53, "*-(r3)"),
            (0o63, "14.(r3)"),
            (0o73, "*14.(r3)"),
            (0o73, "*(r3)"),
            (0o67, "177776"),
            (0o27, "$17"),
            (0o37, "*$177776"),
            (0o77, "*177776"),
        )
        for mode, s in testvecs:
            with self.subTest(mode=mode, s=s):
                az = AP.ASMParser(ASMTokenizer([s]).tokens())
                rslt = Operand._parse1op(az, 6, False)
                self.assertTrue(isinstance(rslt, Operand))
                self.assertEqual(rslt.mode, mode)

    # the semantics of forward references in unix v7 'as' are bizarre.
    # Loosely speaking: when a symbol is assigned, if its value can be
    # computed at that point in the token stream, then the value is
    # assigned as a constant. However, if there are not-yet-resolved
    # forward references, the value is saved as an expression tree to
    # be evaluated later, which means the references stay "live" rather
    # than being resolved to a particular value (until later).
    #
    # Thus, for example, in this sequence:
    #    foo=bar
    #    bar=1
    #    bar=2
    # foo becomes "an expression referencing bar in the symbol table"
    # And, because bar ultimately ends up as "2", the value of foo
    # will be 2.
    #
    # But in this sequence:
    #    bar=1
    #    foo=bar
    #    bar=2
    # foo becomes the constant 1; changing bar later does not affect it.
    #
    # These two tests verify those semantics (which in turn have been
    # verifed against actual 'as' running in a pdp11 emulator)
    #
    # in PRACTICAL REALITY, one level of forward reference is fairly
    # common, but no real code is likely to play these redefinition
    # games. It's not even documented in the 'as' manual ... it's just
    # how things actually work in 'as'.
    #
    def test_redef_semantics(self):
        # NOTE: instructions used in testvecs are KNOWN to be CLR
        OPCODE = asconstants.SINGLEOPERANDS['clr']

        testvecs = (
            # (( byteseq values ), str)
            ((2,), "foo=bar\nbar=1\nbar=2\nclr $foo\n"),  # first ex above
            ((1,), "bar=1\nfoo=bar\nbar=2\nclr $foo\n"),  # 2nd ex above
            # really doesn't test anything new just more elaborate:
            ((0o77, 3), """
                       foo=bar
                       bar=1
                       clr $foo
                       bar=77   / this will make foo in 'clr $foo' be 77
                       foo=baz
                       baz=2
                       clr $foo / will foo be 77? 2? NO! 3 of course!
                       baz=3
                  """),
            ((3, 0o77), """
                       foo=bar
                       bar=1
                       clr $foo
                       bar=77
                       foo=bar
                       bar=2
                       clr $foo / this will be 77 (!!!!)
                       bar=3   / foo in first 'clr $foo' will be 3 (!!!)
                  """),
            # this example is illegal in 'as' .. multiple levels
            # of forward references are not allowed there (perhaps
            # that is how 'as' avoids the infinite loop problem?)
            # NOTE: See loop below which looks for /!\n
            ((1,), "/!\nfoo=bar\nbar=baz\nbaz=1\nclr $foo\n"),
            )

        for vals, s in testvecs:
            az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
            if s.startswith('/!\n'):
                az.STRICTFWD = False
            self.firstpass_and_check(az)
            textseg = az.segments['.text']
            for i, val in enumerate(vals):
                with self.subTest(val=val, s=s):
                    inst = textseg[i]
                    self.assertTrue(isinstance(inst, OneOper))
                    self.assertEqual(inst.opcode, OPCODE)
                    self.assertEqual(inst.ops[0].mode, 0o27)
                    self.assertEqual(inst.ops[0].byteseq(), XNode._w2b(val))
            # if it was done above without STRICTFWD, validate that
            # doing it with STRICTFWD=True (default) causes an error.
            if s.startswith('/!\n'):
                az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
                # no "and_check" because should fail!"
                self.assertFalse(az.firstpass())
                with self.assertRaises(_UndefinedSymbol):
                    az.segments['.text'][0].ops[0].byteseq()

    # cyclic references are not allowed; make sure detected
    def test_cyclic(self):
        testvecs = (
            (('foo', 'bar'), "foo=bar\nbar=foo\n",),
            (('foo', 'bar', 'baz'), "foo=bar\nbar=baz\nbaz=foo\n"),
            # order shouldn't matter but make sure
            (('baz', 'bar', 'foo'), "foo=bar\nbar=baz\nbaz=foo\n"),
        )

        for sfwd in (True, False):
            for symnames, s in testvecs:
                az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
                az.STRICTFWD = sfwd
                # when STRICTFWD is False, the first pass will succeed
                # and the error only becomes known when the symbols are
                # checked in the second pass. This checks the symbols
                # specifically as outlined in testvecs
                self.assertEqual(not sfwd, az.firstpass())
                pass1errors = len(az.errors)
                with self.subTest(symnames=symnames, s=s):
                    for name in symnames:
                        with self.assertRaises(_UndefinedSymbol):
                            az.symtab[name].resolve().value
                # for good measure ensure this fails
                rslt = az.secondpass()
                self.assertEqual(rslt, None)
                self.assertTrue(len(az.errors) - pass1errors > 0)

    # 'as' caret operator for defining instructions
    def test_caret(self):
        # since mfpi, etc are already defined just make something up to test
        self.simple_asm_check("foo = 107700^tst; foo -(r5)", [0o107745])

        # though it should still work to 'redefine' mfpi so try that:
        self.simple_asm_check("mfpi = 006500^tst; mfpi (r1)", [0o006511])

    def test_redefin(self):
        s = "bozo: clr r0\nbozo:clr r1\n"
        az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
        self.assertFalse(az.firstpass())

    def test_string(self):
        s = "<\0> ; .byte 77"
        self.simple_asm_check(s, [0o037400])
        s = "<A> ; .byte 1, 2, 3"
        self.simple_asm_check(s, [0o000501, 0o001402])

    def test_dbytes(self):
        s = ".byte 1, 2, 3 + 4, 5, 0377 ; .even \n"
        # Have to construct the words as big-endian for 'expected_words'
        xp = [(2 << 8) + 1, (5 << 8) + 7, 255]
        self.simple_asm_check(s, xp)

    def test_segnames(self):
        s = (".text ; foo: clr r0\n" +
             ".data ; bar: clr r1\n" +
             ".bss ; . = . + 4\n" +
             ".text ; foo\n bar\n")
        az = AP.ASMParser(ASMTokenizer(io.StringIO(s)).tokens())
        self.firstpass_and_check(az)

    def test_origin(self):
        s = ".org foo ; mov r1,r2 ; foo = 1024.*16."
        self.simple_asm_check(s, [0o010102])

        # also check it with the full segment/offset data
        self.full_asm_check(s, [(16384, bytes([66, 16]))])

        s = ".boundary foo ; mov r1,r2 ; foo = 1024.*16."
        self.simple_asm_check(s, [0o010102])
        self.full_asm_check(s, [(0, bytes([66, 16]))])

        s = "mov r1,r2 ; .data ; .boundary 8192. ; 1"
        self.simple_asm_check(s, [0o010102, 1])
        self.full_asm_check(s, [(0, bytes([66, 16])),
                                (8192, bytes([1, 0]))])

    def test_dotdot(self):
        s = ".. = 40000; mov r1,r2; bozo: mov r1,r3; mov $bozo,r4"
        self.simple_asm_check(s, [0o010102, 0o010103, 0o012704, 0o40002])

    # test basic jmp instruction arithmetic
    def test_jmp(self):
        s = "start: jmp bozo; clr r2; bozo: jmp start"
        self.simple_asm_check(s, [0o000167, 0o2, 0o005002, 0o000167, 0o177766])

    # test jmp instructions to numeric labels
    def test_jmptmp(self):
        s = "2: ; 1: jmp 2f; clr r2; 2: jmp 1b"
        self.simple_asm_check(s, [0o000167, 0o2, 0o005002, 0o000167, 0o177766])

    # test jmp instructions to other address modes
    def test_jmpother(self):
        s = "mov r2,r3; mov pc,r0; jmp -4(r0)"
        self.simple_asm_check(s, [0o010203, 0o010700, 0o000160, 0o177774])

    # this was a bug revealed by a jump table with jumps backwards
    # (the important point being: an already-defined label vs a fwd ref).
    # The bug was more general than this, but this type of construction
    # was how it was first discovered. Regression test it.
    def test_jmptab(self):
        s = "foo: clr r0; foo ; bar ; baz ;bar: clr r1 ; baz: clr r2"
        self.simple_asm_check(
            s, [0o005000, 0o000000, 0o000010, 0o000012, 0o005001, 0o005002])

    def test_silly_combinations(self):
        s = "foo = r1 + r2; mov foo,r4"
        self.simple_asm_check(s, [0o010304])
        # this is LEGAL in 'as' but is completely useless
        s = "foo = mov + mov; foo r2,r3"
        self.simple_asm_check(s, [0o020203])

    def test_code_in_data(self):
        s = "1: clr r2; 2; 6; .data; clr r3; bozo: 0; 2; clr bozo"
        self.simple_asm_check(
            s, [0o005002, 2, 6, 0o005003, 0, 2, 0o005067, 0o177770])

    def test_3seg(self):
        s = "clr foo; .data; clr bar; .bss; foo: . = .+2; bar: . = .+2"
        self.simple_asm_check(s, [0o005067, 4, 0o005067, 2, 0, 0])

    # test references to other segments
    def test_xseg(self):
        testvecs = (

            ("mov $t,r1; .data ; .org 1000; t: 77",
             [(0, bytes([193, 21, 0, 2])), (0o1000, bytes([0o77, 0]))]),
            ("mov t,r1; .data ; .org 1000; t: 77",
             [(0, bytes([193, 29, 252, 1])), (0o1000, bytes([0o77, 0]))]),
            )
        for s, r in testvecs:
            self.full_asm_check(s, r)

    # subtracting two segment-relative values makes an absolute value
    # Note that generally to catch this bug the values must be .data or .bss
    # because .text offset is generally 0
    def test_segsegabs(self):
        s = "0 ; .data ; 0 ; bozo: 17; clown: 42;  mov $[clown-bozo],r3"
        self.simple_asm_check(s, [0, 0, 0o17, 0o42, 0o012703, 2])

    def test_datasegabs(self):
        s = "clr r1; .data; foo: 0; symtab: 77; clr r2; mov $symtab,r0"
        expected = [0o005001, 0, 0o77, 0o005002, 0o012700, 4]
        self.simple_asm_check(s, expected)

    def test_assemsource(self):
        # this sequence modeled after an 'as' source excerpt.
        # There were bugs that originally didn't resolve all these
        # cross-segment references correctly; hence this regression test
        s = """
            .text; mov r2,r3; mov r4,r5
            .data; a.tmp1: <eatme!>; a.tmp2: <bozo>
            .text; fcreat: clr r1
            .data; jsr r5,fcreat; a.tmp1; movb  r0,pof
                   jsr r5,fcreat; a.tmp2; movb  r0,fbfil
            .bss; clown: .=.+2; pof: .=.+1; fbfil: .=.+1
        """
        # these results obtained from unix v7 'as' directly
        expected = [0o010203, 0o010405, 0o005001, 0o060545,
                    0o066564, 0o020545, 0o067542, 0o067572,
                    0o004567, 0o177760, 0o000006, 0o110067,
                    0o000014, 0o004567, 0o177746, 0o000014,
                    0o110067, 0o000003, 0o000000, 0o000000]
        self.simple_asm_check(s, expected)

    def test_ifendif(self):
        # for simplicity every one of these tests generates a
        # a simple mov r2,r3 instruction

        testvecs = (
            ".if 0\n clr r4\n .endif \n mov r2,r3",
            ".if 0 ; clr r4 ; .endif ; mov r2,r3",   # this shouldn't matter

            )
        for s in testvecs:
            self.simple_asm_check(s, [0o010203])

    # Multiple tests: anything with a simple string for input and
    # an expected byte (really: word) sequence as output. The advantage of
    # collecting all these together in one "test" is: less boilerplate, at
    # the cost of possible annoyance if have to debug one failed subTest

    def test_simplestrings(self):
        testvecs = (
            # string, expected-word-sequence

            # basic jbr test
            ("bozo: mov r0,r1\nbonzo: mov r1, r2\njbr bonzo\n",
             [0o010001, 0o010102, 0o000776]),

            # simple negative constant (which becomes a binary expression)
            ("-1", [0o177777]),

            # expressions involving multiple temp labels (was a bug)
            (" 9f-8f ; 8: 111 ; 9: 222",
             [0o2, 0o111, 0o222]),

            # A howler side-effect of "adjacency is addition"
            # This one is, in effect, 1 + 2 + x and assembles just
            # fine under v7 'as' (and works here too)
            (" x=4 ; 1 + 2x", [0o7]),

            # jbr but with an expression on the branch target
            ("bozo: mov r0,r1\nmov r1, r2\njbr bozo+2\n",
             [0o010001, 0o010102, 0o000776]),

            # regression test; early version of tokenizer took anything
            # that STARTS with the two chars of a register name as register.
            # XXX: this test is silly as registers are no longer even tokens.
            ("spx = 4\nspx\n", [4]),

            # use of an instruction keyword as a constant
            ("mov $mov,r0\n", [0o012700, 0o010000]),

            # use of an alias for a register
            ("foo = r2 ; clr (foo)", [0o005012]),

            # use of expression for a register
            ("clr r0 + 1 ; clr 1 + r2", [0o005001, 0o005003]),

            # this is an ill-defined construction that has different
            # results (causes an error) in 'as' than here.
            # A register expression that is "out of bounds" becomes
            # a Constant here.
            ("clr r5+5", [0o005067, 6]),

            # use of an alias for a register in a * operand
            ("foo = r2; clr *foo", [0o005012]),

            # use of an alias for a location in a * operand
            # NOTE: This one works if foo is undefined at parse time
            ("clr *foo; foo = 2", [0o005077, 0o177776]),

            # tests pc-rel adjustment of second operands
            ("mov foo,bar ; foo=1000; bar=2000", [0o016767, 0o774, 0o1772]),

            # testing the .even directive, and string literals
            ("""
                 <odd>  / string length odd
                .even
                clr r0
            """, [0o062157, 0o144, 0o005000]),

            # dot
            ("mov $.,r0\n . = . + 3; . = . + 1\nmov $.,r1\n",
             [0o012700, 0, 0, 0, 0o012701, 8]),

            # this was a silly bug at an early stage: the second reference
            # to a not-yet-defined variable would fail. Meh. Test it.
            ("mov $foo,r0 ; mov $foo,r1\n ; foo = 5\n",
             [0o012700, 5, 0o012701, 5]),
            ("bar=foo+foo; mov $bar,r0; foo=6\n", [0o012700, 12]),

            # an earlier implementation got this wrong, the second reference
            # to 'b' triggered the cyclic detection.
            ("a = b + b\nb = 3\nmov $a,r1\n", [0o012701, 6]),

            # an earlier implementation missed newlines if they were
            # preceded by WHITESPACE (!!) so test for that.
            ("mov r1,r2    \nmov r2, r3", [0o010102, 0o010203]),

            # test temporary labels. bseq obtained from pdp-11 'as'
            (""" 5:
                 1: mov r1,r2
                    br 1b
                    br 1f
                 1: mov r2,r3
                    br 1f
                    br 1b
                 1: mov r3,r4
                    br 5b
              """, [0o010102, 0o000776, 0o000400,
                    0o010203, 0o000401, 0o000775,
                    0o010304, 0o000770]),

            # basic SOB
            ("1: mov (r0)+,(r1)+ ; sob r2,1b",
             [0o012021, 0o077202]),

            # furthest possible SOB
            ("1: mov(r0)+, (r1)+ ;  . = . + 0172; sob r2, 1b",
             [0o012021] + [0]*(0o172//2) + [0o77277]),

            # sys (which is really 'trap')
            ("write = 4; sys write", [0o104404]),

            # comments claim these three statements:
            #   mov r0,r1
            #   010001
            #   010000 + 1
            # all put the same bytes into the instruction stream.
            # Test that.
            ("mov r0,r1; 010001; 010000 + 1\n",
             [0o010001, 0o010001, 0o010001])
        )
        for s, xp in testvecs:
            self.simple_asm_check(s, xp)

    # this is just a trash-can test of some various things that haven't
    # worked right at one point or another, thrown together into one test
    def test_stuff(self):
        foo = """
        <> ; <a> ; <ab>
        .even
        0: 1f
        clr r1+4
        'A ; 'A
        'A
        'B
        <\0\0XYZ>
        <!>
        1: mov r1,r2
        mov 111,222
        2: mov .+7,r3
        br 0b
        """
        self.simple_asm_check(foo, [0o060541, 0o000142, 0o000026, 0o005005,
                                    ord('A'), ord('A'), ord('A'), ord('B'),
                                    0o000000, 0o054530, 0o020532,
                                    0o010102, 0o016767, 0o000055, 0o000164,
                                    0o016703, 0o000003, 0o000760])

    # various tests of branch squishing
    def test_jbr_too_far(self):
        s = "jeq foo; . = . + 512. ; foo: mov r1,r2\n"
        result = (
            [0o1002, 0o137, 0o1006] +
            256 * [0] +
            [0o010102]
            )
        self.simple_asm_check(s, result)

    # this was a bug where branch squishing screwed up forward seg refs
    def test_squish_segs(self):
        # for example, a variable reference to data segment AFTER
        # a text segment jbranch that will be squished
        s_data = """
           .text
           jes clowns ; mov bozo,r0 ; clowns:  mov r1,r2
           .data
           bozo: 0
        """

        # same thing but bss variant
        s_bss = """
           .text
           jes clowns ; mov bozo,r0 ; clowns:  mov r1,r2
           .bss
           bozo: . = . + 2
        """
        # from running unixv7 'as' (both data/bss strings same output)
        expected = [0o103402, 0o016700, 0o000002, 0o010102, 0o000000]

        self.simple_asm_check(s_data, expected)
        self.simple_asm_check(s_bss, expected)

    def test_squish_forward1(self):
        for dist in range(128):
            s = "jeq foo\n"
            for i in range(dist):
                s += "0\n"
            s += "foo: 777"
            expected = [0o001400 + dist]
            expected += [0] * dist
            expected += [0o777]
            self.simple_asm_check(s, expected)

    def test_squish_forwardN(self):
        for dist in range(128):
            s = "jeq foo\n"
            expected = [0o001400 + dist]
            for i in range(dist):
                s += "jeq foo\n"
                expected += [0o001400 + (dist - (i+1))]
            s += "foo: 777"
            expected += [0o777]
            self.simple_asm_check(s, expected)

    def test_squish_mixedN(self):
        for dist in range(125):    # can't go as far as fwd, because reasons
            s = "start: jeq foo\n"
            expected = [0o001400 + dist]
            for i in range(dist):
                s += "jeq start\n"
                expected += [0o001776 - i]
            s += "foo: 777"
            expected += [0o777]
            self.simple_asm_check(s, expected)

    # as-specific and often-surprising semantics
    def test_as_semantics(self):
        testvecs = (
            # string, expected-byte-sequence

            # pseudo-ops are zero if used in a value context
            ("foo = .text + .bss + .byte; mov $foo,r1\n",
             [0o012701, 0]),

            ("bar=1; foo=bar; foo; bar=3", [0o000001]),

            ("bar=1; foo=abc; abc=3; abc=4; foo; foo=bar; foo",
             [0o000004, 0o000001]),

            # instructions are just symbols with parse rules so
            # new aliases for them can be created
            ("foo=mov; foo r1,r2", [0o010102]),

            # some instructions are really just constants
            # and use adjacency-addition to get their values
            ("spl=230; spl 0; spl 7; spl 1+2",
             [0o000230, 0o000237, 0o000233]),

            # goofy way unary operations handled
            ("1 + !1", [0o177777]),

            # 'as' won't parse this, though it parses the + example
            ("1 * !1", [0o177776]),

            # though 'as' WILL parse this. Go figure.
            ("!1 * 1", [0o177776]),

            # from the screed in the comments in asparse
            ("1 + + + 2; 1 2 4; 2[3*4]", [0o3, 0o7, 0o16]),
        )
        for s, xp in testvecs:
            self.simple_asm_check(s, xp)


if __name__ == "__main__":
    unittest.main()
