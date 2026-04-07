# This is the unittests for tokenizer.py, broken out Because Reasons.
# Among other things, this doesn't pylint cleanly and it's not worth
# making it do so (but tokenizer does)
#
# To run the tests:
#
#     python3 tkztests.py
#

import unittest
from dataclasses import dataclass
from enum import Enum

from tokenizer import Tokenizer, TokenRules, TokenMatch, TokLoc
from tokenizer import NamedRuleSet, TokenIDOnly, TokenMatchIgnore
from tokenizer import TokenMatchIgnoreButKeep, TokenMatchInt
from tokenizer import TokenMatchKeyword, TokenMatchRuleSwitch


def strictzip(a, b):
    return zip(a, b, strict=True)


class TestMethods(unittest.TestCase):

    def test1(self):
        rules = TokenRules([
            TokenMatchIgnoreButKeep('NEWLINE', r'\s+', keep='\n'),
            TokenMatch('IDENTIFIER', TokenMatch.ID_UNICODE),
            TokenMatchInt('CONSTANT', r'-?[0-9]+'),
        ])
        s = "    abc123 def _has_underbars_ \n\n  ghi_jkl     123456\n"
        tkz = Tokenizer(rules, [s])
        expected_IDvals = [
            (rules.TokenID.IDENTIFIER, 'abc123'),
            (rules.TokenID.IDENTIFIER, 'def'),
            (rules.TokenID.IDENTIFIER, '_has_underbars_'),
            (rules.TokenID.NEWLINE, '\n'),
            (rules.TokenID.IDENTIFIER, 'ghi_jkl'),
            (rules.TokenID.CONSTANT, 123456),
            (rules.TokenID.NEWLINE, '\n')
        ]

        for x, t in strictzip(expected_IDvals, tkz.tokens()):
            tid, val = x
            self.assertEqual(t.id, tid)
            self.assertEqual(t.value, val)

    def test_badregexp(self):
        # make sure bad regexps cause ValueError in TokenMatch
        with self.assertRaises(ValueError):
            _ = TokenMatch('a', r'[')        # unbalanced regexp

    def test_identifiers(self):
        # various combinations of the "identifier" expressions provided

        s = "MötleyCrüe 4_foo_bar77"
        testvectors = (
            #   (RULES, EXPECTED)
            (TokenRules((TokenMatch('ID', TokenMatch.ID_UNICODE),
                         TokenMatchIgnore('WHITESPACE', r'\s+'),
                         TokenMatch('DEBRIS', '.'))),
             (('ID', 'MötleyCrüe'),
              ('DEBRIS', '4'),
              ('ID', '_foo_bar77'))
             ),
            (TokenRules((TokenMatch('ID', TokenMatch.ID_UNICODE_NO_UNDER),
                         TokenMatchIgnore('WHITESPACE', r'\s+'),
                         TokenMatch('DEBRIS', '.'))),
             (('ID', 'MötleyCrüe'),
              ('DEBRIS', '4'),
              ('DEBRIS', '_'),
              ('ID', 'foo'),
              ('DEBRIS', '_'),
              ('ID', 'bar77'))
             ),
            (TokenRules((TokenMatch('ID', TokenMatch.ID_ASCII),
                        TokenMatchIgnore('WHITESPACE', r'\s+'),
                        TokenMatch('DEBRIS', '.'))),
             (('ID', 'M'),
              ('DEBRIS', 'ö'),
              ('ID', 'tleyCr'),
              ('DEBRIS', 'ü'),
              ('ID', 'e'),
              ('DEBRIS', '4'),
              ('ID', '_foo_bar77'))
             ),
            (TokenRules((TokenMatch('ID', TokenMatch.ID_ASCII_NO_UNDER),
                         TokenMatchIgnore('WHITESPACE', r'\s+'),
                         TokenMatch('DEBRIS', '.'))),
             (('ID', 'M'),
              ('DEBRIS', 'ö'),
              ('ID', 'tleyCr'),
              ('DEBRIS', 'ü'),
              ('ID', 'e'),
              ('DEBRIS', '4'),
              ('DEBRIS', '_'),
              ('ID', 'foo'),
              ('DEBRIS', '_'),
              ('ID', 'bar77'))
             )
        )

        for rules, expected in testvectors:
            tkz = Tokenizer(rules, [s])
            for x, t in strictzip(expected, tkz.tokens()):
                ids, val = x
                tid = getattr(rules.TokenID, ids)
                self.assertEqual(t.id, tid)
                self.assertEqual(t.value, val)

    def test_iter(self):
        rules = TokenRules([TokenMatch('A', 'a'),
                            TokenMatch('B', 'b')])
        tkz = Tokenizer(rules, ["ab", "ba"])
        expected = [
            rules.TokenID.A,
            rules.TokenID.B,
            rules.TokenID.B,
            rules.TokenID.A,
        ]

        for id, t in strictzip(expected, tkz):
            self.assertEqual(id, t.id)

    # this used to fail because tokens() couldn't iterate None
    def test_noinput(self):
        tkz = Tokenizer(TokenRules([TokenMatch('A', 'a')]))
        toks = list(tkz)
        self.assertEqual(len(toks), 0)

    def test_locations(self):
        rules = TokenRules([TokenMatch('ONE_A', r'a'),
                            TokenMatch('ANY_B', r'b+'),
                            TokenMatch('CAB', r'ca+b')])

        tkz = Tokenizer(rules, ["abaabbbcaaabbxcab"])

        expected = [
            # TokenID, startpos, endpos
            (rules.TokenID.ONE_A, 0, 1),
            (rules.TokenID.ANY_B, 1, 2),
            (rules.TokenID.ONE_A, 2, 3),
            (rules.TokenID.ONE_A, 3, 4),
            (rules.TokenID.ANY_B, 4, 7),
            (rules.TokenID.CAB, 7, 12),
            (rules.TokenID.ANY_B, 12, 13),
            (Tokenizer.MatchError, 13, 14),
            ]

        tg = tkz.tokens()
        for xp in expected:
            try:
                result = next(tg)
            except Tokenizer.MatchError as x:
                self.assertEqual(xp[1], x.location.startpos)
                self.assertEqual(xp[2], x.location.endpos)
            else:
                self.assertEqual(xp[0], result.id)
                self.assertEqual(xp[1], result.location.startpos)
                self.assertEqual(xp[2], result.location.endpos)

    def test_lines(self):
        rules = TokenRules([TokenMatch('A', 'a'),
                            TokenMatch('B', 'b'),
                            TokenMatch('C', 'c')])
        tkz = Tokenizer(rules, ["aba", "cab"], )
        expected = [
            (rules.TokenID.A, 1, 0),
            (rules.TokenID.B, 1, 1),
            (rules.TokenID.A, 1, 2),
            (rules.TokenID.C, 2, 0),
            (rules.TokenID.A, 2, 1),
            (rules.TokenID.B, 2, 2),
        ]
        for x, t in strictzip(expected, tkz):
            tokid, lineno, startpos = x
            self.assertEqual(tokid, t.id)
            self.assertEqual(t.location.lineno, lineno)
            self.assertEqual(t.location.startpos, startpos)
            # just knows each test is 1 char
            self.assertEqual(t.location.endpos, startpos+1)

    def test_nomatch(self):
        rules = TokenRules([TokenMatch('A', 'a'),
                            TokenMatch('B', 'b')])
        lines = ["ab", "baxb"]
        tkz = Tokenizer(rules, lines, loc=TokLoc(lineno=0))
        expected = [
            rules.TokenID.A,
            rules.TokenID.B,
            rules.TokenID.B,
            rules.TokenID.A,
            None,
        ]
        g = tkz.tokens()
        for expected_id in expected:
            try:
                t = next(g)
            except Tokenizer.MatchError as e:
                # should be the 'x' in the reported lineno
                c = lines[e.location.lineno][e.location.startpos]
                self.assertEqual(c, 'x')
            else:
                self.assertEqual(expected_id, t.id)

    # C comment example
    def test_C(self):

        # note: this is also implicitly a test of NEXTRULE
        nextrule = TokenMatchRuleSwitch.NEXTRULE

        tms = [
            # just a few other lexical elements thrown in for example
            TokenMatch('LBRACE', r'{'),
            TokenMatch('RBRACE', r'}'),
            TokenMatch('IDENTIFIER', TokenMatch.ID_ASCII),
            TokenMatchRuleSwitch(
                'COMMENT_START', r'/\*', rulename=nextrule),
            TokenMatch('BAD', r'.'),
        ]
        mainrules = NamedRuleSet(rules=tms)

        tms = [
            # eat everything that is not a star
            TokenMatchIgnore('C_NOTSTAR', r'[^*]+'),

            # */ ends the comment and returns to regular rules
            TokenMatchRuleSwitch('COMMENT_END', r'\*/', rulename=nextrule),

            # when a star is seen that isn't */ this eats it
            TokenMatchIgnore('C_STAR', r'\*'),
        ]
        altrules = NamedRuleSet(rules=tms, name='ALT')

        rules = TokenRules(mainrules, altrules)

        for sx, expected in (
                (["abc/*", "def*/"],
                 ['IDENTIFIER', 'COMMENT_START', 'COMMENT_END']),
                (["/**/"], ['COMMENT_START', 'COMMENT_END']),
                (["{/**/}"],
                 ['LBRACE', 'COMMENT_START', 'COMMENT_END', 'RBRACE']),
                (["/***/"], ['COMMENT_START', 'COMMENT_END']),
                (["/****/"], ['COMMENT_START', 'COMMENT_END']),
                (["/*****/"], ['COMMENT_START', 'COMMENT_END']),
                (["/* */"], ['COMMENT_START', 'COMMENT_END']),
                (["/* * / * */"], ['COMMENT_START', 'COMMENT_END']),
                (["abc/*", "def*/"],
                 ['IDENTIFIER', 'COMMENT_START', 'COMMENT_END']),
                (["/* here is a bunch",
                  "of lines representing a wordy C comment.",
                  "** this one even has * characters and / characters",
                  "and, oh my, event a second /* to see what happens.",
                  "This brace is not matched because in comment: {",
                  "here is the end of the comment: */",
                  "BUT_THIS_IS_AN_IDENTIFIER"],
                 ['COMMENT_START', 'COMMENT_END', 'IDENTIFIER']),
                ):
            tkz = Tokenizer(rules, sx)
            toks = list(tkz.tokens())
            with self.subTest(sx=sx):
                for name, t in strictzip(expected, toks):
                    self.assertEqual(rules.TokenID[name], t.id)

    # check that duplicated toknames are allowed
    def test_dups(self):
        rules = TokenRules([TokenMatch('FOO', 'f'),
                            TokenMatch('BAR', 'b'),
                            TokenMatch('FOO', 'zzz')])
        tkz = Tokenizer(rules)

        expected = (('FOO', 'f'), ('BAR', 'b'), ('FOO', 'zzz'))
        for token, ex in strictzip(
                tkz.string_to_tokens('fbzzz'), expected):
            self.assertEqual(token.id, rules.TokenID[ex[0]])
            self.assertEqual(token.value, ex[1])

    # Test naked tokenIDs (no regexp)
    def test_tokIDonly(self):
        rules = TokenRules([
            TokenMatch('CONSTANT', r'-?[0-9]+'),
            TokenMatch('FOO', None),
            TokenIDOnly('BAR')
        ])
        self.assertTrue(hasattr(rules.TokenID, 'FOO'))
        self.assertTrue(hasattr(rules.TokenID, 'BAR'))
        self.assertTrue(hasattr(rules.TokenID, 'CONSTANT'))

    # Example of multiple rule sets from README
    def test_ruleswitch(self):

        group1 = [
            TokenMatch('ZEE', r'z'),
            TokenMatchRuleSwitch('ALTRULES', r'/@/', rulename='ALT')
        ]

        group2 = [
            TokenMatch('ZED', r'z'),
            TokenMatchRuleSwitch('MAINRULES', r'/@/')
        ]

        ng1 = NamedRuleSet(rules=group1)
        ng2 = NamedRuleSet(rules=group2, name='ALT')
        rules = TokenRules(ng1, ng2)
        tkz = Tokenizer(rules)
        expected = (
            rules.TokenID.ZEE,
            rules.TokenID.ZEE,
            rules.TokenID.ALTRULES,
            rules.TokenID.ZED,
            rules.TokenID.MAINRULES,
            rules.TokenID.ZEE,
        )

        for token, ex in strictzip(
                tkz.string_to_tokens('zz/@/z/@/z'), expected):
            self.assertEqual(token.id, ex)
            if ex in (rules.TokenID.ZEE, rules.TokenID.ZED):
                self.assertEqual(token.value, 'z')
            elif ex in (rules.TokenID.ALTRULES,
                        rules.TokenID.MAINRULES):
                self.assertEqual(token.value, '/@/')
            else:
                self.assertTrue(False)

    # This tests that it is ok to have duplicate names across rulesets
    # It also tests an explicit (not-None) name for the primary rules
    def test_ruleswitch2(self):

        r1 = [
            TokenMatch('ZEE', r'z'),
            TokenMatchRuleSwitch('SWITCH', r'/@/', rulename='ALT')
        ]

        r2 = [
            TokenMatch('ZED', r'z'),
            TokenMatchRuleSwitch('SWITCH', r'/@/', rulename='PRIMARY')
        ]
        ng1 = NamedRuleSet(rules=r1, name='PRIMARY')
        ng2 = NamedRuleSet(rules=r2, name='ALT')
        rules = TokenRules(ng1, ng2)

        tkz = Tokenizer(rules)
        expected = (
            rules.TokenID.ZEE,
            rules.TokenID.ZEE,
            rules.TokenID.SWITCH,
            rules.TokenID.ZED,
            rules.TokenID.SWITCH,
            rules.TokenID.ZEE,
        )

        for token, ex in strictzip(
                tkz.string_to_tokens('zz/@/z/@/z'), expected):
            self.assertEqual(token.id, ex)
            if ex in (rules.TokenID.ZEE, rules.TokenID.ZED):
                self.assertEqual(token.value, 'z')
            elif ex == rules.TokenID.SWITCH:
                self.assertEqual(token.value, '/@/')
            else:
                self.assertTrue(False)

    # Same as ruleswitch2 but breaking each token into its own string
    # so as to test whether it works right if a rules switch is the
    # end of an individual line string.
    def test_ruleswitch_eol(self):

        r1 = [
            TokenMatch('ZEE', r'z'),
            TokenMatchRuleSwitch('SWITCH', r'/@/', rulename='ALT')
        ]

        r2 = [
            TokenMatch('ZED', r'z'),
            TokenMatchRuleSwitch('SWITCH', r'/@/', rulename='PRIMARY')
        ]
        ng1 = NamedRuleSet(rules=r1, name='PRIMARY')
        ng2 = NamedRuleSet(rules=r2, name='ALT')
        rules = TokenRules(ng1, ng2)

        lines = [
            'z',
            'z',
            '/@/',
            'z',
            '/@/',
            'z'
        ]

        tkz = Tokenizer(rules, lines)
        expected = (
            rules.TokenID.ZEE,
            rules.TokenID.ZEE,
            rules.TokenID.SWITCH,
            rules.TokenID.ZED,
            rules.TokenID.SWITCH,
            rules.TokenID.ZEE,
        )

        for token, ex in strictzip(tkz.tokens(), expected):
            self.assertEqual(token.id, ex)
            if ex in (rules.TokenID.ZEE, rules.TokenID.ZED):
                self.assertEqual(token.value, 'z')
            elif ex == rules.TokenID.SWITCH:
                self.assertEqual(token.value, '/@/')
            else:
                self.assertTrue(False)

    # This test demonstrates returning different types of Token
    # objects depending on the match. Likely not a real use-case.
    def test_factory_2(self):
        class MyToken_1:
            def __init__(self, tokid, value, location, /):
                self.id = tokid
                self.value = value
                self.location = location

        class MyToken_2:
            def __init__(self, tokid, value, location, /):
                self.id = tokid
                self.value = value
                self.location = location

        class TokenMatch_1(TokenMatch):
            def action(self, ta, /):
                return MyToken_1(ta.token_id, ta.value, ta.location)

        class TokenMatch_2(TokenMatch):
            def action(self, ta, /):
                return MyToken_2(ta.token_id, ta.value, ta.location)

        rules = TokenRules([
            TokenMatch('NATIVE', '0'),
            TokenMatch_1('_1', '1'),
            TokenMatch_2('_2', '2'),
        ])

        expected = [
            Tokenizer.Token, MyToken_1, MyToken_2, Tokenizer.Token]
        tkz = Tokenizer(rules)
        classes = [t.__class__ for t in tkz.string_to_tokens('0120')]
        self.assertEqual(classes, expected)

    # this tests whether the Token type can successfully be overridden
    # by subclassing Tokenizer
    def test_subclasstoken(self):

        @dataclass
        class MyToken:
            id: Enum
            value: str
            location: TokLoc

            def __post_init__(self):
                self.foo = 'bar'

        class MyTokenizer(Tokenizer):
            Token = MyToken

        rules = TokenRules([TokenMatch('A', 'a')])
        s = "aa"
        tkz = MyTokenizer(rules, [s])
        for t in tkz.tokens():
            self.assertEqual(t.foo, 'bar')

    def test_locinfo(self):
        t = Tokenizer(TokenRules([TokenMatch('A', 'a')]))
        toks = list(t.tokens(["a"], loc=TokLoc(sourcename='foo', lineno=17)))
        self.assertEqual(len(toks), 1)
        k = toks[0]
        self.assertEqual(k.location.sourcename, 'foo')
        self.assertEqual(k.location.lineno, 17)

    def test_keywords(self):
        rules = TokenRules([
            TokenMatchIgnoreButKeep('NEWLINE', r'\s+', keep='\n'),
            TokenMatchKeyword('if'),
            TokenMatchKeyword('then'),
            TokenMatch('IDENTIFIER', r'[^\W\d]\w*'),
        ])

        s = "if then Then thence thençe ifõ\n"
        tkz = Tokenizer(rules, [s])
        expected = [
            (rules.TokenID.IF, 'if'),
            (rules.TokenID.THEN, 'then'),
            (rules.TokenID.IDENTIFIER, 'Then'),
            (rules.TokenID.IDENTIFIER, 'thence'),
            (rules.TokenID.IDENTIFIER, 'thençe'),
            (rules.TokenID.IDENTIFIER, 'ifõ'),
            (rules.TokenID.NEWLINE, '\n')
        ]
        for x, t in strictzip(expected, tkz.tokens()):
            tid, val = x
            with self.subTest(tid=tid, val=val, t=t):
                self.assertEqual(t.id, tid)
                self.assertEqual(t.value, val)


def run_unit_tests(_=None):
    unittest.main()


if __name__ == "__main__":
    run_unit_tests()
