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


from tokenizer import Tokenizer, TokenMatch, Token
from tokenizer import TokenMatchIgnoreButKeep
from tokenizer import TokenMatchIgnore


# some ASM-specific TokenMatch subclasses

class TokenMatchASMString(TokenMatch):
    """Take the <> brackets off an 'as' string."""

    def action(self, val, loc, tkz, /):
        v = val[1:-1]
        name = self.tokname
        try:
            s = ASMTokenizer.str_deslash(v)
        except ValueError:
            s = f"bad string: **{v}**"
            name = 'BAD'
        return super().action(s, loc, tkz, name=name)


class TokenMatchASMConstant(TokenMatch):
    """Convert all the various integer formats; see _intcvt."""

    def _value(self, val, /):
        return ASMTokenizer._intcvt(val)


class TokenMatchID8(TokenMatch):
    """In "id8" mode, long identifiers are accepted but truncated."""

    def _value(self, val, /):
        return val[:8]


class ASMTokenizer(Tokenizer):
    """Lexical analysis on a textfile."""

    def __init__(self, *args, id8=False, **kwargs):
        """Set up a Tokenizer; see tokens() to generate tokens.

        Arguments:
           Any Tokenizer argument -- see Tokenizer()
           EXCEPT: the rules and the tokenIDs args are set here

           id8      -- Turn on strict 8-char identifier *significance*

        NOTE: To tokenize a SINGLE string s, do this:
           ASMTokenizer([s])
        """

        # the normal rules or the "truncate identifiers" rules...
        if id8:
            rules = [r if r.tokname != 'IDENTIFIER'
                     else TokenMatchID8(r.tokname, r.regexp)
                     for r in _rules]
        else:
            rules = _rules

        super().__init__(rules, *args, tokenIDs=TokenID, **kwargs)

    @staticmethod
    def __deslash1(s):
        """Returns 1st char, possibly escape-converted, and remainder of s."""
        if s[0] == '\\':
            if len(s) < 2:
                raise ValueError("backslash at end of string")
            return {'n': '\n',
                    't': '\t',
                    'e': chr(4),
                    '0': chr(0),
                    'r': '\r',
                    'a': chr(6),
                    'p': chr(0o33),
                    '\\': '\\',
                    '>': '>'}.get(s[1], s[1]), s[2:]
        else:
            return s[0], s[1:]

    @classmethod
    def str_deslash(cls, s):
        """Returns a string with the slash-escapes converted accordingly."""
        result = ""
        while s:
            ch, s = cls.__deslash1(s)
            result += ch
        return result

    # handles the various syntax for integer constants:
    #    If it starts with a single quote, convert 1 character
    #    If it starts with a double quote, convert 2 characters
    #    If it ends with '.' then it is a decimal integer
    #    OTHERWISE it is octal, but digits 8 and 9 are allowed(!)
    # This syntax is defined by the unix v7 assembler; no complaints accepted.
    @classmethod
    def _intcvt(cls, s):
        if s[0] == "'":
            slo, _ = cls.__deslash1(s[1:])
            vlo = ord(slo)
            if vlo > 255:
                raise ValueError(f"bad character '{s}'")
            return vlo
        elif s[0] == '"':
            slo, s = cls.__deslash1(s[1:])
            shi, s = cls.__deslash1(s)
            vlo, vhi = ord(slo), ord(shi)
            if vlo > 255 or vhi > 255:
                raise ValueError(f"bad character '{value}'")
            return (vhi << 8) | vlo
        else:
            sign = 1
            if s[0] == '-':
                sign = -1
                s = s[1:]
            if s[-1] == '.':
                base = 10
                s = s[:-1]
            else:
                base = 8
            v = 0
            place = 1
            for digit in reversed(s):
                v += (int(digit) * place)
                place *= base
            return (v * sign) & 0xFFFF


_rules = [
    TokenMatchIgnoreButKeep('NEWLINE', r'\s+', keep='\n'),
    TokenMatch('IDENTIFIER', r'[A-Za-z_~\.][A-Za-z_~\.0-9]*'),
    TokenMatch('TEMPLABREF', r'[0-9](f|b)'),
    TokenMatchASMConstant('CONSTANT', r'[0-9]+\.?'),
    TokenMatch('LSHIFT', r'\\<'),
    TokenMatch('RSHIFT', r'\\>'),
    TokenMatchASMString('STRING', r'<[^>]*>'),
    TokenMatch('LPAREN', r'\('),
    TokenMatch('RPAREN', r'\)'),
    TokenMatch('LBRA', r'\['),
    TokenMatch('RBRA', r'\]'),
    TokenMatch('EQ', r'='),
    TokenMatch('PLUS', r'\+'),
    TokenMatch('MINUS', r'-'),
    TokenMatch('STAR', r'\*'),
    TokenMatch('VSLASHES', r'\\/'),
    TokenMatch('AMPERSAND', r'&'),
    TokenMatch('BAR', r'\|'),
    TokenMatch('PERCENT', r'%'),
    TokenMatch('BANG', r'\!'),
    TokenMatch('CARET', r'\^'),
    TokenMatch('DOLLAR', r'\$'),
    TokenMatch('COLON', r'\:'),
    TokenMatch('SEMICOLON', r'\;'),

    # in prior implementations these made DQUOTED/SQUOTED tokens
    # but now ASMConstant can handle all formats and doesn't need
    # those phantom token types to do it
    TokenMatchASMConstant('CONSTANT', r"'((\\.)|.)"),
    TokenMatchASMConstant('CONSTANT', r'\"((\\.)|.){2}'),

    TokenMatchIgnore('COMMENT', r'/[^\n]*'),
    TokenMatch('COMMA', r','),

    # matches anything; MUST (obviously?) be the last re
    TokenMatch('BAD', r'.'),

    # but these can come after .. special tokens w/no 're' pattern
    TokenMatch('EOF', None)
]

TokenID = Tokenizer.create_tokenID_enum(_rules)

# some handy categories
STMT_ENDS = {TokenID.NEWLINE, TokenID.SEMICOLON}

if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):
        def test1(self):
            tx = ASMTokenizer()
            stmt = list(tx.string_to_tokens("  inc  r2"))
            self.assertEqual(len(stmt), 2)
            t0, t1 = stmt
            self.assertEqual(t0.id, TokenID.IDENTIFIER)
            self.assertEqual(t0.value, "inc")
            self.assertEqual(t1.id, TokenID.IDENTIFIER)
            self.assertEqual(t1.value, "r2")

        def test2(self):
            # test recognition of temporary labels
            tx = ASMTokenizer()
            tokens = list(tx.string_to_tokens("2:  inc  r2 ; br 2b"))

            self.assertEqual(len(tokens), 7)
            self.assertEqual(tokens[0].id, TokenID.CONSTANT)
            self.assertEqual(tokens[0].value, 2)

            self.assertEqual(tokens[2].id, TokenID.IDENTIFIER)
            self.assertEqual(tokens[2].value, "inc")
            self.assertEqual(tokens[3].id, TokenID.IDENTIFIER)
            self.assertEqual(tokens[3].value, "r2")

            self.assertEqual(tokens[4].id, TokenID.SEMICOLON)

            self.assertEqual(tokens[5].id, TokenID.IDENTIFIER)
            self.assertEqual(tokens[5].value, "br")

            self.assertEqual(tokens[6].id, TokenID.TEMPLABREF)
            self.assertEqual(tokens[6].value, '2b')

        def testconstants(self):
            # test parsing of various constant syntax
            tx = ASMTokenizer()

            for s, val in (
                    ("177777", 65535),
                    ("65535.", 65535),
                    ("8", 8),
                    ("9", 9),
                    ("8.", 8),
                    ("9.", 9),
                    ("18", 16),          # that goofy octal syntax
                    ("18.", 18),
                    (r"'\e", 4),         # some single-quote examples
                    (r"'\n", 0o12),
                    (r"'\t", 0o11),
                    (r"'\0", 0),
                    (r"'\a", 6),
                    (r"'\p", 0o33),
                    (r"'\>", ord('>')),
                    (r"'A", 65),
                    (r'"AB', (66 << 8) + 65),   # some double-quote examples
                    (r'"A\e', (4 << 8) + 65),
                    (r'"\eA', (65 << 8) + 4),
                    (r'"\r\n', (10 << 8) + 13),
                    ):
                tokens = list(tx.string_to_tokens(s))
                with self.subTest(s=s, val=val):
                    self.assertEqual(len(tokens), 1)
                    self.assertEqual(tokens[0].id, TokenID.CONSTANT)
                    self.assertEqual(tokens[0].value, val)

        def test_badquotes(self):
            # test mal-formed single/double quote and <> formats
            tx = ASMTokenizer()
            for s in (r"'ab", r'"abc', r'<abc', r'<abc\>'):
                with self.subTest(s=s):
                    tokens = list(tx.string_to_tokens(s))
                    # some cause 2 tokens, some cause BAD
                    if len(tokens) > 1:
                        self.assertEqual(len(tokens), 2)
                        # the second token is an IDENTIFIER in these cases
                        self.assertEqual(tokens[1].id, TokenID.IDENTIFIER)
                    else:
                        self.assertEqual(len(tokens), 1)
                        self.assertEqual(tokens[0].id, TokenID.BAD)

        def test_newline(self):
            # a bug caused newlines to get swallowed into into WHITESPACE
            # so this tests for that.
            tx = ASMTokenizer()
            for s in (
                    "a\nb\nc\n",
                    "   a    \n     b \n   c \n",
                    "   a    \n     b \n   c \n   ",
                    "   a    \n\n\n     b \n\n\n   c \n\n\n   ",
                    ):
                tokens = list(tx.string_to_tokens(s))
                with self.subTest(s=s):
                    self.assertEqual(len(tokens), 6)
                    for i in 1, 3, 5:
                        self.assertEqual(tokens[i].id, TokenID.NEWLINE)
                    for i, name in ((0, "a"), (2, "b"), (4, "c")):
                        self.assertEqual(tokens[i].id, TokenID.IDENTIFIER)
                        self.assertEqual(tokens[i].value, name)

        def test_strict(self):
            tx = ASMTokenizer(id8=True)
            s = "abc456789 abc45678"
            tokens = list(tx.string_to_tokens(s))
            self.assertEqual(tokens[0].value, tokens[1].value)
            self.assertEqual(tokens[0].value, "abc45678")

        def test_strings(self):
            tx = ASMTokenizer()

            for s in (
                "<>",
                "< <>",
                "< abcd1234 !@#$%^&*()+ !!~>",
                "<<A>",
                "<<>"
            ):
                tokens = list(tx.string_to_tokens(s))
                with self.subTest(s=s):
                    self.assertEqual(len(tokens), 1)
                    self.assertEqual(tokens[0].id, TokenID.STRING)
                    self.assertEqual(tokens[0].value, s[1:-1])

    unittest.main()
