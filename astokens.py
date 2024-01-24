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

from dataclasses import dataclass
from enum import Enum
import typing
import re


#
# REGULAR EXPRESSIONS for lexical analysis of tokens. NOTE:
#   * ORDER MATTERS. The first match will rule.
#   * The TokenID Enum is built dynamically from names here.
# See also _othertokens for a few more types of tokens.
#

token_specs = [
    ('WHITESPACE', r'\s+'),        # but see _tokmods re: NEWLINE
    ('IDENTIFIER', r'[A-Za-z_~\.][A-Za-z_~\.0-9]*'),
    ('TEMPLABREF', r'[0-9](f|b)'),
    ('CONSTANT', r'-?[0-9]+\.?'),
    ('STRING', r'<>|(<[^<][^>]*>)'),
    ('LPAREN', r'\('),
    ('RPAREN', r'\)'),
    ('LBRA', r'\['),
    ('RBRA', r'\]'),
    ('EQ', r'='),
    ('PLUS', r'\+'),
    ('MINUS', r'-'),
    ('STAR', r'\*'),
    ('VSLASHES', r'\\/'),
    ('AMPERSAND', r'&'),
    ('BAR', r'\|'),
    ('RR', r'>>'),
    ('LL', r'<<'),
    ('PERCENT', r'%'),
    ('BANG', r'\!'),
    ('CARET', r'\^'),
    ('DOLLAR', r'\$'),
    ('COLON', r'\:'),
    ('SEMICOLON', r'\;'),
    ('SQUOTED', r"'((\\.)|.)"),
    ('DQUOTED', r'\"((\\.)|.){2}'),
    ('COMMENT', r'/[^\n]*'),
    ('COMMA', r','),

    ('BAD', r'.'),       # matches anything; MUST (obviously?) BE LAST!!
]


# these have no corresponding regex
_othertokens = ['NEWLINE', 'NULLTOKEN', 'EOF']

TokenID = Enum('TokenID', [t for t, r in token_specs] + _othertokens)

# some handy categories
TokenID.STMT_ENDS = {TokenID.NEWLINE, TokenID.SEMICOLON}

# symbols that are legal in expressions
TokenID.EXPR_SYM = {
    TokenID.IDENTIFIER, TokenID.CONSTANT,   # these two are obvious
    TokenID.TEMPLABREF,                     # same as a label ref
}


# where a given token was found; solely for syntax error reporting
@dataclass(frozen=True)
class TokLoc:
    source: str = "unknown"
    line: int = None
    start: int = 0
    end: int = 0


@dataclass
class Token:
    id: TokenID
    value: typing.Any
    orig: str
    location: TokLoc = TokLoc()

    @classmethod
    def nulltok(cls, val=None):
        """Convenience for constructing a NULLTOKEN."""
        return cls(TokenID.NULLTOKEN, value=val, orig=None, location=TokLoc())


class ASMTokenizer:
    """Lexical analysis on a textfile."""

    # set this to True to retain the Unix "only the first 8 characters
    # are significant" feature of identifiers; otherwise they are full-length
    # See also id8 argument in __init__
    STRICT_ID8 = False

    def __init__(self, strings=None, /, *, name=None, startnum=1, id8=False):
        """Set up a Tokenizer; see tokens() to generate tokens.

        Arguments:
           strings  -- should be an iterable of strings. Most commonly
                       it is an open text file. It is used duck-typed as:
                          for s in strings:
                              ... tokenize s ...
           name     -- for error reporting, but otherwise ignored.
                       Usually should be specified as the input file name.

           startnum -- for error reporting, but otherwise ignored.
                       Each string in strings is assumed to be a separate
                       line and will be numbered starting from this number.
                       Default is 1. If None, line numbers left out.

           id8      -- Turn on strict 8-char identifier *significance*

        NOTE: To tokenize a SINGLE string s, do this:
           ASMTokenizer([s])

        """
        self.strings = strings
        self.name = name
        self.startnum = startnum
        if id8:
            self.STRICT_ID8 = True

    def tokens(self):
        """GENERATE tokens for the entire file."""
        if self.startnum is None:         # no line numbers
            g = ((None, s) for s in self.strings)
        else:
            g = enumerate(self.strings, start=self.startnum)
        for i, s in g:
            yield from self.string_to_tokens(s, linenumber=i, name=self.name)

    def string_to_tokens(self, s, /, *, linenumber=None, name=None):
        """GENERATE tokens from a string.

        Optional keyword argument linenumber will be put into error messages.
        """
        tok_re = '|'.join(f'(?P<{tn}>{x})' for tn, x in token_specs)
        for mo in re.finditer(tok_re, s):
            tok_ID, value = self._tokmods(TokenID[mo.lastgroup], mo.group())
            if tok_ID is not None:   # None means ignore this token
                loc = TokLoc(name, linenumber, mo.start(), mo.end())
                yield Token(tok_ID, value, s, loc)

    def _tokmods(self, tok_ID, value):
        """Return a new tok_ID, value per special rules for some tokens."""

        # some TokenIDs get modified:
        #   WHITESPACE -- if it contains a \n return a NEWLINE
        #                 otherwise don't even return it; skip it
        #   COMMENT    -- don't even return it; skip it
        #   CONSTANT   -- convert the value from string to int
        #   STRING     -- backslash processing, strip the bracketing <>
        #   DQUOTED    -- convert two characters to CONSTANT
        #   SQUOTED    -- convert one character to CONSTANT
        #   IDENTIFIER -- optionally enforce length8 truncation.

        match tok_ID:
            case TokenID.WHITESPACE:
                # It seems desirable to use the re '\s' for WHITESPACE
                # but that also matches \n, and newlines have semantic
                # significance as they create a statement boundary.
                # Easy solution here: if there is one or more newlines
                # in the WHITESPACE, return a NEWLINE (one suffices
                # regardlessof how many newlines are in there)
                #
                # Aside from that special case, WHITESPACE is otherwise
                # ignored and never returned in the token stream.
                if '\n' in value:
                    return TokenID.NEWLINE, "\n"
                else:
                    return None, None
            case TokenID.COMMENT:
                return None, None
            case TokenID.CONSTANT:
                return TokenID.CONSTANT, self._intcvt(value)
            case TokenID.STRING:
                try:
                    return TokenID.STRING, self.str_deslash(value[1:-1])
                except ValueError:
                    return TokenID.BAD, f"bad string: **{value[1:-1]}**"
            case (TokenID.SQUOTED | TokenID.DQUOTED):
                return TokenID.CONSTANT, self._intcvt(value)
            case TokenID.IDENTIFIER:
                if self.STRICT_ID8:
                    return TokenID.IDENTIFIER, value[:8]

        # if no special cases then return unchanged
        return tok_ID, value

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

            for s, good in (
                ("<>", True),
                ("< <>", True),
                ("< abcd1234 !@#$%^&*()+ !!~>", True),
                ("<<>", False),
            ):
                tokens = list(tx.string_to_tokens(s))
                with self.subTest(s=s):
                    if good:
                        self.assertEqual(len(tokens), 1)
                        self.assertEqual(tokens[0].id, TokenID.STRING)
                        self.assertEqual(tokens[0].value, s[1:-1])

    unittest.main()
