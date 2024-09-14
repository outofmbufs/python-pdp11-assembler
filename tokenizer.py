# A generic tokenizer driven by regular expressions

from collections import namedtuple
from enum import Enum
import re

# Regular-expression line-oriented tokenizer.
#
# This is a thin layer on top of using re.match or re.finditer directly,
# and so is useful for lexing when a line-by-line/regexp model works.
#
# The basics:
#    Tokenizer        Main object. Built from rulesets of TokenMatch objects.
#
#    TokenMatch       Object encapsulating a basic regexp rule.
#    ... subclasses   Various subclasses of TokenMatch for special functions
#
#    Token            The Tokenizer produces these.
#    TokenID          An Enum type dynamically created by the Tokenizer
#                     from all of the TokenMatch specifications; this is the
#                     type of each individual Token (i.e., what it matched)
#
#    TokLoc   -- source location information, for error reporting.


# A Token has an id (typically a TokenID enum), a value, and a 'location'
# (typically a TokLoc ... information about the source the token came from).
# This is the default Token type produced by the Tokenizer.
# Applications can override this, see Tokenizer arguments.

Token = namedtuple('Token', ['id', 'value', 'location'])


class Tokenizer:
    """Break streams into tokens with rules from regexps."""

    def __init__(self, tms, strings=None, /, *,
                 srcname=None, startnum=1,
                 tokenIDs=None,
                 tokentype=Token):
        """Set up a Tokenizer; see tokens() to generate tokens.

        Arguments:
           tms      -- Either a sequence of TokenMatch objects or a mapping
                       of names to such sequences.

           strings  -- should be an iterable of strings. Can be None.
                       Most commonly it is an open text file, and it
                       gets used like this:
                          for s in strings:
                              ... tokenize s ...
                       so anything duck-typing as iterable of str works.

           **NOTE** :: A TokenMatch will not find a match ACROSS a string
                       boundary. Said differently: Every Token must lie
                       within a single string. Typically this is a non-issue
                       when each string is a line in a file but it could be
                       a problem if the strings are arbitrary chunks of some
                       larger source. See also: Tokenizer.linefilter

           srcname  -- for error reporting, but otherwise ignored.
                       Usually should be specified as the input file name.
                       This eventually goes into TokLoc location.

           startnum -- for error reporting, but otherwise ignored.
                       Each string in strings is assumed to be a separate
                       line and will be numbered starting from this number.
                       Default is 1. If None, line numbers left out.

           tokenIDs -- If caller doesn't want the TokenID Enum created
                       automatically, it can be provided here.

           tokentype -- will be used in lieu of Token(), if provided.
        """

        tmsmap = self.__tmscvt(tms)
        self.TokenID = tokenIDs or self.create_tokenID_enum(tmsmap)

        # each named ruleset will become one regexp with a (?P=name)
        # annotation for each individual regexp in it. The 'name' in
        # those annotations is a "pname" and it is not the same as
        # the tokname because toknames are allowed to be duplicated.
        #
        RuleSet = namedtuple('RuleSet', ['joined_rx', 'pmap', 'name'])

        self.rulesets = {}
        for name, tms in tmsmap.items():
            pmap = {f"PN{i:04d}": tm for i, tm in enumerate(tms)}
            joined_rx = '|'.join(f'(?P<{pname}>{tm.regexp})'
                                 for pname, tm in pmap.items()
                                 if tm.regexp is not None)
            self.rulesets[name] = RuleSet(joined_rx, pmap, name)

        self.strings = strings
        self.rules = self.rulesets[None]
        self.startnum = startnum
        self.srcname = srcname
        self.tokentype = tokentype

    @staticmethod
    def __tmscvt(tms):
        """Convert (if necessary) a sequence tms into a map"""
        try:
            _ = tms[None]                # test it for dict-ness
            tmsmap = tms                 # it's already a proper mapping
        except KeyError:
            msg = f"TokenMatch mapping has no default ([None]) entry"
            raise ValueError(msg) from None
        except TypeError:
            # There's just one named ruleset and it was given as
            # an iterable of TokenMatch objects; convert to mapping
            tmsmap = {None: tms}

        # Sanity checks:
        #   - No ruleset may be zero-length
        #   - No ruleset may be named '_next'
        #   - No ruleset may begin with _TKZ_  (for future use)
        #   - There must be a None ruleset name

        # any() can do this but this gives better error messages
        has_a_none = False
        for name in tmsmap:
            if len(tmsmap[name]) == 0:
                raise ValueError(f"Ruleset {name!r}: zero length")
            if name is None:
                has_a_none = True
            elif name == '_next' or name.startswith('_TKZ_'):
                raise ValueError(f"Ruleset name {name!r} is reserved")

        return tmsmap

    @classmethod
    def create_tokenID_enum(cls, tms):
        """Can be called separately to make a TokenID Enum from rules."""
        tmsmap = cls.__tmscvt(tms)

        # collect all the toknames from all the TokenMatch objects
        # NOTE: weed out duplicates (using set()); dups are allowable
        #       when there are multiple context-dependent TokenMatch lists
        toknames = set(r.tokname for mx in tmsmap.values() for r in mx)
        return Enum('TokenID', sorted(toknames))

    # Iterating over a Tokenizer is the same as iterating over
    # the tokens() method, but without the ability to specify other args.
    def __iter__(self):
        return self.tokens()

    def _nextruleset(self):
        """Return the 'next' ruleset -- 'next' defined as circular order."""
        rulenames = list(self.rulesets)
        current = rulenames.index(self.rules.name)
        nextindex = (current + 1) % len(rulenames)
        return rulenames[nextindex]

    def activate_ruleset(self, newrule):
        if newrule == "_next":
            newrule = self._nextruleset()
        self.rules = self.rulesets[newrule]

    _NOTGIVEN = object()     # None is valid for some tokens() args

    def tokens(self, strings=None, /, *,
               srcname=_NOTGIVEN, startnum=_NOTGIVEN):
        """GENERATE tokens. See __init__() for arg descriptions."""

        if strings is not None:
            self.strings = strings

        # Only clobber the __init__ values if these explicitly given.
        # Note that None is a legitimate value, hence the "_NOTGIVEN" object
        if srcname is not self._NOTGIVEN:
            self.srcname = srcname
        if startnum is not self._NOTGIVEN:
            self.startnum = startnum

        if self.startnum is None:         # no line numbers
            g = ((None, s) for s in self.strings)
        else:
            g = enumerate(self.strings, start=self.startnum)
        for i, s in g:
            yield from self.string_to_tokens(
                s, linenumber=i, name=self.srcname)

    def string_to_tokens(self, s, /, *, linenumber=None, name=None):
        """Tokenize string 's', yield Tokens.

           NOTE: optional keyword arguments linenumber and name are
                 entirely for making a TokLoc for better error reporting.
        """

        so_far = 0
        prevrules = None

        # Note that TokenMatch objects can cause active_rulesname to change
        # so the loop is written this way to accommodate that.
        while True:
            # this fires on any rules change AND ALSO the first time through
            if prevrules is not self.rules:
                prevrules = self.rules
                g = re.finditer(self.rules.joined_rx, s[so_far:])
                baseoffset = so_far

            # 'tm' is the TokenMatch that matched
            tm, value, startrel, stoprel = self._nextmatch(g)
            start = startrel + baseoffset
            if tm is None or start != so_far:
                break

            so_far = stoprel + baseoffset   # end of processed chars in s
            loc = TokLoc(s, name, linenumber, start, so_far)

            tok = tm.action(value, loc, self)
            if tok is not None:
                yield tok

        # If haven't made it to the end, something didn't match along the way
        if so_far != len(s):
            loc = TokLoc(s, name, linenumber, so_far, so_far)
            raise self.MatchError(f"unmatched @{so_far}, {s=}", location=loc)

    def _nextmatch(self, g):
        """Support for string_to_tokens; returns next match and info"""

        try:
            mobj = next(g)
        except StopIteration:
            return None, None, -1, -1
        tm = self.rules.pmap[mobj.lastgroup]
        return tm, mobj.group(0), mobj.start(), mobj.end()

    class MatchError(Exception):
        """Exception raised when the input doesn't match any rules"""
        def __init__(self, *args, location=None, **kwargs):
            super().__init__(*args, **kwargs)
            self.location = location
            self.add_note(f"Token Location: {location}")

    # convenience for use in case where strings that end with two
    # character sequence "backslash newline" should be combined with
    # the newline (and backslash) elided. Example of use:
    #
    #     rules = [ blah blah blah ]
    #     f = open('foo.c', 'r')
    #     tz = Tokenizer(rules, Tokenizer.linefilter(f))
    #
    # this allows tokenizing to span "lines" (escaped lines). Note that
    # at some point of complexity the entire idea of "regexp from a line"
    # will break down if the lexical requirements are too complex.
    #
    @staticmethod
    def linefilter(strings, preservelinecount=True):
        """Implement backslash-newline escapes.

        If preservelinecount is True (DEFAULT), lines that are combined
        will have extra "\n" lines generated after them to preserve the
        running line count (if one is being kept).
        """
        makeups = []
        prev = ""
        for s in strings:
            yield from makeups
            makeups = []

            escaped = False
            if s.endswith('\\\n'):
                lastslash = 0
                for pos in range(len(s)-3, -1, -1):
                    if s[pos] != '\\':
                        lastslash = pos + 1
                        break
                nslashes = (len(s) - 1) - lastslash
                # if it's odd then the \n is escaped
                escaped = (nslashes % 2)
                if escaped:
                    prev += s[:-2]         # remove the backslash-newline
                    if preservelinecount:
                        makeups.append("\n")
            if not escaped:
                yield prev + s
                prev = ""
        if prev:
            yield prev
        yield from makeups


# A TokenMatch combines a name (e.g., 'CONSTANT') with a regular
# expression (e.g., r'-?[0-9]+'), and its action() method for
# processing the match and creating the token.

class TokenMatch:
    def __init__(self, tokname, regexp, /):
        self.tokname = tokname
        self.regexp = regexp

        # fail early, because failing later is very confusing...
        if regexp is not None:
            try:
                _ = re.compile(regexp)
            except re.error:
                raise ValueError(
                    self.__class__.__name__ +
                    f" {tokname}, bad regexp: '{regexp}'") from None

    # NOTATIONAL convenience for subclasses that just need to change
    # the value. They can override this (with a simpler signature)
    # instead of action() if that's all they need to do.
    def _value(self, val, /):
        return val

    # When called from the framework, name is never given, but it is
    # added to this signature as a convenience for subclassing
    def action(self, val, loc, tkz, /, *, name=None):
        return tkz.tokentype(
            tkz.TokenID[name or self.tokname], self._value(val), loc)


class TokenIDOnly(TokenMatch):
    """Put a bare tokname into the TokenID Enum; no regexp"""
    # no *args/**kwargs -  enforce "just a tokname and nothing else"
    def __init__(self, tokname):
        super().__init__(tokname, None)

    def action(self, *args, **kwargs):
        assert False, "This method is supposed to be unreachable"


class TokenMatchIgnore(TokenMatch):
    """TokenMatch that eats tokens (i.e., matches and ignores them)."""

    def action(self, *args, **kwargs):
        """Cause this token to be ignored."""
        return None


class TokenMatchConvert(TokenMatch):
    """Type-convert the value field from string."""

    def __init__(self, *args, converter=int, **kwargs):
        """A TokenMatch that applies a converter() function to the value.
             converter:    will be applied to convert .value
        """
        super().__init__(*args, **kwargs)
        self._value = converter         # check out this awesome hack


class TokenMatchInt(TokenMatchConvert):
    """Converts values to integers. Special case of TokenMatchConvert."""

    def __init__(self, *args):      # exists solely to enforce no kwargs
        super().__init__(*args)


class TokenMatchKeyword(TokenMatch):
    """For keywords. Just specify the keyword no regexp.
       Example:

            TokenMatchKeyword('if')

       is equivalent to:

            TokenMatch('IF', r'(if)(?![a-zA-Z0-9_])')
    """
    def __init__(self, tokname, regexp=None, *args, **kwargs):
        if regexp is None:
            regexp = self.keyword_regexp(tokname)
        super().__init__(tokname.upper(), regexp, *args, **kwargs)

    # broken out so can be overridden if application has other syntax
    def keyword_regexp(self, tokname):
        return f"({tokname})(?![a-zA-Z0-9_])"


class TokenMatchIgnoreButKeep(TokenMatch):
    def __init__(self, *args, keep, **kwargs):
        super().__init__(*args, **kwargs)
        self.keep = keep

    def action(self, val, loc, tkz, /):
        if self.keep in val:
            return super().action(self.keep, loc, tkz)
        else:
            return None


class TokenMatchRuleSwitch(TokenMatch):
    def __init__(self, *args, rulename="_next", **kwargs):
        super().__init__(*args, **kwargs)
        self.rulename = rulename

    def action(self, val, loc, tkz, /):
        tkz.activate_ruleset(self.rulename)
        return super().action(val, loc, tkz)


# A TokLoc describes a source location; for error reporting purposes.
#    s          -- Entire string (i.e., typically an input line)
#    sourcename -- Name of the input source as was given to the
#                  tokenizer (when created/invoked). CAN BE None.
#    lineno     -- Line number, counted from the start lineno given
#                  to the tokenizer. CAN BE None (means no line numbers)
#    startpos   -- Index with s where the error occurred.
#    endpos     -- ONE PAST the end of the error (i.e., the "next" position)
#
TokLoc = namedtuple('TokLoc',
                    ['s', 'sourcename', 'lineno', 'startpos', 'endpos'],
                    defaults=["", "unknown", None, 0, 0])


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):

        def test1(self):
            rules = [
                TokenMatchIgnoreButKeep('NEWLINE', r'\s+', keep='\n'),
                TokenMatch('IDENTIFIER', r'[A-Za-z_][A-Za-z_0-9]*'),
                TokenMatchInt('CONSTANT', r'-?[0-9]+'),
            ]

            s = "    abc123 def  \n\n  ghi_jkl     123456\n"
            tkz = Tokenizer(rules, [s])
            expected_IDvals = [
                (tkz.TokenID.IDENTIFIER, 'abc123'),
                (tkz.TokenID.IDENTIFIER, 'def'),
                (tkz.TokenID.NEWLINE, '\n'),
                (tkz.TokenID.IDENTIFIER, 'ghi_jkl'),
                (tkz.TokenID.CONSTANT, 123456),
                (tkz.TokenID.NEWLINE, '\n')
            ]

            for x, t in zip(expected_IDvals, tkz.tokens()):
                id, val = x
                self.assertEqual(t.id, id)
                self.assertEqual(t.value, val)

        def test_iter(self):
            rules = [TokenMatch('A', 'a'),
                     TokenMatch('B', 'b')]
            tkz = Tokenizer(rules, ["ab", "ba"])
            expected = [
                tkz.TokenID.A,
                tkz.TokenID.B,
                tkz.TokenID.B,
                tkz.TokenID.A,
            ]

            for id, t in zip(expected, tkz):
                self.assertEqual(id, t.id)

        def test_lines(self):
            rules = [TokenMatch('A', 'a'),
                     TokenMatch('B', 'b'),
                     TokenMatch('C', 'c')]
            tkz = Tokenizer(rules, ["aba", "cab"], )
            expected = [
                (tkz.TokenID.A, 1, 0),
                (tkz.TokenID.B, 1, 1),
                (tkz.TokenID.A, 1, 2),
                (tkz.TokenID.C, 2, 0),
                (tkz.TokenID.A, 2, 1),
                (tkz.TokenID.B, 2, 2),
            ]
            for x, t in zip(expected, tkz):
                tokid, lineno, startpos = x
                self.assertEqual(tokid, t.id)
                self.assertEqual(t.location.lineno, lineno)
                self.assertEqual(t.location.startpos, startpos)
                # just knows each test is 1 char
                self.assertEqual(t.location.endpos, startpos+1)

        def test_nomatch(self):
            rules = [TokenMatch('A', 'a'),
                     TokenMatch('B', 'b')]
            lines = ["ab", "baxb"]
            tkz = Tokenizer(rules, lines, startnum=0)
            expected = [
                tkz.TokenID.A,
                tkz.TokenID.B,
                tkz.TokenID.B,
                tkz.TokenID.A,
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
            rules = [
                # just a few other lexical elements thrown in for example
                TokenMatch('LBRACE', r'{'),
                TokenMatch('RBRACE', r'}'),
                TokenMatch('IDENTIFIER', r'[A-Za-z_][A-Za-z_0-9]*'),
                TokenMatchRuleSwitch('COMMENT_START', r'/\*'),
                TokenMatch('BAD', r'.'),
            ]

            altrules = [
                # eat everything that is not a star
                TokenMatchIgnore('C_NOTSTAR', r'[^*]+'),

                # */ ends the comment and returns to regular rules
                TokenMatchRuleSwitch('COMMENT_END', r'\*/'),

                # when a star is seen that isn't */ this eats it
                TokenMatchIgnore('C_STAR', r'\*'),
            ]

            for sx, expected in (
                    (["abc/*", "def*/"],
                     ['IDENTIFIER', 'COMMENT_START', 'COMMENT_END']),
                     ):
                foo = """
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
                """
                tkz = Tokenizer({None: rules, 'ALT': altrules}, sx)
                toks = list(tkz.tokens())
                with self.subTest(sx=sx):
                    for name, t in zip(expected, toks):
                        self.assertEqual(tkz.TokenID[name], t.id)

        # check that duplicated toknames are allowed
        def test_dups(self):
            rules = [TokenMatch('FOO', 'f'),
                     TokenMatch('BAR', 'b'),
                     TokenMatch('FOO', 'zzz')]
            tkz = Tokenizer(rules)

            expected = (('FOO', 'f'), ('BAR', 'b'), ('FOO', 'zzz'))
            for token, ex in zip(tkz.string_to_tokens('fbzzz'), expected):
                self.assertEqual(token.id, tkz.TokenID[ex[0]])
                self.assertEqual(token.value, ex[1])

        # Test naked tokenIDs (no regexp)
        def test_tokIDonly(self):
            rules = [
                TokenMatch('CONSTANT', r'-?[0-9]+'),
                TokenMatch('FOO', None),
                TokenIDOnly('BAR')
            ]
            tkz = Tokenizer(rules)
            self.assertTrue(hasattr(tkz.TokenID, 'FOO'))
            self.assertTrue(hasattr(tkz.TokenID, 'BAR'))
            self.assertTrue(hasattr(tkz.TokenID, 'CONSTANT'))

        # Example of multiple rule sets from README
        def test_ruleswitch(self):

            group1 = [
                TokenMatch('ZEE', r'z'),
                TokenMatchRuleSwitch('ALTRULES', r'/@/', rulename='ALT')
            ]

            group2 = [
                TokenMatch('ZED', r'z'),
                TokenMatchRuleSwitch('MAINRULES', r'/@/', rulename=None)
            ]

            rules = {None: group1, 'ALT': group2}
            tkz = Tokenizer(rules)
            expected = (
                tkz.TokenID.ZEE,
                tkz.TokenID.ZEE,
                tkz.TokenID.ALTRULES,
                tkz.TokenID.ZED,
                tkz.TokenID.MAINRULES,
                tkz.TokenID.ZEE,
            )

            for token, ex in zip(tkz.string_to_tokens('zz/@/z/@/z'), expected):
                self.assertEqual(token.id, ex)
                if ex in (tkz.TokenID.ZEE, tkz.TokenID.ZED):
                    self.assertEqual(token.value, 'z')
                elif ex in (tkz.TokenID.ALTRULES,
                            tkz.TokenID.MAINRULES):
                    self.assertEqual(token.value, '/@/')
                else:
                    self.assertTrue(False)

        # test the simplest, most-direct way to make a different Token()
        def test_factory_1(self):
            class MyToken:
                def __init__(self, id, value, location, /):
                    self.id = id
                    self.value = value
                    self.location = location
            rules = [
                TokenMatch('A', 'a'),
            ]
            tkz = Tokenizer(rules, tokentype=MyToken)
            self.assertTrue(all(isinstance(t, MyToken)
                                for t in tkz.string_to_tokens('a')))

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
                def action(self, val, loc, tkz, /):
                    tokid = tkz.TokenID[self.tokname]
                    return MyToken_1(tokid, val, loc)

            class TokenMatch_2(TokenMatch):
                def action(self, val, loc, tkz, /):
                    tokid = tkz.TokenID[self.tokname]
                    return MyToken_2(tokid, val, loc)

            rules = [
                TokenMatch('NATIVE', '0'),
                TokenMatch_1('_1', '1'),
                TokenMatch_2('_2', '2'),
            ]

            expected = [Token, MyToken_1, MyToken_2, Token]
            tkz = Tokenizer(rules)
            classes = [t.__class__ for t in tkz.string_to_tokens('0120')]
            self.assertEqual(classes, expected)

        # this example shows how to pass additional arguments to a Token()
        # They are specified when the MyTokenMatch object is instantiated in
        # the rules setup.

        def test_factory_3(self):
            class MyToken:
                # NOTE how this takes an extra argument, and how it
                #      was supplied by the MyTokenMatch
                def __init__(self, tokid, value, location, custom_arg):
                    self.id = tokid
                    self.value = value
                    self.location = location
                    self.custom_arg = custom_arg

            class MyTokenMatch(TokenMatch):
                def __init__(self, *args, custom_arg, **kwargs):
                    super().__init__(*args, **kwargs)
                    self.custom_arg = custom_arg

                def action(self, val, loc, tkz, /):
                    tokid = tkz.TokenID[self.tokname]
                    return MyToken(tokid, val, loc, self.custom_arg)

            # Ok, now put that all together...
            rules = [
                MyTokenMatch('FOO', '1', custom_arg='bozo'),
                MyTokenMatch('BAR', '2', custom_arg='krusty')
            ]

            expected = ['bozo', 'krusty', 'bozo']
            tkz = Tokenizer(rules)
            customs = [t.custom_arg for t in tkz.string_to_tokens('121')]
            self.assertEqual(customs, expected)

    unittest.main()
