# A generic tokenizer driven by regular expressions

from collections import namedtuple
from enum import Enum
import re

# Regular-expression line-oriented tokenizer.
#
# This is a thin layer on top of using re.match or re.finditer directly,
# and so is useful for lexing when a line-by-line/regexp model works.
#
# The basics are:
#    Tokenizer        Main object. Built from rulesets of TokenMatch objects.
#
#    TokenMatch       Object encapsulating a basic regexp rule.
#    ... subclasses   Various subclasses of TokenMatch for special functions
#
#    Token            The Tokenizer produces these.
#    TokenID          An Enum type dynamically created by the Tokenizer
#                     from all of the TokenMatch specifications; this is the
#                     type of each individual Token (i.e., what it matched)


# A _MatchedInfo is created by the Tokenizer from each regexp match, and
# passed to the corresponding TokenMatch.matched() for further processing.
_MatchedInfo = namedtuple(
    '_MatchedInfo', ['tokname', 'value', 'start', 'stop', 'tokenizer'])


# A TokenMatch combines a name (e.g., 'CONSTANT') with a regular
# expression (e.g., r'-?[0-9]+'), and its matched() method is part of
# how subclasses can extend functionality (see docs or read examples below).

class TokenMatch:
    def __init__(self, tokname, regexp, /):
        self.tokname = tokname
        self.regexp = regexp

    # Token-specific post processing on a match. This is a no-op; subclasses
    # will typically override with token-specific conversions/actions.
    def matched(self, minfo, /):
        """Return a _MatchedInfo based on the one given."""
        return minfo


class TokenMatchIgnore(TokenMatch):
    """TokenMatch that eats tokens (i.e., matches and ignores them)."""

    def matched(self, minfo, /):
        """Cause this token to be ignored."""
        return minfo._replace(tokname=None)  # tokname=None means "ignore"


class TokenMatchConvert(TokenMatch):
    """Type-convert the value field from string."""

    # see the README for a discussion of why alt_tokname is sometimes useful
    def __init__(self, *args, converter=int, alt_tokname=None, **kwargs):
        """A TokenMatch that applies a converter() function to the value.

        Keyword arguments:
             converter:    will be applied to convert .value
             alt_tokname:  if specified, changes the tokname (see README)
        """
        super().__init__(*args, **kwargs)
        self.converter = converter
        self.alt_tokname = alt_tokname

    def matched(self, minfo, /):
        """Convert the value in this token (from string)."""
        return minfo._replace(
            value=self.converter(minfo.value),
            tokname=self.alt_tokname or minfo.tokname)


class TokenMatchInt(TokenMatchConvert):
    """Just for clarity; equivalent to TokenMatchConvert w/no kwargs."""

    def __init__(self, *args):
        super().__init__(*args)


class TokenMatchKeyword(TokenMatch):
    """For keywords. Just specify the keyword no regexp.
       Example:

            TokenMatchKeyword('if')

       is equivalent to:

            TokenMatch('IF', r'(if)[^a-zA-Z_0-9]*')
    """
    def __init__(self, tokname, regexp=None, *args, **kwargs):
        if regexp is None:
            regexp = self.keyword_regexp(tokname)
        super().__init__(tokname.upper(), regexp, *args, **kwargs)

    # broken out so can be overridden if application has other syntax
    def keyword_regexp(self, tokname):
        return f"({tokname})(?![a-zA-Z0-9_])"


class TokenMatchIgnoreButKeep(TokenMatch):
    def __init__(self, tokname, regexp, *args, keep, **kwargs):
        super().__init__(tokname, regexp, *args, **kwargs)
        self.keep = keep

    def matched(self, minfo, /):
        if self.keep in minfo.value:
            return minfo._replace(value=self.keep)
        else:
            return minfo._replace(tokname=None)


class TokenMatchRuleSwitch(TokenMatch):
    def __init__(self, *args, new_rulename="_next", **kwargs):
        super().__init__(*args, **kwargs)
        self.new_rulename = new_rulename

    def matched(self, minfo, /):
        minfo.tokenizer.activate_ruleset(self.new_rulename)
        return minfo


# A TokLoc is for error reporting, it describes the location in the
# source stream a given Token was matched.
TokLoc = namedtuple('TokLoc',
                    ['source', 'line', 'start', 'end'],
                    defaults=["unknown", None, 0, 0])


# A Token has a TokenID, a value, an "origin" which is the full string
# (usually an entire line) containing the token and a TokLoc for more
# details on match location.
Token = namedtuple('Token',
                   ['id', 'value', 'origin', 'location'],
                   defaults=[None, None, TokLoc()])


class Tokenizer:
    """Break streams into tokens with rules from regexps."""

    _NOTGIVEN = object()

    def __init__(self, tms, strings=None, /, *,
                 srcname=None, startnum=_NOTGIVEN,
                 tokenIDs=None):
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
                       This eventually goes into the TokLocs as "source"

           startnum -- for error reporting, but otherwise ignored.
                       Each string in strings is assumed to be a separate
                       line and will be numbered starting from this number.
                       Default is 1. If None, line numbers left out.

           tokenIDs -- If caller doesn't want the TokenID Enum created
                       automatically, it can be provided here.
        """

        tmsmap = self.__tmscvt(tms)
        self.TokenID = tokenIDs or self.create_tokenID_enum(tmsmap)

        # each named rule gets a RuleInfo containing:
        #      rx        - the fully-joined regexp
        #      name2tms  - map a TokenMatch name back to the TokenMatch
        RuleInfo = namedtuple('RuleInfo', ['rx', 'name2tms'])

        # Make a mapping from each rule name to its RuleInfo
        self.rules = {
            k: RuleInfo('|'.join(f'(?P<{r.tokname}>{r.regexp})'
                                 for r in tms if r.regexp is not None),
                        {tm.tokname: tm for tm in tms})
            for k, tms in tmsmap.items()
        }

        self.strings = strings
        self.active_rulesname = None
        if startnum is self._NOTGIVEN:
            startnum = 1
        self.startnum = startnum
        self.srcname = srcname

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

        # No rule set may be zero-length
        if any([len(x) == 0 for x in tmsmap.values()]):
            raise ValueError("zero-length ruleset(s)")

        # duplicate names in any single group of TokenMatch objects can cause
        # truly head-scratching bugs so check for that.
        for rulesetname, tms in tmsmap.items():
            names = set()
            for tm in tms:
                if tm.tokname in names:
                    raise ValueError(f"Duplicate tokname: {tm.tokname}")
                names.add(tm.tokname)
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

    def activate_ruleset(self, newrule):
        if newrule == "_next":      # means 'go to next rule'
            rulenames = list(self.rules)
            current = rulenames.index(self.active_rulesname)
            nextindex = (current + 1) % len(rulenames)
            self.active_rulesname = rulenames[nextindex]
        else:
            self.active_rulesname = newrule

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

    # Iterating over a Tokenizer is the same as iterating over
    # the tokens() method, but without the ability to specify other args.
    def __iter__(self):
        return self.tokens()

    def tokens(self, strings=None, /, *,
               srcname=_NOTGIVEN, startnum=_NOTGIVEN):
        """GENERATE tokens for the entire file.

        strings/srcname/startnum arguments same as for __init__().
        """
        if strings is not None:
            self.strings = strings
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
        """GENERATE tokens from a string.

        Optional keyword argument linenumber will be put into error messages.
        """

        expected_next_pos = 0    # to catch unmatched characters
        for minfo in self.run(s):
            # make sure this match starts at the next expected character
            if minfo.start != expected_next_pos:
                break
            expected_next_pos = minfo.stop
            try:
                id = self.TokenID[minfo.tokname]
            except KeyError:
                pass
            else:
                loc = TokLoc(name, linenumber, minfo.start, minfo.stop)
                yield Token(id, minfo.value, s, loc)

        # If the expected_next_pos here is not the end of s then there
        # was something not matched along the way (or at the end)
        if expected_next_pos != len(s):
            raise ValueError(f"unmatched @{expected_next_pos}, {s=}")

    def run(self, s):
        """Run the TokenRules on string 's', yield MatchInfo's."""

        so_far = 0
        rules = None

        # Note that TokenMatch objects can cause active_rulesname to change
        # so the loop is written this way to accommodate that.
        while True:
            if rules is not self.rules[self.active_rulesname]:
                rules = self.rules[self.active_rulesname]
                g = re.finditer(rules.rx, s[so_far:])
                baseoffset = so_far
            try:
                mobj = next(g)
            except StopIteration:
                break

            so_far = mobj.end() + baseoffset   # end of processed chars
            minfo = _MatchedInfo(
                tokname=mobj.lastgroup,
                value=mobj.group(0),
                start=mobj.start()+baseoffset,
                stop=so_far,
                tokenizer=self)

            yield rules.name2tms[minfo.tokname].matched(minfo)


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):

        def test1(self):
            rules = [
                TokenMatchIgnoreButKeep('NEWLINE', r'\s+', keep='\n'),
                TokenMatch('IDENTIFIER', r'[A-Za-z_][A-Za-z_0-9]*'),
                TokenMatchInt('CONSTANT', r'-?[0-9]+'),
            ]

            s = "    abc123 def    ghi_jkl     123456\n"
            tkz = Tokenizer(rules, [s])
            expected_IDvals = [
                (tkz.TokenID.IDENTIFIER, 'abc123'),
                (tkz.TokenID.IDENTIFIER, 'def'),
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

        # check that duplicated toknames are not allowed
        def test_dups(self):
            rules = [TokenMatch('FOO', 'f'),
                     TokenMatch('BAR', 'b'),
                     TokenMatch('FOO', 'zzz')]
            with self.assertRaises(ValueError):
                tkz = Tokenizer(rules)

        # Example of multiple rule sets from README
        def test_ruleswitch(self):

            group1 = [
                TokenMatch('ZEE', r'z'),
                TokenMatchRuleSwitch('ALTRULES', r'/@/', new_rulename='ALT')
            ]

            group2 = [
                TokenMatch('ZED', r'z'),
                TokenMatchRuleSwitch('MAINRULES', r'/@/', new_rulename=None)
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

    unittest.main()
