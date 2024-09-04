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
#    TokenMatch        Object encapsulating a basic regexp rule.
#    ... subclasses    Various subclasses of TokenMatch for special functions
#
#    TokenRuleSuite    Collection of TokenMatch objects, optionally
#                      grouped into named context-dependent rulesets.
#    Tokenizer         The engine driven by a TokenRuleSuite.
#
#    Token             The Tokenizer produces these.
#    TokenID           An Enum type dynamically created by the TokenRuleSuite
#                      from all of the TokenMatch specifications; this is the
#                      type of each individual Token (i.e., what it matched)


# basic information about a regexp match
MatchedInfo = namedtuple('MatchedInfo', ['tokname', 'value', 'start', 'stop'])


# A TokenMatch combines a name (e.g., 'CONSTANT') with a regular
# expression (e.g., r'-?[0-9]+'), and its matched() method is part of
# how subclasses can extend functionality (see docs or read examples below).

class TokenMatch:
    def __init__(self, tokname, regexp, /):
        self.tokname = tokname
        self.regexp = regexp

    # Called when the regexp matches an input. This is where
    # token-specific post processing can happen (by subclasses
    # overriding). 'minfo' is a MatchedInfo and the matched()
    # method can alter it accordingly; in this base implementation
    # it is just returned unchanged.
    def matched(self, minfo, /, *, rulesuite):
        """Return a MatchedInfo based on the one given."""
        return minfo


class TokenMatchIgnore(TokenMatch):
    """TokenMatch that eats tokens (i.e., matches and ignores them)."""

    def matched(self, minfo, /, *, rulesuite):
        """Cause this token to be ignored."""

        return minfo._replace(tokname=None)  # tokname=None means "ignore"


class TokenMatchConvert(TokenMatch):
    """Type-convert the value field from string."""

    # see the README for a discussion of why alt_tokname is sometimes useful
    def __init__(self, *args, converter=int, alt_tokname=None, **kwargs):
        """A TokenMatch that applies a converter() function to the value.

        Keyword arguments:
             converter:    will be applied to convert .value
             alt_tokname:  if specified, changes the tokname (see README
                           for discussion of why this can be useful)
        """
        super().__init__(*args, **kwargs)
        self.converter = converter
        self.alt_tokname = alt_tokname

    def matched(self, minfo, /, *, rulesuite):
        """Convert the value in this token (from string)."""
        replacements = {'value': self.converter(minfo.value)}
        if self.alt_tokname is not None:
            replacements['tokname'] = self.alt_tokname
        return minfo._replace(**replacements)


class TokenMatchInt(TokenMatchConvert):
    """Just for clarity; equivalent to TokenMatchConvert w/no kwargs."""

    def __init__(self, *args):
        super().__init__(*args)


class TokenMatchIgnoreWhiteSpaceKeepNewline(TokenMatch):
    def matched(self, minfo, /, *, rulesuite):
        if '\n' in minfo.value:
            return minfo._replace(value="\n")
        else:
            return minfo._replace(tokname=None)


class TokenMatchRuleSwitch(TokenMatch):
    def __init__(self, *args, new_rulename=None, **kwargs):
        super().__init__(*args, **kwargs)
        self.new_rulename = new_rulename

    def matched(self, minfo, /, *, rulesuite):
        # Switch to the new ruleset; note that None means switch to
        # the "next" ruleset (wrapping around from last to first). The None
        # style is particularly convenient for swapping back and forth
        # between a main ruleset and one alternate (if there are only two)
        if self.new_rulename is None:
            rulenames = list(rulesuite.rules)
            current = rulenames.index(rulesuite.active_rulename)
            nextindex = (current + 1) % len(rulenames)
            rulesuite.active_rulename = rulenames[nextindex]
        else:
            rulesuite.active_rulename = self.new_rulename
        return minfo


# A TokLoc is for error reporting, it describes the location in the
# source stream a given Token was matched.
TokLoc = namedtuple('TokLoc',
                    ['source', 'line', 'start', 'end'],
                    defaults=["unknown", None, 0, 0])


# A Token has a TokenID, a value (initially: the string match, but the
# post-processing function can alter that), an "origin" which is the full
# string (usually an entire line) containing the token and a TokLoc for
# more details on match location.
Token = namedtuple('Token',
                   ['id', 'value', 'origin', 'location'],
                   defaults=[None, None, TokLoc()])


# A TokenRuleSuite is a collection of TokenMatch objects that will be
# used to create/drive a Tokenizer.
#
# In the simplest form, a TokenRuleSuite is initialized from a sequence
# of TokenMatch objects. They form the total set of match rules.
# This is also where the TokenID Enum is created, from the union of
# token names in all of the match rules in the suite.
#
# To handle context-dependent tokenizing rules, a suite can have multiple
# named groups of rules, and processing functions can switch among them.
# In that case the TokenRuleSuite is initialized from a dictionary
# of lists of TokenMatch objects.
#

class TokenRuleSuite:
    def __init__(self, tms, /, *, tokenIDs=None):
        """Collect TokenMatch objects together for a tokenizer.

        Argument is either a sequence (e.g., list) of TokenMatch

        objects (simplest case) or a mapping (e.g., dict) of sequences
        of TokenMatch objects (modal/context-dependent case).

        If specifying a mapping, note that None must be a valid key.
        It denotes the default/initial TokenMatch sequence.
        All other key names can be arbitrary.
        """

        # Internally, always use a mapping so convert if needed.
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

        # rule sets cannot be zero-length
        if any([len(x) == 0 for x in tmsmap.values()]):
            raise ValueError("zero-length ruleset(s)")

        # In the standard usage scenarios, tokenIDs should be left None
        # and this code will create the Enum for token IDs automatically:
        if tokenIDs is None:
            # collect all the tokennames from all the TokenMatch objects
            # NOTE: weed out duplicates (using set()); dups are allowable
            #       when there are multiple context-dependent TokenMatch lists
            toknames = set(r.tokname for mx in tmsmap.values() for r in mx)
            self.TokenID = Enum('TokenID', sorted(toknames))
        else:
            # for some reason caller desired to supply the TokenID mapping.
            # It doesn't actually have to be an Enum, but it does have
            # to be a mapping that will accept any tokname from any
            # of the supplied TokenMatch objects.
            self.TokenID = tokenIDs

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

        self.active_rulename = None          # default rule 'name' is: None

    def run(self, s):
        """Run the TokenRules on string 's', yield MatchInfo's."""

        rules = None
        so_far = 0

        # Note that TokenMatch objects can cause a rulename change
        # so the loop is written this way to accommodate that.
        while True:
            if self.rules[self.active_rulename] != rules:
                rules = self.rules[self.active_rulename]
                g = re.finditer(rules.rx, s[so_far:])
                baseoffset = so_far
            try:
                mobj = next(g)
            except StopIteration:
                break

            so_far = mobj.end() + baseoffset   # end of processed chars
            minfo = MatchedInfo(
                tokname=mobj.lastgroup,
                value=mobj.group(0),
                start=mobj.start()+baseoffset,
                stop=so_far)

            yield rules.name2tms[minfo.tokname].matched(minfo, rulesuite=self)


class Tokenizer:
    """Break streams into tokens with rules from regexps and callbacks."""

    _NOTGIVEN = object()

    def __init__(self, trs, strings=None, /, *,
                 srcname=None, startnum=_NOTGIVEN):
        """Set up a Tokenizer; see tokens() to generate tokens.

        Arguments:
           trs      -- A TokenRuleSuite object

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
        """

        # This is probably being too lenient, but: allow trs to be a direct
        # list of TokenMatch objects (instead of a TokenRuleSuite) and convert
        # it accordingly if so. 'TokenID' attribute and 'run' method are
        # the duck-typing requirement for the TokenRuleSuite.
        if not (hasattr(trs, 'TokenID') and hasattr(trs, 'run')):
            try:
                trs = TokenRuleSuite(trs)
            except (AttributeError, TypeError):
                msg = "trs not a TokenRuleSuite and can't be converted"
                raise TypeError(msg) from None

        self.strings = strings
        self.trs = trs
        if startnum is self._NOTGIVEN:
            startnum = 1
        self.startnum = startnum
        self.srcname = srcname

    # convenience for use in case where strings that end with two
    # character sequence "backslash newline" should be combined with
    # the newline (and backslash) elided. Example of use:
    #
    #     trs = TokenRuleSuite(blah blah)
    #     f = open('foo.c', 'r')
    #     tz = Tokenizer(trs, Tokenizer.linefilter(f))
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
        for minfo in self.trs.run(s):
            # make sure this match starts at the next expected character
            if minfo.start != expected_next_pos:
                break
            expected_next_pos = minfo.stop
            try:
                id = self.trs.TokenID[minfo.tokname]
            except KeyError:
                pass
            else:
                loc = TokLoc(name, linenumber, minfo.start, minfo.stop)
                yield Token(id, minfo.value, s, loc)

        # If the expected_next_pos here is not the end of s then there
        # was something not matched along the way (or at the end)
        if expected_next_pos != len(s):
            raise ValueError(f"unmatched @{expected_next_pos}, {s=}")


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):

        def test1(self):
            # example adapted from README.md on github
            rules = [
                TokenMatchIgnoreWhiteSpaceKeepNewline('NEWLINE', r'\s+'),
                TokenMatch('IDENTIFIER', r'[A-Za-z_][A-Za-z_0-9]*'),
                TokenMatchInt('CONSTANT', r'-?[0-9]+'),
            ]

            rule_suite = TokenRuleSuite(rules)

            s = "    abc123 def    ghi_jkl     123456\n"
            tkz = Tokenizer(rule_suite, [s])
            expected_IDvals = [
                (rule_suite.TokenID.IDENTIFIER, 'abc123'),
                (rule_suite.TokenID.IDENTIFIER, 'def'),
                (rule_suite.TokenID.IDENTIFIER, 'ghi_jkl'),
                (rule_suite.TokenID.CONSTANT, 123456),
                (rule_suite.TokenID.NEWLINE, '\n')
            ]

            for x, t in zip(expected_IDvals, tkz.tokens()):
                id, val = x
                self.assertEqual(t.id, id)
                self.assertEqual(t.value, val)

        def test_iter(self):
            rules = [TokenMatch('A', 'a'),
                     TokenMatch('B', 'b')]
            rule_suite = TokenRuleSuite(rules)
            tkz = Tokenizer(rule_suite, ["ab", "ba"])
            expected = [
                rule_suite.TokenID.A,
                rule_suite.TokenID.B,
                rule_suite.TokenID.B,
                rule_suite.TokenID.A,
            ]

            for id, t in zip(expected, Tokenizer(rule_suite, ["ab", "ba"])):
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

            trs = TokenRuleSuite(
                {None: rules, 'ALT': altrules})

            for sx, expected in (
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
                tkz = Tokenizer(trs, sx)
                toks = list(tkz.tokens())
                with self.subTest(sx=sx):
                    for name, t in zip(expected, toks):
                        self.assertEqual(trs.TokenID[name], t.id)

        # Example of multiple rule sets from README
        def test_ruleswitch(self):

            switcher = TokenMatchRuleSwitch('SWITCH', r'/')
            group1 = [
                TokenMatch('ZEE', r'z'),
                TokenMatchRuleSwitch('ALTRULES', r'/', new_rulename='ALT')
            ]

            group2 = [
                TokenMatch('ZED', r'z'),
                TokenMatchRuleSwitch('MAINRULES', r'/', new_rulename=None)
            ]

            rules = {None: group1, 'ALT': group2}
            rule_suite = TokenRuleSuite(rules)
            expected = (
                rule_suite.TokenID.ZEE,
                rule_suite.TokenID.ZEE,
                rule_suite.TokenID.ALTRULES,
                rule_suite.TokenID.ZED,
                rule_suite.TokenID.MAINRULES,
                rule_suite.TokenID.ZEE,
            )

            tkz = Tokenizer(rule_suite, None)
            for token, ex in zip(tkz.string_to_tokens('zz/z/z'), expected):
                self.assertEqual(token.id, ex)
                if ex in (rule_suite.TokenID.ZEE, rule_suite.TokenID.ZED):
                    self.assertEqual(token.value, 'z')
                elif ex in (rule_suite.TokenID.ALTRULES,
                            rule_suite.TokenID.MAINRULES):
                    self.assertEqual(token.value, '/')
                else:
                    self.assertTrue(False)

    unittest.main()
