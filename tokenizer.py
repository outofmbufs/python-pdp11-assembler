# A generic tokenizer driven by regular expressions

from collections import namedtuple
from enum import Enum
import itertools
import re

# Regular-expression line-oriented tokenizer.
#
# This is a thin layer on top of using re.match or re.finditer directly,
# and so is useful for lexing when a line-by-line/regexp model works.
#
# The basics are:
#    TokenMatch        Object encapsulating a basic regexp rule.
#    TokenRuleSuite    Collection of TokenMatch objects, optionally
#                      grouped into named context-dependent subsets.
#    Tokenizer         The engine driven by a TokenRuleSuite.
#
#    Token             The Tokenizer produces these.
#    TokenID           An Enum type dynamically created by the TokenRuleSuite
#                      from all of the TokenMatch specifications; this is the
#                      type of each individual Token (i.e., what it matched)
#


# A TokenMatch is a name (which will become a TokenID), a regular expression,
# and an optional post-processor function. NOTE: There are token "names" and
# TokenID Enums. Generally the TokenID Enums are used as much as possible;
# however, the TokenID type is dynamically created from examining all the
# TokenMatch objects so here of course they are names (str) not TokenIDs.
TokenMatch = namedtuple('TokenMatch',
                        ['tokname', 'regexp', 'ppf'],
                        defaults=[None])


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
    DEFAULT_NAME = '*'
    ALT_NAME = 'ALT'

    def __init__(self, tms, /, *, tokenIDs=None):
        """Collect TokenMatch objects together for a tokenizer.

        Argument is either a sequence (e.g., list) of TokenMatch
        objects (simplest case) or a mapping (e.g., dict) of sequences
        of TokenMatch objects (modal/context-dependent case).

        If specifying a mapping, note that DEFAULT_NAME ('*') must be a
        valid key. It denotes the default/initial TokenMatch sequence.
        All other key names can be arbitrary.
        """

        # Internally, always use a mapping so convert if needed.
        try:
            _ = tms[self.DEFAULT_NAME]        # test it for dict-ness
        except KeyError:
            msg = f"TokenMatch mapping has no '{self.DEFAULT_NAME}' entry"
            raise ValueError(msg) from None
        except TypeError:
            # it wasn't a dict so make it one.
            tms = {self.DEFAULT_NAME: tms}

        # In the standard usage scenarios, tokenIDs should be left None
        # and this code will create the Enum for token IDs automatically:
        if tokenIDs is None:
            # collect all the tokennames from all the TokenMatch objects
            # NOTE: weed out duplicates (using set()); dups are allowable
            #       when there are multiple context-dependent TokenMatch lists
            toknames = set(r.tokname for mx in tms.values() for r in mx)
            self.TokenID = Enum('TokenID', sorted(toknames))
        else:
            # for some reason caller desired to supply the TokenID mapping.
            # It doesn't actually have to be an Enum, but it does have
            # to be a mapping that will accept any tokname from any
            # of the supplied TokenMatch objects.
            self.TokenID = tokenIDs

        # each named rule set needs its own pre-computed re, ppf, etc.
        RuleInfo = namedtuple('RuleInfo', ['rx', 'ppfmap'])

        self.rules = {
            k: RuleInfo('|'.join(f'(?P<{r.tokname}>{r.regexp})'
                                 for r in mx if r.regexp is not None),
                        {r.tokname: r.ppf
                         for r in mx if r.ppf is not None})
            for k, mx in tms.items()
        }

        # After all that:
        #   self.rules is a dictionary, indexed by names from the tms keys.
        #   Each entry is a RuleInfo that has:
        #         rx: big joined regexp for those TokenMatch objects
        #     ppfmap: ppf functions for Match objects that have them

        self.activate()        # start with the default/initial match rules

    def activate(self, name=None):
        self.activerules = self.rules[name or self.DEFAULT_NAME]

    # There is optional post processing before an re match becomes
    # a token. The processing function is invoked like this:
    #
    #     newtokID, newval = ppf(trs, tokID, val)
    #
    # where:
    #  'ppf' is the post-processing function,
    #  'trs' is the TokenRuleSuite object
    #  'tokID' and 'val' are the tokenID and value from the match.
    # The returned (newtokID, newval) are used to create the Token;
    # if newtokID is None then no Token is created (useful, for example,
    # for ignoring certain types of tokens, perhaps whitespace for example)
    #
    # The ppf is permitted to invoke trs.activate() to switch the active
    # match rules in the Tokenizer. See the unittests for an example of
    # how (e.g.) C-style '/* ..... */' multi-line comments can use this.

    def run(self, s):
        """Run the TokenRules on string 's'

        Generate a sequence of tuples:
               (tokID, val, start, stop)
        where:
                tokID -- TokenID from rule/ppf
                  val -- value from rule/ppf
                start -- position in 's' of match start
                 stop -- end position in 's'

        Note that the ppf can modify value, so stop-start+1 might not
        equal len(val) (val might not even be a string)
        """

        for mobj in re.finditer(self.activerules.rx, s):

            # 'lastgroup' means 'outmost' here. Most of the time there is
            # only one named matching group anyway, but conceptually there
            # could be others if the rules contain their own such names.
            tokname = mobj.lastgroup
            ppf = self.activerules.ppfmap.get(tokname, lambda trs, *a: a)
            tokID, val = ppf(self, self.TokenID[tokname], mobj.group(0))
            yield tokID, val, mobj.start(), mobj.end()

    # SOME BUILT-IN PPF FUNCTIONS FOR CONVENIENCE:
    #   ppf_keepnewline: Helper for using \s for WHITESPACE while
    #      still keeping \n visible as a separate token. Useful for
    #      line-oriented grammars that otherwise ignore white space.
    #
    #   ppf_ignored: "ignore this when seen" ppf.
    #
    #   ppf_int: simply calls "int()" on the value string.
    #
    #   ppf_altrules: "switch in alternate rules" ppf
    #
    #   ppf_mainrules: "go back to main (default) rules" ppf
    #
    # NOTE: Any of the ppf functions that take extra arguments will have
    #       to be curried via functools.partial (or equivalent).

    @staticmethod
    def ppf_keepnewline(trs, id, val, *, name='NEWLINE'):
        if name is None:      # special case means use the attached TokenID
            replacer = id
        else:
            replacer = trs.TokenID[name]
        if '\n' in val:
            return replacer, "\n"
        else:
            return None, None

    @staticmethod
    def ppf_ignored(trs, id, val):
        return None, None

    @staticmethod
    def ppf_int(trs, id, val):
        return id, int(val)

    @staticmethod
    def ppf_altrules(trs, tokID, val, altrules=ALT_NAME):
        """Switch to an alternate set of rules"""
        trs.activate(altrules)
        return tokID, val

    @staticmethod
    def ppf_mainrules(trs, tokID, val):
        trs.activate()
        return tokID, val


class Tokenizer:
    """Break streams into tokens with rules from regexps and callbacks."""

    _NOTGIVEN = object()

    def __init__(self, trs, strings, /, *,
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

        # outer loop for looping over potential rules-changes mid-string.
        rulescheck = -1    # the point being: "is not" self.activerules

        while self.trs.activerules is not rulescheck:
            rulescheck = self.trs.activerules

            next_start = 0    # to catch unmatched characters

            # inner loop goes through the string, match by match
            for tokID, val, start, stop in self.trs.run(s):
                if start != next_start:
                    raise ValueError(f"unmatched @{next_start}, {s=}")
                next_start = stop
                if tokID is not None:         # None means just eat this
                    loc = TokLoc(name, linenumber, start, stop)
                    yield Token(tokID, val, s, loc)

                if self.trs.activerules != rulescheck:
                    # new rules activated by a post-processor; break 's' at
                    # the last token and loop around outer level again
                    s = s[stop:]
                    break


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):

        # C comment example
        def test_C(self):
            rules = [
                # just a few other lexical elements thrown in for example
                TokenMatch('LBRACE', r'{'),
                TokenMatch('RBRACE', r'}'),
                TokenMatch('IDENTIFIER', r'[A-Za-z_][A-Za-z_0-9]*'),
                TokenMatch('COMMENT_START',
                           r'/\*', TokenRuleSuite.ppf_altrules),
                TokenMatch('BAD', r'.'),
            ]

            altrules = [
                # eat everything that is not a star
                TokenMatch('C_NOTSTAR', r'[^*]+', TokenRuleSuite.ppf_ignored),

                # */ ends the comment and returns to regular rules
                TokenMatch(
                    'COMMENT_END', r'\*/', TokenRuleSuite.ppf_mainrules),

                # when a star is seen that isn't */ this eats it
                TokenMatch('C_STAR', r'\*', TokenRuleSuite.ppf_ignored),
            ]

            trs = TokenRuleSuite(
                {TokenRuleSuite.DEFAULT_NAME: rules, 'ALT': altrules})

            for sx, expected in (
                    (["/**/"], ['COMMENT_START', 'COMMENT_END']),
                    (["/***/"], ['COMMENT_START', 'COMMENT_END']),
                    (["/****/"], ['COMMENT_START', 'COMMENT_END']),
                    (["/*****/"], ['COMMENT_START', 'COMMENT_END']),
                    (["/* */"], ['COMMENT_START', 'COMMENT_END']),
                    (["/* * / * */"], ['COMMENT_START', 'COMMENT_END']),
                    (["abc/*", "def*/"],
                     ['IDENTIFIER', 'COMMENT_START', 'COMMENT_END']),
                    ):
                tkz = Tokenizer(trs, sx)
                toks = list(tkz.tokens())
                with self.subTest(sx=sx):
                    for name, t in zip(expected, toks):
                        self.assertEqual(trs.TokenID[name], t.id)

    unittest.main()
