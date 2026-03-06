# A generic tokenizer driven by regular expressions

from dataclasses import dataclass, field, KW_ONLY
from enum import Enum
import typing
import re

# Regular-expression line-oriented tokenizer.
#
# This is a thin layer on top of using re.match or re.finditer directly,
# and so is useful for lexing when a line-by-line/regexp model works.
#
# The basics:
#    Tokenizer        Main object. Built from a TokenRules object
#
#    TokenRules       In simple applications this is built from one or more
#                     TokenMatch objects representing the lexical rules.
#                     In advanced applications NamedRuleSet objects will be
#                     used to build the TokenRules, allowing for different
#                     lexical rules to switch in/out dynamically.
#
#    NamedRuleSet     A sequence of TokenMatch objects gathered together
#                     and selectable by a given name. Advanced applications
#                     can use multiple NamedRuleSets in a single TokenRules.
#                     Simple applications don't even have to know there
#                     is such a thing as a NamedRuleSet.
#
#    TokenMatch       Object encapsulating a basic regexp rule.
#    ... subclasses   Various subclasses of TokenMatch for special functions
#
#    Token            The Tokenizer produces these.
#
#    TokenID          An Enum dynamically created when TokenRules are
#                     built from TokenMatch objects. Every Token has a
#                     TokenID denoting its type (CONSTANT, IDENTIFIER, etc)
#
#    TokLoc           source location information, for error reporting.


# A TokLoc describes a source location; for error reporting purposes.
#    s          -- Entire string (i.e., typically an input line)
#    sourcename -- Name of the input source as was given to the
#                  tokenizer (when created/invoked). CAN BE None.
#    lineno     -- Line number, counted from the start lineno given
#                  to the tokenizer. CAN BE None (means no line numbers)
#    startpos   -- Index with s where the error occurred.
#    endpos     -- ONE PAST the end of the error (i.e., the "next" position)
#
# NOTE: A bare TokLoc() can be specified if none of this is useful/known.
#
@dataclass
class TokLoc:
    s: str = ""
    sourcename: str = "unknown"
    lineno: int | None = None
    startpos: int = 0
    endpos: int = 0


# This Token class is the Token type produced by the Tokenizer.
# Subclasses can override if necessary; define a different class and
# then override the class-level variable "Token". See test_subtoken in
# the unittests for an example of this.

@dataclass
class Token:
    id: Enum                   # The TokenID Enum created automatically
    value: typing.Any          # typically string but could be int or others
    location: TokLoc           # source stream info for error reporting


class Tokenizer:
    """Break iterables of strings into Tokens with rules from regexps."""

    # subclasses can change the Token type produced by overriding this
    Token = Token

    # TOKENIZER
    def __init__(self, rules, strings=None, /, *, loc=None):
        """Set up a Tokenizer; see tokens() to generate tokens.

        Arguments:
           rules    -- A TokenRules object.

           strings  -- should be an iterable of strings. Can be None.
                       Most commonly it is an open text file.

           **NOTE** :: A TokenMatch will not find a match ACROSS a string
                       boundary. Said differently: Every Token must lie
                       within a single string. Typically this is a non-issue
                       when each string is a line in a file but it could be
                       a problem if the strings are arbitrary chunks of some
                       larger source. See also: Tokenizer.linefilter

           loc      -- [optional] TokLoc or a naked string. Specifies the
                       information for source error reporting. If it is
                       a naked string then that is used as the file name and
                       the line number starts at 1. Otherwise those values
                       come from loc.sourcename and loc.lineno
        """

        self.rules = rules
        self.current_ruleset = rules.rulesets[rules.primary_rulename]
        self.strings = strings
        self.lineno = getattr(loc, 'lineno', 1)
        self.sourcename = getattr(loc, 'sourcename', loc)

    # Iterating over a Tokenizer is the same as iterating over
    # the tokens() method, but without the ability to specify other args.
    # NOTE: This only makes sense if the input ("strings" argument) was
    #       provided at Tokenizer init time.
    def __iter__(self):
        return self.tokens()

    def tokens(self, strings=None, /, *, loc=None):
        """GENERATE tokens. See __init__() for arg descriptions."""

        # rationalize vs what was (or was not) provided at __init__ time
        if strings is None:
            strings = self.strings

        if loc is not None:
            self.sourcename = loc.sourcename
            self.lineno = loc.lineno

        # distinguish between line number tracking and not...
        try:
            g = enumerate(strings, start=self.lineno)
        except TypeError:
            g = ((None, s) for s in strings)

        for i, s in g:
            yield from self.string_to_tokens(
                s, loc=TokLoc(lineno=i, sourcename=self.sourcename))

    def string_to_tokens(self, s, /, *, loc=None):
        """Tokenize string 's', yield Tokens."""

        sourcename = getattr(loc, 'sourcename', self.sourcename)
        lineno = getattr(loc, 'lineno', self.lineno)

        so_far = 0
        grules = None      # the rules that were used to make generator 'g'

        while True:
            # this fires on any rules change AND ALSO the first time through
            if grules is not self.current_ruleset:
                grules = self.current_ruleset
                g = re.finditer(grules.joined_rx, s[so_far:])
                baseoffset = so_far

            # 'tm' is the TokenMatch that matched
            tm, value, startrel, stoprel = self._nextmatch(g)
            start = startrel + baseoffset
            if tm is None or start != so_far:
                break

            # build the TokLok for error reporting
            so_far = stoprel + baseoffset   # end of processed chars in s
            loc = TokLoc(s, sourcename, lineno, start, so_far)

            # perform the TokenMatch "action" and if all good, yield a token
            if (tok := tm.action(value, loc, self)) is not None:
                yield tok

        # If haven't made it to the end, something didn't match along the way
        if so_far != len(s):
            raise self.MatchError(
                f"unmatched @{so_far}, {s=}",
                loc=TokLoc(s, sourcename, lineno, so_far, so_far))

    def _nextmatch(self, g):
        """Support for string_to_tokens; returns next match and info"""

        try:
            mobj = next(g)
        except StopIteration:
            return None, None, -1, -1
        tm = self.current_ruleset.pmap[mobj.lastgroup]
        return tm, mobj.group(0), mobj.start(), mobj.end()

    # support for switching the active rules.
    def nextruleset(self):
        """Switch to the next ruleset, using circular-order"""
        allnames = list(self.rules.rulesets)
        i = allnames.index(self.current_ruleset.name)
        try:
            self.activate_ruleset(allnames[i + 1])
        except IndexError:
            self.activate_ruleset(allnames[0])

    def activate_ruleset(self, name=None, /):
        self.current_ruleset = self.rules.rulesets[name]

    class MatchError(Exception):
        """Exception raised when the input doesn't match any rules"""
        def __init__(self, *args, loc=None, **kwargs):
            super().__init__(*args, **kwargs)
            self.loc = loc
            self.add_note(f"Token Location: {loc}")

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
    def linefilter(strings, /, *, preservelinecount=True):
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


# A NamedRuleSet collects TokenMatch objects together along with a
# name, and is useful for creating TokenRules with multiple such sets.
# This is unnecessary in the standard/simple case where there is only
# one (unnamed) set of TokenMatch objects; those can be given to
# TokenRules directly.
@dataclass(kw_only=True)
class NamedRuleSet:
    rules: typing.List
    name: typing.Optional[str] = None

    pmap: dict = field(init=False)        # created from the rules
    joined_rx: str = field(init=False)    # ...

    def __post_init__(self):
        # Create one enormous "or" regexp with (?P=name) annotations
        # for each clause within it. The 'name' is a "pname" - not a
        # tokname - because toknames can appear multiple times in rules.
        # The pmap attribute maps these annotation names to token names

        self.pmap = {f"PN{i:04d}": tm for i, tm in enumerate(self.rules)}
        self.joined_rx = '|'.join(f'(?P<{pname}>{tm.regexp})'
                                  for pname, tm in self.pmap.items()
                                  if tm.regexp is not None)


#
# TokenRules encapsulate one or more sets of TokenMatch objects, forming
# the lexical rules for a Tokenizer. In the simplest case, a TokenRules
# contains a single grouping of TokenMatch objects, indeed the most
# common way to instantiate a TokenRules is something like:
#    tkr = TokenRules([
#              TokenMatch('VARIABLE', r'[A-Za-z][A-Za-z0-9]*'),
#              TokenMatchIgnore('WHITESPACE', r'\s+'),
#              TokenMatchInt('CONSTANT', r'-?[0-9]+'),
#          )]
#
# In more complicated use cases, in addition to the primary_rules specified
# as above any additional number of NamedRuleSet objects can be given.
#
class TokenRules:
    def __init__(self, primary_rules, *alt_rules):

        # If there is only one group of rules they can be given directly
        # as an iterable of TokenMatch objects rather than a NamedRuleSet;
        # look for that and convert.

        try:
            self.primary_rulename = primary_rules.name
        except AttributeError:
            primary_rules = NamedRuleSet(rules=primary_rules)
            self.primary_rulename = primary_rules.name

        if self.primary_rulename in (n_r.name for n_r in alt_rules):
            raise ValueError(f"Dup primary name: {self.primary_rulename}")

        self.rulesets = {n_r.name: n_r for n_r in (primary_rules, *alt_rules)}
        self.TokenID = self.__makeEnum()

    def __makeEnum(self):
        # ordering is not really guaranteed, but given that dicts
        # preserve insertion order, this produces an Enum with the
        # toknames in order of definition.
        allnames = []
        for rs in self.rulesets.values():
            for tokname in (tm.tokname for tm in rs.pmap.values()):
                if tokname not in allnames:
                    allnames.append(tokname)

        return Enum('TokenID', allnames)


# A TokenMatch combines a name (e.g., 'CONSTANT') with a regular
# expression (e.g., r'-?[0-9]+'), and its action() method for
# processing the match and creating the token.

class TokenMatch:

    # A few convenience-variables useful for "identifier" style regexps

    # This unicode regexp is from (mash this into one long URL):
    #   https://stackoverflow.com/questions/1673749/
    #           how-to-match-alphabetical-chars-without-
    #           numeric-chars-with-python-regexp
    #
    ID_UNICODE: typing.ClassVar[str] = r'[^\W\d]\w*'
    ID_UNICODE_NO_UNDER: typing.ClassVar[str] = r'[^\W\d_][^\W_]*'

    # The ASCII versions are traditional/easy
    ID_ASCII = r'[A-Za-z_][A-Za-z_0-9]*'
    ID_ASCII_NO_UNDER = r'[A-Za-z][A-Za-z0-9]*'

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
        return tkz.Token(
            tkz.rules.TokenID[name or self.tokname], self._value(val), loc)


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

            TokenMatch('IF', r'(if)(magic)

       where 'magic' is "not the TokenMatch.ID_UNICODE expression"
    """
    def __init__(self, tokname, regexp=None, *args, **kwargs):
        if regexp is None:
            regexp = self.keyword_regexp(tokname)
        super().__init__(tokname.upper(), regexp, *args, **kwargs)

    # broken out so can be overridden if application has other syntax
    def keyword_regexp(self, tokname):
        return f"({tokname})(?!{TokenMatch.ID_UNICODE})"


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
    NEXTRULE = object()

    def __init__(self, *args, rulename=None, **kwargs):
        super().__init__(*args, **kwargs)
        self.rulename = rulename

    def action(self, val, loc, tkz, /):
        if self.rulename is self.NEXTRULE:
            tkz.nextruleset()
        else:
            tkz.activate_ruleset(self.rulename)
        return super().action(val, loc, tkz)


if __name__ == "__main__":
    import unittest

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
                id, val = x
                self.assertEqual(t.id, id)
                self.assertEqual(t.value, val)

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
                    id = getattr(rules.TokenID, ids)
                    self.assertEqual(t.id, id)
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
                    c = lines[e.loc.lineno][e.loc.startpos]
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
            tkz = Tokenizer(rules)
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
                    tokid = tkz.rules.TokenID[self.tokname]
                    return MyToken_1(tokid, val, loc)

            class TokenMatch_2(TokenMatch):
                def action(self, val, loc, tkz, /):
                    tokid = tkz.rules.TokenID[self.tokname]
                    return MyToken_2(tokid, val, loc)

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
                id, val = x
                with self.subTest(id=id, val=val, t=t):
                    self.assertEqual(t.id, id)
                    self.assertEqual(t.value, val)

    unittest.main()
