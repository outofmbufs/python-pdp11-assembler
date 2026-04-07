"""A generic tokenizer driven by regular expressions."""

# pylint: disable=missing-function-docstring
# ... because pylint is annoying about this for property getters, etc.

from dataclasses import dataclass, field
import dataclasses                          # for dataclasses.replace
from enum import Enum
import itertools
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
#
#    TokenAction      Context used by the framework; part of the interface
#                     for TokenMatch subclasses. Only applications with their
#                     own TokenMatch subclasses need to know about this.
#


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
@dataclass(frozen=True)
class TokLoc:
    """Describes the source location that led to this token being created."""
    s: str = ""
    sourcename: str = "unknown"
    lineno: int | None = None
    startpos: int = 0
    endpos: int = 0

    # Make another TokLoc like this one but with specific overrides
    def copy_with(self, **overrides):
        """Copy a TokLoc optionally overriding some attributes."""
        return dataclasses.replace(self, **overrides)


# This Token class is the Token type produced by the Tokenizer.
# Subclasses can override if necessary; define a different class and
# then override the class-level variable "Token". See test_subtoken in
# the unittests for an example of this.

@dataclass(frozen=True)
class Token:
    """A 'token' resulting from lexical analysis by the Tokenizer"""
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
        self.strings = strings if strings is not None else []
        self.lineno = getattr(loc, 'lineno', 1)
        self.sourcename = getattr(loc, 'sourcename', loc)

    # Iterating over a Tokenizer is the same as iterating over
    # the tokens() method, but without the ability to specify other args.
    # NOTE: This only makes sense if the input ("strings" argument) was
    #       provided at Tokenizer init time.
    def __iter__(self):
        return self.tokens()      # note this returns the generator

    def tokens(self, strings=None, /, *, loc=None):
        """GENERATE tokens. See __init__() for arg descriptions."""

        # rationalize vs what was (or was not) provided at __init__ time
        if strings is None:
            strings = self.strings

        if loc is not None:
            self.sourcename = loc.sourcename
            self.lineno = loc.lineno

        # each string in the iterable of strings will be considered
        # as a separate line, lexed on its own.
        for i, s in self.__linenumbers_and_lines(strings):
            yield from self.string_to_tokens(
                s, loc=TokLoc(lineno=i, sourcename=self.sourcename))
            self.lineno += 1

    def __linenumbers_and_lines(self, strings):
        """Helper for tokens generator creation; allows for lineno vs None"""

        try:
            linenums = itertools.count(self.lineno)   # normal case
        except TypeError:
            linenums = itertools.repeat(None)         # no line numbers

        try:
            g = zip(linenums, strings)
        except TypeError:
            # something is wrong with strings (not iterable)
            not_an_iterable = f"input: `{strings!r}` is not an iterable"
            raise ValueError(not_an_iterable) from None
        return g

    def string_to_tokens(self, s, /, *, loc=None):
        """Tokenize string 's', yield Tokens."""

        # ctx is context (TokLoc etc) as matches proceed; set it up.
        ctx = TokenAction(tkz=self)
        if loc is None:
            loc = TokLoc(sourcename=self.sourcename, lineno=self.lineno)
        ctx.location = loc.copy_with(s=s)

        # outer 'while' loop allows for rules changes
        while True:
            grules = self.current_ruleset
            for tm in self._matches(ctx):
                try:
                    tok = tm.action(ctx)
                except TypeError:    # usually means tm.action is None
                    tok = tm.action
                if tok is not None:
                    yield tok
                # If the tm.action changed the lexing rules then get
                # out of this inner 'for' loop so as to get a
                # new _matches sequence
                if grules is not self.current_ruleset:
                    break                       # but nuke this _matches
            else:
                # 'for' loop ended without a rules change, so all done
                break

        if ctx.endpos != len(s):
            raise self.MatchError(
                f"unmatched @{ctx.location}", location=ctx.location)

    def _matches(self, ctx):
        """Support for string_to_tokens; returns next match and info"""

        # NOTE: This is a little puzzling:
        #   If this is the FIRST TIME:
        #      ctx.endpos is zero; everything is zero and the full string
        #      is sent to finditer().
        #
        #   If this is AFTER A RULES CHANGE:
        #      ctx.endpos is the end of the rules-change match. "Eat" the
        #      the string up to there; starting_startpos is the "offset" of
        #      that (and also used to adjust .location attributes to be
        #      relative to the entire string, not just the working string)
        starting_startpos = ctx.endpos
        working_s = ctx.location.s[starting_startpos:]

        # loop until no more matches, OR a "missed" match that does not
        # include the very next character.
        # NOTE: If there is a rules change, string_to_tokens abandons
        #       this generator without letting it complete. It will then
        #       restart the generator with the new rules; see above too.
        for mobj in re.finditer(self.current_ruleset.joined_rx, working_s):
            ctx.startpos = mobj.start() + starting_startpos

            # Check for matches not consuming the immediate next char
            if ctx.startpos != ctx.endpos:
                # flip start/end so applications can know what did not match
                ctx.startpos, ctx.endpos = ctx.endpos, ctx.startpos
                raise self.MatchError(
                    f"unmatched @{ctx.location}", location=ctx.location)

            tm = self.current_ruleset.pmap[mobj.lastgroup]
            ctx.token_id = tm.tokname
            ctx.endpos = mobj.end() + starting_startpos
            ctx.value = mobj.group(0)
            yield tm

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
        """Switch to the given/named result (default: primary)"""
        self.current_ruleset = self.rules.rulesets[name]

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
                escaped = nslashes % 2
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
    """A way to name a sequence of TokenMatch objects"""
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
# TokenRules encapsulate one or more sequences of TokenMatch objects, forming
# the lexical rules for a Tokenizer. In the simplest case, a TokenRules
# contains a single sequence of TokenMatch objects, indeed the most
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
    """Encapsulate one or more sequences of TokenMatch objects."""
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
        self.TokenID = self.__make_enum()  # pylint: disable=invalid-name

    def __make_enum(self):
        # ordering is not really guaranteed, but given that dicts
        # preserve insertion order, this produces an Enum with the
        # toknames in order of definition.
        allnames = []
        for rs in self.rulesets.values():
            for tokname in (tm.tokname for tm in rs.pmap.values()):
                if tokname not in allnames:
                    allnames.append(tokname)

        return Enum('TokenID', allnames)


# TokenAction is a mutable context passed to a TokenMatch.action method.
# When the action method is ready to make a token it can use the maketoken
# method to construct a token from the context info (though if the application
# requires something else, it is free to make the token any way it wants).
# Subclasses of TokenMatch may modify attributes within this context;
# for example, TokenMatchConvert modifies the value attribute.
#

@dataclass
class TokenAction:
    """Context for the action method in a TokenMatch."""

    value: typing.Any = None
    location: TokLoc = None
    tkz: Tokenizer = None
    token_id: Enum | str = None
    token_cls: typing.Callable = None  # usually this is Token (the class)

    def __post_init__(self):
        if self.token_cls is None and self.tkz is not None:
            self.token_cls = self.tkz.Token

    def maketoken(self):
        """Construct a token from the context."""
        # as a convenience, if token_id is convertible to the Enum, convert it
        try:
            tkid = self.tkz.rules.TokenID[self.token_id]
        except KeyError:
            if isinstance(self.token_id, self.tkz.rules.TokenID):
                tkid = self.token_id
            else:
                raise
        return self.token_cls(tkid, self.value, self.location)

    # these really simplify location tracking within string_to_tokens
    @property
    def startpos(self):
        return self.location.startpos

    @startpos.setter
    def startpos(self, value):
        self.location = self.location.copy_with(startpos=value)

    @property
    def endpos(self):
        return self.location.endpos

    @endpos.setter
    def endpos(self, value):
        self.location = self.location.copy_with(endpos=value)


# A TokenMatch combines a name (e.g., 'CONSTANT') with a regular
# expression (e.g., r'-?[0-9]+'), and its action() method for
# processing the match and creating the token.

@dataclass
class TokenMatch:
    """Associate a Token name with a regexp"""

    tokname: str
    regexp: str

    # A few convenience-variables useful for "identifier" style regexps

    # This unicode regexp is from (mash this into one long URL):
    #   https://stackoverflow.com/questions/1673749/
    #           how-to-match-alphabetical-chars-without-
    #           numeric-chars-with-python-regexp
    #
    ID_UNICODE: typing.ClassVar[str] = r'[^\W\d]\w*'
    ID_UNICODE_NO_UNDER: typing.ClassVar[str] = r'[^\W\d_][^\W_]*'

    # The ASCII versions are traditional/easy
    ID_ASCII: typing.ClassVar[str] = r'[A-Za-z_][A-Za-z_0-9]*'
    ID_ASCII_NO_UNDER: typing.ClassVar[str] = r'[A-Za-z][A-Za-z0-9]*'

    def __post_init__(self):

        # fail early, because failing later is very confusing...
        if self.regexp is not None:
            try:
                _ = re.compile(self.regexp)
            except re.error:
                raise ValueError(
                    self.__class__.__name__ +
                    f" {self.tokname}, bad regexp: '{self.regexp}'") from None

    def action(self, ta: TokenAction, /) -> Token | None:
        """Called by the framework to create a Token."""
        return ta.maketoken()


class TokenIDOnly(TokenMatch):
    """Put a bare tokname into the TokenID Enum; no regexp"""
    # no *args/**kwargs -  enforce "just a tokname and nothing else"
    def __init__(self, tokname):
        super().__init__(tokname, None)

    def action(self, ta, /):
        assert False, "action should not be reachable"


class TokenMatchIgnore(TokenMatch):
    """TokenMatch that eats tokens (i.e., matches and ignores them)."""

    action = None


class TokenMatchConvert(TokenMatch):
    """Type-convert the value field from string."""

    def __init__(self, *args, converter=int, **kwargs):
        """A TokenMatch that applies a converter() function to the value.
             converter:    will be applied to convert .value
        """
        super().__init__(*args, **kwargs)
        self.valconvert = converter

    def action(self, ta, /):
        ta.value = self.valconvert(ta.value)
        return super().action(ta)


# for TokenMatchInt the default TokenMatchConvert is fine (default is int)
TokenMatchInt = TokenMatchConvert


class TokenMatchKeyword(TokenMatch):
    """For keywords. Just specify the keyword no regexp.
       Example:

            TokenMatchKeyword('if')

       is equivalent to:

            TokenMatch('IF', r'(if)(magic)

       where 'magic' is "not the TokenMatch.ID_UNICODE expression"
    """
    def __init__(self, tokname, regexp=None, /):
        if regexp is None:
            regexp = self.keyword_regexp(tokname)
        super().__init__(tokname.upper(), regexp)

    # broken out so can be overridden if application has other syntax
    def keyword_regexp(self, tokname):
        """Create a regular expression from the keyword ('tokname')"""
        return f"({tokname})(?!{TokenMatch.ID_UNICODE})"


class TokenMatchIgnoreButKeep(TokenMatch):
    """Like Ignore, but will keep one specific character; typically NEWLINE"""
    def __init__(self, *args, keep, **kwargs):
        super().__init__(*args, **kwargs)

        if len(keep) > 1:
            raise ValueError(f"Multiple keeps not supported: {keep=}")
        self.keep = keep

    def action(self, ta, /):
        if self.keep not in ta.value:
            return None
        ta.value = self.keep
        return super().action(ta)


class TokenMatchRuleSwitch(TokenMatch):
    """Used to switch among multiple rulesets"""

    NEXTRULE = object()

    def __init__(self, *args, rulename=None, **kwargs):
        super().__init__(*args, **kwargs)
        self.rulename = rulename

    def action(self, ta, /):
        if self.rulename is self.NEXTRULE:
            ta.tkz.nextruleset()
        else:
            ta.tkz.activate_ruleset(self.rulename)
        return super().action(ta)


if __name__ == "__main__":
    try:
        from tkztests import TestMethods, run_unit_tests
    except ModuleNotFoundError:
        pass
    else:
        run_unit_tests(TestMethods)
