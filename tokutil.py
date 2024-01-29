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

# Token Stream Enhancer
#
# Adds unget, peek, and mark/restore capability to any stream of "tokens"
# (really any iterable)
#


from contextlib import contextmanager
from functools import partial
from types import SimpleNamespace
import itertools


class TokStreamEnhancer:
    def __init__(self, *tokstreams, lasttok=None, eoftok=None):
        """chain N tokstreams into a single stream, with pushback capability.

        Optional argument lasttok will be returned as a token at the end
        of the tokstreams and prior to reaching EOF condition. It is
        identical to adding: iter((lasttok,)) at the end of tokstreams.

        Optional argument eoftok will be returned once the end of the token
        streams has been reached. If supplied, it will be returned REPEATEDLY
        and no StopIteration exception will ever be raised.

        If no eoftok is supplied, StopIteration will be raised once the
        end of the tokstreams has been reached AND no pushbacks remain.
        """

        if lasttok is not None:
            tokstreams = (*tokstreams, iter((lasttok,)))

        self._tokens = itertools.chain.from_iterable(tokstreams)
        self._eoftok = eoftok

        self._pushedback = []                  # see peektok() etc
        self._markedtoks = None                # for unwinding tokens
        self._unmarkctx = None                 # for unwinding tokens

    # token peek/get .. everything is built on top of peektoks() which
    # ensures there are at least N tokens on the _pushedback queue.
    def peektoks(self, n):
        """Return a LIST of n tokens, WITHOUT consuming them."""
        while len(self._pushedback) < n:
            try:
                t = next(self._tokens)
            except StopIteration:
                if self._eoftok is None:
                    raise
                t = self._eoftok
            self._pushedback.append(t)
        return self._pushedback[:n]

    def peektok(self):
        """Return a single token, WITHOUT consuming it."""
        return self.peektoks(1)[0]

    _NOTGIVEN = object()

    def peekif(self, pred, /, *, eofmatch=_NOTGIVEN):
        """Return (peek) a token, if pred(token) is True, else return None.

        If optional argument eofmatch is given, it is returned (regardless
        of pred match) if at_eof().
        """
        if eofmatch is not self._NOTGIVEN and self.at_eof():
            return eofmatch
        t = self.peektok()
        return t if pred(t) else None

    def at_eof(self):
        try:
            return self.peektok() is self._eoftok
        except StopIteration:
            return True

    def gettok(self):
        """Return a single token and move to next."""
        self.peektok()          # guarantee at least one on pushedback
        t = self._pushedback.pop(0)
        if self._markedtoks is not None:      # see tokmark()
            self._markedtoks.append(t)
        return t

    def gettoks(self, n):
        """Like n calls to gettok(), returning a LIST of n tokens"""
        return [self.gettok() for _ in range(n)]

    def ungettok(self, tok=None, *, bulk=None):
        """Push tok back onto front of stream.

        An entire list of toks can be pushed back via 'bulk'.
        It is illegal to supply both tok and bulk.
        """
        if bulk is None:
            self._pushedback.insert(0, tok)
        elif tok is None:
            if bulk:     # it could be zero-len
                self._pushedback = bulk + self._pushedback
        else:
            raise TypeError("internal error: tok and bulk")

    # tokmark is a WITH statement context manager.
    # It sets a 'mark' in the token stream and automatically ungets
    # tokens back to that point when the context is exited.
    #
    # acceptmarks() prevents that unwind for the "current" context.
    #
    # In the simplest, non-nested form:
    #      with foo.tokmark():
    #          t1 = foo.gettok()
    #          t2 = foo.gettok()
    #
    # This is equivalent to calling gettok ZERO times ... no tokens
    # will be consumed. More precisely, they are consumed during the
    # context, but are put back when the context terminates.
    #
    # Method acceptmarks() neutralizes the tokmark() so that all tokens
    # will remain consumed, whether they were consumed before or after
    # the call to acceptmark(). For example:
    #      with foo.tokmark():
    #          t1 = foo.gettok()
    #          t2 = foo.gettok()
    #          foo.acceptmarks()
    #          t3 = foo.gettok()
    #
    # This is equivalent to calling gettok() three times.
    #
    # Usage of mark/accept can be nested, in which case an explicit context
    # must be provided, and acceptmarks invoked from that context:
    #    with foo.tokmark() as ctx:
    #        t1 = foo.gettok()
    #        with foo.tokmark() as ctx2:
    #            t2 = foo.gettok()
    #            ctx2.acceptmarks()
    #        t3 = foo.gettok()
    #    t4 = foo.gettok()
    #
    # This will cause t3 to be the same token AFTER t2, because of
    # the ctx2.acceptmarks(). However, t4 will be the same token as t1,
    # (and the next two tokens will be the same as t2, t3, respectively)
    # because the outer context was not accepted.

    @contextmanager
    def tokmark(self):
        ctx = SimpleNamespace()

        ctx.acceptmarks = partial(self.acceptmarks, ctx)
        ctx.accepted = False
        ctx.prevmarked, self._markedtoks = self._markedtoks, []
        prevctx, self._unmarkctx = self._unmarkctx, ctx

        try:
            yield ctx
        finally:
            if ctx.accepted:
                # if there is a nested context these tokens still need
                # to be added to THAT one
                if ctx.prevmarked is not None:
                    ctx.prevmarked += self._markedtoks
            else:
                self.ungettok(bulk=self._markedtoks)
            self._markedtoks = ctx.prevmarked
            self._unmarkctx = prevctx

    def acceptmarks(self, ctx=None):
        """Disable (i.e., no-op) the unwind of current tokmark() context."""
        if ctx is None:
            ctx = self._unmarkctx
        ctx.accepted = True


if __name__ == "__main__":
    import unittest

    class TestMethods(unittest.TestCase):
        def testsimple(self):
            t = TokStreamEnhancer(iter((1, 2, 3)))
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 3)
            with self.assertRaises(StopIteration):
                t.gettok()

        def testN(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 3)
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            with self.assertRaises(StopIteration):
                t.gettok()

        def testEOFtok(self):
            t = TokStreamEnhancer(iter((1, 2)), eoftok=99)
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertTrue(t.at_eof())
            for i in range(10):     # arbitrary, point is > 1
                self.assertEqual(t.gettok(), 99)
            self.assertTrue(t.at_eof())

        def testLASTtok(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), lasttok=99)
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 99)
            with self.assertRaises(StopIteration):
                t.gettok()

        def testLASTandEOFtok(self):
            t = TokStreamEnhancer(iter((1, 2)), lasttok=17, eoftok=99)
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 17)
            self.assertTrue(t.at_eof())
            for i in range(10):     # arbitrary, point is > 1
                self.assertEqual(t.gettok(), 99)

        def test_peek(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            for i in range(5+1):
                self.assertEqual(t.peektoks(i), list(range(1, i+1)))
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 3)
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            with self.assertRaises(StopIteration):
                t.gettok()

        def test_peekpastEOF(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            for i in range(5+1+4):
                try:
                    toks = t.peektoks(i)
                    self.assertEqual(toks, list(range(1, i+1)))
                except StopIteration:
                    self.assertTrue(i > 5)

        def test_peekpastEOFtok(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)),
                                  eoftok=99)
            for i in range(5+1+4):
                toks = t.peektoks(i)
                if i <= 5:
                    self.assertEqual(toks, list(range(1, i+1)))
                else:
                    self.assertEqual(toks, list(range(1, 6)) + [99] * (i - 5))

        def test_peekif(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.peekif(lambda t: t == 2), None)
            self.assertEqual(t.gettok(), 1)
            self.assertEqual(t.peekif(lambda t: t == 2), 2)

        def test_mark(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.gettok(), 1)
            with t.tokmark() as ctx:
                self.assertEqual(t.gettok(), 2)
                self.assertEqual(t.gettok(), 3)
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 3)
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            self.assertTrue(t.at_eof())

        def test_mark_accept(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.gettok(), 1)
            with t.tokmark() as ctx:
                self.assertEqual(t.gettok(), 2)
                self.assertEqual(t.gettok(), 3)
                ctx.acceptmarks()
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            self.assertTrue(t.at_eof())

        def test_mark_accept_nested(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.gettok(), 1)
            with t.tokmark() as ctx:
                self.assertEqual(t.gettok(), 2)
                self.assertEqual(t.gettok(), 3)
                with t.tokmark() as ctx2:
                    self.assertEqual(t.gettok(), 4)
                    self.assertEqual(t.gettok(), 5)
                ctx.acceptmarks()
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            self.assertTrue(t.at_eof())

        def test_mark_accept_nested2(self):
            t = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            self.assertEqual(t.gettok(), 1)
            with t.tokmark() as ctx:
                self.assertEqual(t.gettok(), 2)
                self.assertEqual(t.gettok(), 3)
                with t.tokmark() as ctx2:
                    self.assertEqual(t.gettok(), 4)
                    ctx2.acceptmarks()
            self.assertEqual(t.gettok(), 2)
            self.assertEqual(t.gettok(), 3)
            self.assertEqual(t.gettok(), 4)
            self.assertEqual(t.gettok(), 5)
            self.assertTrue(t.at_eof())

        def test_mark_example(self):
            # the test case form the comments in tokmark()/accept:
            foo = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            with foo.tokmark() as ctx:
                t1 = foo.gettok()
                self.assertEqual(t1, 1)
                with foo.tokmark() as ctx2:
                    t2 = foo.gettok()
                    self.assertEqual(t2, 2)
                    ctx2.acceptmarks()
                t3 = foo.gettok()
                self.assertEqual(t3, 3)
            t4 = foo.gettok()
            self.assertEqual(t4, t1)
            self.assertEqual(foo.gettok(), t2)
            self.assertEqual(foo.gettok(), t3)

        def test_mark_example2(self):
            # the same test but using implicit contexts
            foo = TokStreamEnhancer(iter((1,)), iter((2,)), iter((3, 4, 5)))
            with foo.tokmark():
                t1 = foo.gettok()
                self.assertEqual(t1, 1)
                with foo.tokmark():
                    t2 = foo.gettok()
                    self.assertEqual(t2, 2)
                    foo.acceptmarks()
                t3 = foo.gettok()
                self.assertEqual(t3, 3)
            t4 = foo.gettok()
            self.assertEqual(t4, t1)
            self.assertEqual(foo.gettok(), t2)
            self.assertEqual(foo.gettok(), t3)

    unittest.main()
