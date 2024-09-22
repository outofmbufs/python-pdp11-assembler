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

# Segment (.text/.data/.bss) support

import itertools


class Segment:
    def __init__(self, segID=None, *, offset=None):
        """A Segment is an augmented list of XNode objects.

        In addition to keeping XNode objects in an ordered sequence,
        each Segment supports an independent '.' pseudo-variable.

        Typically, there will be three segments: text, data, and bss.
        The segID attribute can be used by callers to tag segments.
        The class itself does not ever look at the segID attribute.
        """
        self.segID = segID
        self.offset = offset     # segment 'base', often determined later

        # these are controlled by .org or .boundary directives
        self.org = None
        self.boundary = None

        # The pseudo-variable "." means the current offset within the
        # current segment. To avoid recomputing '.' every time it is
        # referenced, _dots is a parallel list of running dot values.
        # It is (only) computed/updated when needed.
        #
        # _dots[i] is the value of dot AFTER _nodes[i]
        #
        self._nodes = []
        self._dots = None

    @staticmethod
    def _none_zero(z):
        return 0 if z is None else z

    def addnode(self, xn, /):
        """Append an Xnode to the segment."""
        self._nodes.append(xn)
        self._dots = None             # force recompute when next needed

    def __len__(self):
        """Careful, this is the length in XNodes."""
        return len(self._nodes)

    def __getitem__(self, key):
        """Return the XNode at index 'key'."""
        return self._nodes[key]

    def last(self):
        try:
            return self._nodes[-1]
        except IndexError:
            return None

    def replacenode(self, oldxn, newxn, /):
        self._nodes[self._nodes.index(oldxn)] = newxn
        self._dots = None             # force recompute when next needed

    @staticmethod
    def rounduptomultiple(n, m):
        return ((n + m - 1) // m) * m

    def origindirective(self, v, roundup=False):
        if roundup:
            self.boundary = v
        else:
            self.org = v

    def setoffset(self, atleast):
        """Set the .offset to be >= atleast and respecting .org/.boundary.

        Raises ValueError if there is a conflict (e.g., a .org that
        is below 'atleast')
        """
        if self.org is not None:
            if self.org < atleast:
                raise ValueError("segment .org conflict")
            atleast = self.org

        if self.boundary is not None:
            atleast = self.rounduptomultiple(atleast, self.boundary)

        self.offset = atleast

    def _computedots(self):
        """Run through all nodes to get the _dots set up"""
        self._dots = list(itertools.accumulate(x.nbytes for x in self._nodes))

    def dot(self, *, after=None, before=None):
        """Return dot value after/before a node (or at end if None)."""

        if not self._dots:
            self._computedots()

        if before and after:
            raise ValueError("cannot specify both before and after")

        # "before" is implemented as after, adjusted
        if after is None:
            after = before

        # If after is None, use the last node. The result is zero
        # if there are no nodes.
        if after is None:
            if not self._nodes:
                return 0
            after = self.last()

        # If after is not in _nodes that's a KeyError, including if _nodes
        # itself is None (see "after is None" and "not self._nodes" above)
        try:
            afterindex = self._nodes.index(after)
        except (ValueError, TypeError):
            raise KeyError(f"dot(): {after} not found") from None

        d = self._dots[afterindex]
        if before:
            d -= before.nbytes
        return d

    def byteseq(self):
        """Return the full byte sequence from every node in the segment"""
        return bytes(
            itertools.chain.from_iterable(
                map(lambda node: node.byteseq(), self._nodes)))


if __name__ == "__main__":
    import unittest

    # Very very dummied up XNode for testing
    class _XNode:
        def __init__(self, value, *, segment=None, sz=2):
            self.value = value
            self.segment = segment
            self.nbytes = sz

    class TestMethods(unittest.TestCase):
        def test_len(self):
            seg = Segment()
            self.assertEqual(len(seg), 0)

            seg.addnode(_XNode(0))
            self.assertEqual(len(seg), 1)

            seg.addnode(_XNode(1))
            self.assertEqual(len(seg), 2)

        def test_subscript(self):
            seg = Segment()

            seg.addnode(x0 := _XNode(0))
            seg.addnode(x1 := _XNode(1))
            self.assertTrue(seg[0] is x0)
            self.assertTrue(seg[1] is x1)
            self.assertTrue(seg[-1] is x1)

        def test_last(self):
            seg = Segment()

            for _ in range(5):
                seg.addnode(xN := _XNode(0))
                self.assertTrue(seg.last() is xN)

        def test_dot(self):
            seg = Segment()

            self.assertEqual(seg.dot(), 0)
            nodesize = 2
            nodes = [_XNode(0, sz=nodesize) for _ in range(10)]
            for xn in nodes:
                seg.addnode(xn)

            for i, xn in enumerate(nodes):
                self.assertEqual(seg.dot(after=xn), nodesize * (i + 1))

        def test_dotbefore(self):
            seg = Segment()
            nodes = [_XNode(0, sz=i*2) for i in range(10)]
            for xn in nodes:
                seg.addnode(xn)

            expected = 0
            for xn in nodes:
                self.assertEqual(seg.dot(before=xn), expected)
                expected += xn.nbytes

        # test cases where node not found by dot and segment is empty
        def test_dot_notfound_after0(self):
            seg = Segment()
            with self.assertRaises(KeyError):
                _ = seg.dot(after=_XNode(0))

        # test cases where node not found by dot and segment is not empty
        def test_dot_notfound_after1(self):
            seg = Segment()
            seg.addnode(_XNode(0))
            with self.assertRaises(KeyError):
                _ = seg.dot(after=_XNode(0))

        # test node replacement and proper dot calculations
        def test_replacement(self):
            seg = Segment()

            N = 8
            sizes = list(range(1, N+1))
            for sz in sizes:
                seg.addnode(_XNode(0, sz=sz))

            self.assertEqual(sum(sizes), seg.dot())

            # replace an element partway through
            partway = N // 2       # not critical which one this is
            oldnode = seg[partway]

            # replace old with a zero-length one
            seg.replacenode(oldnode, _XNode(0, sz=0))
            sizes[partway] = 0    # for use beow

            # now the adjusted sizes should still add up to (new) dot
            self.assertEqual(sum(sizes), seg.dot())

    unittest.main()
