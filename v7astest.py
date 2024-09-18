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

# the idea of this test is it takes the source files from Unix v7 'as'
# and assembles them, and compares the result to a "golden object file"
# taken from assembling the same source under Unix v7 (running under SIMH).
#
# However, since - technically - the v7 assembler source is still under
# copyright and it's not clear whether it would be legal to therefore
# include it with this source, what this test does is:
#
#   - look for a directory named (by default) V7TEST. If found:
#       - run an assembler test on files named 'as1*.s'
#       - compare that output to a file named: 'as1-gold.out'
#       - run an assembler test on files named 'as2*.s'
#       - compare that output to a file named: 'as2-gold.out'
#
# If the directory or the important files are not found, no test is run.
#


if __name__ == '__main__':
    import unittest
    import argparse
    import itertools
    import sys
    from astokens import ASMTokenizer
    from asparse import ASMParser
    from main import sysinclude, OpenedFileInfo

    SOURCEDIR = 'V7TEST'
    GOLD = 'as{}-gold.out'

    class TestMethods(unittest.TestCase):
        @classmethod
        def setUpClass(cls):
            cls.GOLDS = [f"{SOURCEDIR}/{GOLD.format(1)}",
                         f"{SOURCEDIR}/{GOLD.format(2)}"]

        def gatherfiles(self, as12):
            files = [sysinclude()]
            for i in range(10):
                name = f"{SOURCEDIR}/as{as12}{i}.s"
                try:
                    f = open(f"{SOURCEDIR}/as{as12}{i}.s", "r")
                except OSError:
                    pass
                else:
                    files.append(OpenedFileInfo(f, name))
            return files

        def cmprslts(self, rslt, goldfile):
            textanddata = rslt[0][1] + rslt[1][1]
            hdr = goldfile.read(16)       # a.out header

            # explicitly get the text/data sizes and only read that,
            # because the GOLD file usually also has a symbol table
            # which the rslt will not have, of course.
            goldtextsize = hdr[3]*256 + hdr[2]
            golddatasize = hdr[5]*256 + hdr[4]
            goldbytes = goldfile.read(goldtextsize + golddatasize)

            # these should be same length - both are text+data byte arrays
            self.assertEqual(len(goldbytes), len(textanddata))

            # compare them the slow way for better error message reasons
            for i, b1b2 in enumerate(zip(goldbytes, textanddata)):
                if b1b2[0] != b1b2[1]:
                    print("Miscompare at ", i, b1b2)
                    return False
            return True

        def test_asv7as(self):
            for i in (0, 1):
                with self.subTest(passno=i+1):
                    goldfile = open(self.GOLDS[i], 'rb')
                    tokenizers = [
                        ASMTokenizer(info.f, srcname=info.name).tokens()
                        for info in self.gatherfiles(i+1)]
                    az = ASMParser(itertools.chain(*tokenizers))
                    if not az.firstpass():
                        show_errors(az)
                        assert False, "First pass failed"
                    rslt = az.secondpass()
                    if rslt is None or az.errors:
                        show_errors(az)
                        assert False, "Second pass failed"
                    self.assertTrue(self.cmprslts(rslt, goldfile))

    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--directory', default=SOURCEDIR)
    parser.add_argument('-g', '--goldfiles', default=GOLD)
    args = parser.parse_args()
    SOURCEDIR = args.directory
    GOLD = args.goldfiles

    suite = unittest.TestSuite()
    suite.addTest(TestMethods('test_asv7as'))
    runner = unittest.TextTestRunner()
    results = runner.run(suite)
    if results.errors or results.failures:
        sys.exit(1)
