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

from collections import namedtuple
import argparse
import itertools
import io
from astokens import ASMTokenizer
from asparse import ASMParser


# Three output formats are supported:
#       407  -- a v7 a.out file in 407 format (no symbol/reloc data)
#      load  -- a SIMH binary LOAD file
#        do  -- a SIMH text file suitable for the DO command
#
# Each has a default output file name and a file opening mode
#
OUTPUT_INFO = {
    #    (default-name, output-mode)
    'load': ('a.simh', 'wb'),
    '407': ('a.out', 'wb'),
    'do': ('-', 'w'),
}


# This allows argparse.FileType() to be used while also exposing the
# file name for error reporting. Although the same information is available
# via the .name io class attribute, that attribute doesn't exist for
# StringIO objects (--org/--sys). And though --sys and --org streams
# don't need a name (they will never report errors unless there are bugs),
# ultimately it seemed best to just make this "save a name" be explicit.

OpenedFileInfo = namedtuple('OpenedFileInfo', ['f', 'name'])


def openkeepname(mode):
    opener_function = argparse.FileType(mode)

    def _inner(s):
        return OpenedFileInfo(f=opener_function(s), name=s)
    return _inner


def cmd_main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--format', type=str.lower,
                        choices=['load', 'do', '407'], default='do')
    parser.add_argument('--org', type=str, default=None)
    parser.add_argument('-o', '--output', type=str, default=None)
    parser.add_argument('--sys', action='store_true')
    parser.add_argument('--strictv7', action='store_true')
    parser.add_argument('inputs', nargs="*", type=openkeepname('r'))

    args = parser.parse_args()

    outdefault, outmode = OUTPUT_INFO[args.format]
    outname = args.output or outdefault
    # leverage FileType for '-' handling
    outf = argparse.FileType(outmode)(outname)

    # the --sys argument automatically includes the unix v7 trap numbers
    # (that are found in /usr/include/sys.s)
    if args.sys:
        args.inputs = [sysinclude()] + args.inputs

    # The way the --origin option works is by simply prepending
    # a tokenizer created from an io.StringIO of ".org {origin}.\n"
    if args.org:
        try:
            origin = argint(args.org)
        except ValueError:
            print(f"Invalid format for origin ({args.org})")
            return False
        args.inputs = [strfile(f".org {origin}.\n", "--org")] + args.inputs

    # To ensure the right file name shows in any error messages,
    # each input file becomes a separate ASMTokenizer (vs., e.g.,
    # chaining the strings from multiple files into one ASMTokenizer)
    tokenizers = [
        ASMTokenizer(info.f, srcname=info.name, id8=args.strictv7).tokens()
        for info in args.inputs]

    az = ASMParser(itertools.chain(*tokenizers))
    if not az.firstpass():
        show_errors(az)
        return False
    rslt = az.secondpass()
    if rslt is None or az.errors:
        show_errors(az)
        return False

    handler = {'407': aout_407,
               'load': simh_binary,
               'do': simh_text}[args.format]
    ok = handler(rslt, outf)
    if outname != '-':
        outf.close()
    return ok


def w2b(words):
    """Convert a sequence of words to (little-endian!) python bytes().

    words can be: a sequence, a naked integer, or None.
   """
    try:
        return bytes(_littlegen(words))
    except TypeError:
        # words was an integer, or, at least, not iterable
        return bytes(_littlegen((words,)))


def _littlegen(words):
    """Return a sequence of 8-bit values (as native integers)

    'words' should be a sequence of values 0..65535.
    Returns a sequence of values 0..255 representing the same
    values, in little-endian order.
    """
    for w in words:
        if w < 0 or w > 65535:
            raise ValueError(f"Illegal 16-bit value: {w}")
        yield w & 0o377
        yield (w >> 8) & 0o377


def aout_407(rslts, f):
    # To create an a.out file several assumptions are made:
    #   1) There must be 1, 2, or 3 chunks in rslts. They will be
    #      interpreted as .text, .data., and .bss
    #
    #   2) The base addresses of each chunk must be contiguous per size.
    #      That is, .text starts at 0, .data starts at .text + textsize, etc.

    if len(rslts) not in (1, 2, 3):
        print(f"Cannot convert {len(rslts)} segments into text/data/bss")
        return
    textbase, textb = rslts[0]
    textsize = len(textb)
    if textbase != 0:
        print(f"text must start at zero, not {oct(textbase)}")

    try:
        database, datab = rslts[1]
    except IndexError:
        database = textbase + textsize
        datab = bytes()
    datasize = len(datab)
    if database != textbase + textsize:
        print(f"data must start immediately after text")

    try:
        bssbase, bssb = rslts[2]
    except IndexError:
        bssbase = database + datasize
        bssb = bytes()
    bsssize = len(bssb)

    # phew, ok, the rest is easy
    header = [0o407, textsize, datasize, bsssize, 0, 0, 0, 0]
    f.write(w2b(header))
    f.write(textb)
    f.write(datab)
    return True


def simh_text(rslts, f):
    # the rslt is a sequence of tuples:
    #    (startaddress, bytes())
    # Turn this into a bunch of D ("DEPOSIT") commands
    for rslttuple in rslts:
        a, b = rslttuple
        for ival in map(lambda t: (t[0]*256)+t[1], (zip(b[1::2], b[0::2]))):
            f.write(f"D {oct(a)[2:]} {oct(ival)[2:]}\n")
            a += 2
    return True


def simh_binary(rslts, f):
    # From simh pdp11_sys.c ...
    # /* Binary loader.
    #    Loader format consists of blocks, optionally preceded, separated, and
    #    followed by zeroes.  Each block consists of:
    #
    #   001             ---
    #   xxx              |
    #   lo_count         |
    #   hi_count         |
    #   lo_origin        > count bytes
    #   hi_origin        |
    #   data byte        |
    #   :                |
    #   data byte       ---
    #   checksum        one byte
    #
    # If the byte count is exactly six, the block is the last on the tape, and
    # there is no checksum.  If the origin is not 000001, then the origin is
    # the PC at which to start the program. */
    #
    for rslttuple in rslts:
        a, b = rslttuple
        while b:
            outbuf = bytes((1, 0))        # 001:xxx
            todo = min(len(b), 31000)   # arbitrary but avoid sign bit
            cnt = todo + 6
            outbuf += bytes((cnt % 256, cnt // 256))
            outbuf += bytes((a % 256, a // 256))
            bw, b = b[:todo], b[todo:]
            outbuf += bw
            outbuf += bytes((256 - (sum(outbuf)) & 0xFF,))    # checksum
            f.write(outbuf)
            a += todo
    # terminator record
    f.write(bytes((1, 0, 6, 0, 1, 0)))
    return True


def show_errors(az):
    if len(az.errors) == 0 and len(az.warnings) == 0:
        # really shouldn't happen, but ...
        print("unknown error during assembly")
    for s in az.errors:
        print(f"Error: {s}")
    for s in az.warnings:
        print(f"Warning: {s}")


#
# Convert a value specified as a command argument (str) into an int.
# Formats supported:
#
#   naked numbers are OCTAL.
#        -- Digits 8 and 9 are allowed because 'as' allows
#   Leading 0o is allowed and means OCTAL too, no 8/9 allowed
#   Loading 0x is allowed and means HEX
#   Trailing '.' makes a number decimal
#

def argint(s):

    digits = '0123456789'
    if s.startswith('0o'):
        base = 8
        s = s[2:]
        digits = '01234567'
    elif s.startswith('0x'):
        base = 16
        s = s[2:]
        digits += 'abcdef'
    elif s[-1] == '.':
        base = 10
        s = s[:-1]
    else:
        base = 8

    place = 1
    v = 0
    for d in reversed(s.lower()):
        dv = digits.index(d)           # ValueError for bad digits
        v += (dv * place)
        place *= base

    return v


def strfile(s, name):
    return OpenedFileInfo(f=io.StringIO(s), name=name)


def sysinclude():
    return strfile("""
            indir   = 0.
            exit    = 1.
            fork    = 2.
            read    = 3.
            write   = 4.
            open    = 5.
            close   = 6.
            wait    = 7.
            creat   = 8.
            link    = 9.
            unlink  = 10.
            exec    = 11.
            chdir   = 12.
            time    = 13.
            mknod   = 14.
            chmod   = 15.
            chown   = 16.
            break   = 17.
            stat    = 18.
            lseek   = 19.
            getpid  = 20.
            mount   = 21.
            umount  = 22.
            setuid  = 23.
            getuid  = 24.
            stime   = 25.
            ptrace  = 26.
            alarm   = 27.
            fstat   = 28.
            pause   = 29.
            utime   = 30.
            smdate  = 30.
            stty    = 31.
            gtty    = 32.
            access  = 33.
            nice    = 34.
            sleep   = 35.
            sync    = 36.
            kill    = 37.
            csw     = 38.
            setpgrp = 39.
            dup     = 41.
            pipe    = 42.
            times   = 43.
            profil  = 44.
            setgid  = 46.
            getgid  = 47.
            signal  = 48.
            acct    = 51.
            phys    = 52.
            lock    = 53.
            ioctl   = 54.
            reboot  = 55.
            mpx     = 56.
            setinf  = 59.
            umask   = 60.
            getinf  = 60.
    """, "--sys")


if __name__ == "__main__":
    cmd_main()
