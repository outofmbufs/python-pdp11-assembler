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

from dataclasses import dataclass
import typing

# These exceptions are sort-of internal to the parser, in that they are
# caught at upper levels and turned into corresponding syntax errors.


@dataclass
class _ASX(Exception):
    pass


@dataclass
class _UndefinedSymbol(_ASX):
    symname: str               # name of symbol not found
    cyclic: bool = False       # is it part of a mutually-undefined cycle


@dataclass
class _SymbolTypeMismatch(_ASX):
    msg: str = ""


@dataclass
class _IllegalBranch(_ASX):
    msg: str = "too far"
    distance: typing.Optional[int] = None
    node: typing.TypeVar('XNode') = None


@dataclass
class _Alignment(_ASX):
    msg: str = "odd dot"
    node: typing.TypeVar('XNode') = None
