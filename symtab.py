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

from expression import XNode
from asx import _UndefinedSymbol


class SymbolTable:
    """Symbol table for the assembler."""

    @dataclass
    class SymEntry:
        """Symbol table entry; see Symtab.add_symbol to create one."""
        name: str
        value: typing.Optional[XNode]
        symtab: typing.TypeVar('SymbolTable')
        builtin: bool = False   # has no semantics but helpful for debugging

        def _cyclic(self, loops):
            # recursively examine the symrefs for circular definitions
            if self in loops:
                return True

            if self.value is None:
                return False

            l2 = loops + [self]
            return any(map(
                lambda x: x._symtab[x.name]._cyclic(l2), self.value.symrefs()))

        def resolve(self):
            if not self.value:
                raise _UndefinedSymbol(self.name)

            if self._cyclic([]):
                raise _UndefinedSymbol(self.name, cyclic=True)

            return self.value.resolve()

    class __SymRef(XNode):
        """A reference to a symbol name, usually appearing in an expression."""
        def __init__(self, name, symtab, *, tok=None):
            self.name = name
            self._symtab = symtab
            self.tok = tok

        def __repr__(self):
            return f"SymRef<{self.name}>"

        def symrefs(self):
            return [self]

        def resolve(self):
            try:
                return self._symtab[self.name].value.resolve()
            except AttributeError:
                raise _UndefinedSymbol(self.name) from None

        def byteseq(self):
            return self.resolve().byteseq()

        def clone(self, *args, **kwargs):
            raise TypeError("clone on a SymRef")

    def __init__(self):
        self.symbols = {}

    def __getitem__(self, key):
        return self.symbols[key]

    def __contains__(self, key):
        return key in self.symbols

    def sym_defined(self, name):
        return name in self.symbols and self.symbols[name].value is not None

    def add_symbol(self, name, value=None, builtin=False):
        self.symbols[name] = self.SymEntry(name, value, self, builtin=builtin)

    def ref_symbol(self, name):
        if name not in self.symbols:
            self.add_symbol(name)
        return self.__SymRef(name, self)

    def undefs(self):
        """Return a list of undefined symbols."""
        u = []
        for k in self.symbols:
            try:
                v = self.symbols[k].resolve()
            except _UndefinedSymbol:
                u.append(k)
            else:
                if v.value is None:
                    u.append(k)
        return u
