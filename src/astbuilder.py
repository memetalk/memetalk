# Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
from pdb import set_trace as br
from mmpprint import P

class ASTBuilder:
    def ast(self, begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[begin:end]
        start_line = full[:begin].count("\n") + self.line_offset
        start_col = begin - full.rfind("\n", 0, begin)-1
        end_line = start_line + text.count("\n")
        inside_nl = text.rfind("\n", 0)
        if inside_nl == -1:
            end_col = start_col + len(text)
        else:
            end_col = len(text[inside_nl:])
        node = ASTNode(ast, text, start_line, start_col, end_line, end_col)
        return node

    def sint_ast(self, last_begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[last_begin:end]
        line = full[:last_begin].count("\n") + self.line_offset+2
        node = ASTNode(ast, text, line, 0, line, 0)
        return node

class ASTNode():
    def __init__(self, lst, text, start_line, start_col, end_line, end_col):
        self.lst = lst
        self.text = text
        self.start_line = start_line
        self.start_col = start_col
        self.end_line = end_line
        self.end_col = end_col

    def __len__(self):
        return len(self.lst)

    def __getitem__(self, key):
        return self.lst[key]

    def __setitem__(self, key, val):
        self.lst[key] = val

    def __delitem__(self, key):
        del self.lst[key]

    def __iter__(self):
        return self.lst.__iter__()

    def __reversed__(self):
        return self.lst.__reversed__()

    def __contains__(self, item):
        return item in self.lst

    def __coerce__(self, other):
        if hasattr(other, '__iter__'):
            return (self, other)
        return None

    def __add__(self, other):
        return self.lst + other

    def __radd__(self, other):
        return other + self.lst

    def __str__(self):
        return self.lst.__str__()

    def __iter__(self):
        return self.lst.__iter__()

    def __repr__(self):
        # this should be easy on the eyes
        # it should be used for stack traces and AST debugging
        return self.lst.__repr__()

    def __eq__(self, other):
        return id(other) == id(self) or\
            id(other) == id(self.lst) or\
            other == self.lst

    def __ne__(self, other):
        return not self.__eq__(other)

    def __getslice__(self,i,j):
        return self.lst.__getslice__(i,j)

    # dshared requires this hack
    def __getattr__(self, name):
        return self.__dict__[name]

    # ...and this hack...
    def __setattr__(self, name, val):
        self.__dict__[name] = val
