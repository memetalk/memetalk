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
        start_line = full[:begin].count("\n") + self.line_offset+1
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

class ASTNode(list):
    def __init__(self, lst, text, start_line, start_col, end_line, end_col):
        list.__init__(self,lst)
        self.lst = lst
        self.text = text
        self.start_line = start_line
        self.start_col = start_col
        self.end_line = end_line
        self.end_col = end_col

    def __repr__(self):
        # this should be easy on the eyes
        # it should be used for stack traces and AST debugging
        return P(self.lst,1, True)
