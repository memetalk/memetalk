# -*- coding: utf-8 -*-
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

from mmpprint import P
from collections import Iterable
from pdb import set_trace as br
import dshared

class Dict(dict):
    pass

def obj_eq(a,b):
    if id(a) == id(b):
        return True

    if isinstance(a, Iterable) and isinstance(b, Iterable):
        if '@id' in a and '@id' in b:
            return a['@id'] == b['@id']

        if isinstance(a, dshared.list):
            a = list(a)
        if isinstance(b, dshared.list):
            b = list(b)
    return a == b

# ID eq
def id_eq(a,b):
    assert(isinstance(a, Iterable))
    assert(isinstance(b, Iterable))

    if id(a) == id(b):
        return True
    if '@id' in a and '@id' in b:
        return a['@id'] == b['@id']
    return a == None and b == None
