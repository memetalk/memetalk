import math
import struct
from pdb import set_trace as br

WSIZE = 4

def ato32(chunk):
    return struct.unpack('I', ''.join(map(chr, chunk)))[0]

def chunks(l, n):
    res = []
    for i in xrange(0, len(l), n):
        res.append(l[i:i+n])
    return res

def unpack(pack32):
    return struct.unpack('I', pack32)[0]

def pack_int32(num):
    return map(ord, struct.pack('I', num))

def string_block(string):
    # number of bytes required for string, aligned to word size
    return int(math.ceil(len(string) / float(WSIZE)) * WSIZE)
