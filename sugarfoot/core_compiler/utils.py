import math
import struct


WSIZE = 4

def ato32(num):
    if len(num) < 4:
        num += [0] * (4 - len(num))
    return num[0] + (num[1] << 8)  + (num[2] << 16) + (num[3] << 32)

def chunks(l, n):
    res = []
    for i in xrange(0, len(l), n):
        res.append(l[i:i+n])
    return res

def unpack(pack32):
    return struct.unpack('I', pack32)[0]

def string_block(string):
    # number of bytes required for string, aligned to 32 bits (word size)
    return int(math.ceil((len(string)+1) / float(WSIZE)) * WSIZE)
