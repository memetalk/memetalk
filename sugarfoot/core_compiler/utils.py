import math
import struct
from pdb import set_trace as br

WSIZE = 4

SEP='_'

def behavior_label(name):
    return name + SEP + 'Behavior'

def cclass_label(name):
    return name + SEP + 'CompiledClass'

def class_label(name):
    return name # + SEP + "Class"

def cfun_label(owner_name, name):
    return owner_name + SEP + name + SEP + 'CompiledFunction'

def fun_label(cfun_label, name):
    return cfun_label + SEP + name + SEP + "Function"


def ato32(chunk):
    return struct.unpack('I', ''.join(map(chr, chunk)))[0]

def chunks(l, n):
    res = []
    for i in xrange(0, len(l), n):
        res.append(l[i:i+n])
    return res

def unpack(pack32):
    return struct.unpack('I', pack32)[0]

def unpack_tagged(pack32):
    return untag(struct.unpack('I', pack32)[0])

def untag(num):
    return num & 0x7FFFFFFF

def pack32(int32):
    return map(ord, struct.pack('I', int32))

def pack32_tag(int32):
    return map(ord, struct.pack('I', int32 | 0x80000000))

def string_block_size(string):
    # number of bytes required for string, aligned to word size
    return int(math.ceil(len(string) / float(WSIZE)) * WSIZE)
