import math
import struct
from pdb import set_trace as br

WSIZE = 8

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


def atoword(bytelist):
    if WSIZE == 4:
        return struct.unpack('I', ''.join(map(chr, bytelist)))[0]
    elif WSIZE == 8:
        return struct.unpack('Q', ''.join(map(chr, bytelist)))[0]
    else:
        raise Exception('Not implemented')

def chunks(l, n):
    res = []
    for i in xrange(0, len(l), n):
        res.append(l[i:i+n])
    return res

def unpack(string_word):
    if WSIZE == 4:
        return struct.unpack('I', string_word)[0]
    elif WSIZE == 8:
        return struct.unpack('Q', string_word)[0]
    else:
        raise Exception('Not implemented')

def unpack_tagged(string_word):
    if WSIZE == 4:
        return untag(struct.unpack('I', string_word)[0])
    elif WSIZE == 8:
        return untag(struct.unpack('Q', string_word)[0])
    else:
        raise Exception('Not implemented')

def pack_word(num):
    if WSIZE == 4:
        return struct.pack('I', num)
    elif WSIZE == 8:
        return struct.pack('Q', num)
    else:
        raise Exception('Not implemented')

def pack32(num):
    return struct.pack('I', num)

def pack_byte(num):
    return struct.pack('B', num)

def untag(num):
    if WSIZE == 4:
        return num & 0x7FFFFFFF
    elif WSIZE == 8:
        return num & 0x7FFFFFFFFFFFFFFF
    else:
        raise Exception('Not implemented')

def tag(num):
    if WSIZE == 4:
        return num | 0x80000000
    elif WSIZE == 8:
        return num | 0x8000000000000000
    else:
        raise Exception('Not implemented')

def bytelist(num):
    if WSIZE == 4:
        return map(ord, struct.pack('I', num))
    elif WSIZE == 8:
        return map(ord, struct.pack('Q', num))
    else:
        raise Exception('Not implemented')

def bytelist_tag(num):
    if WSIZE == 4:
        return map(ord, struct.pack('I', tag(num)))
    elif WSIZE == 8:
        return map(ord, struct.pack('Q', tag(num)))
    else:
        raise Exception('Not implemented')

def string_block_size(string):
    # number of bytes required for string, aligned to word size
    return int(math.ceil(len(string) / float(WSIZE)) * WSIZE)
