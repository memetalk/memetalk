from pyutils import bits
from pprint import pprint as P
from pdb import set_trace as br
import math
import os
import sys

with open(sys.argv[1], 'r') as fp:
    text = fp.read()
    HEADER_SIZE = bits.WSIZE * 4

    magic = hex(bits.unpack(text[0:bits.WSIZE]))
    ot_size = bits.unpack(text[bits.WSIZE:bits.WSIZE*2])
    es_size = bits.unpack(text[bits.WSIZE*2:bits.WSIZE*3])
    names_size = bits.unpack(text[bits.WSIZE*3:bits.WSIZE*4])

    print 'MAGIC', magic
    print 'ot_size', ot_size
    print 'es_size', es_size
    print 'names_size', names_size

    # names
    begin_names = HEADER_SIZE
    end_names = begin_names + names_size
    names = text[begin_names:end_names]

    # OT
    begin_ot = end_names
    end_ot = begin_ot + ot_size
    ot = text[begin_ot:end_ot]
    print '* OT', map(bits.unpack, bits.chunks(ot, bits.WSIZE)), "\n"

    # external symbols
    begin_es = end_ot
    end_es = begin_es + es_size
    es = text[begin_es:end_es]
    print '* ES',  map(bits.unpack, bits.chunks(es, bits.WSIZE))

    # reloc table
    begin_reloc = end_es
    reloc = text[begin_reloc:]
    print '* RELOC', map(bits.unpack, bits.chunks(reloc, bits.WSIZE))
