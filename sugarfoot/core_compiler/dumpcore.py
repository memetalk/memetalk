from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
from pprint import pprint as P
from pdb import set_trace as br
import struct

WSIZE = 4

class ObjectEntry(object):
    def __init__(self, name):
        self.name = name
        self.slots = []

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ptr', 'name': name, 'value': value})

    def add_slot_literal_string(self, name, value):
        self.slots.append({'type': 'ptr', 'name': name, 'value': value})

    def add_slot_dict(self, name, value):
        self.slots.append({'type': 'ptr', 'name': name, 'value': value})

    def add_slot_list(self, name, value):
        self.slots.append({'type': 'ptr', 'name': name, 'value': value})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'ptr', 'name': name})

    def add_slot_literal_num(self, name, value):
        self.slots.append({'type': 'int', 'name': name, 'value': value})

    def dump(self, dec, ptr):
        print '[{}] -- {}'.format(ptr, self.name)
        obj_size = WSIZE * len(self.slots)
        objs = map(unpack, chunks(dec.file_contents[ptr:ptr+obj_size], WSIZE))

        for idx, slot in enumerate(self.slots):
            if slot['type'] == 'ptr':
                target_name = dec.get_entry_name(objs[idx])
                if target_name:
                    print '{}: #{} <{}>'.format(slot['name'], target_name, objs[idx])
                else:
                    print '{}: <{}>'.format(slot['name'], objs[idx])
            else:
                print '{}: {}'.format(slot['name'], objs[idx])
        return obj_size

class ClassEntry(object):
    def __init__(self, name, super_class):
        self.name = name
        self.super_class = super_class
        self.fields = []
        if super_class != 'Object':
            raise Exception('TODO')

    def set_fields(self, fields):
        self.fields = fields

    def dump(self, dec, ptr):
        br()

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


class Decompiler(ASTBuilder):
    def __init__(self):
        self.entries = {}
        self.current_object = None

    def decompile(self):

        # loading core.md source structure

        self.line_offset = 0
        self.parser = MemeParser(open('core.md').read())
        self.parser.i = self
        ast = self.parser.apply("start")[0]
        self.parser = CoreTr([ast])
        self.parser.i = self
        self.parser.apply('start')

        # loading binary image

        self.load_image()

    def get_entry_name(self, vt_ptr):
        for name_ptr, obj_ptr in chunks([unpack(pack32) for pack32 in chunks(self.INDEX, 4)], 2):
            name = self.file_contents[name_ptr:self.file_contents.find('\0', name_ptr)]
            # print 'get_entry_name({}): {}/{} = {}'.format(vt_ptr, name_ptr, obj_ptr, name)
            if obj_ptr == vt_ptr:
                return name
        return None

    def load_image(self):

        END_OF_HEADER = 12

        self.file_contents = open("core.img", "rb").read()

        header_packs = [unpack(pack32) for pack32 in chunks(self.file_contents[0:END_OF_HEADER], 4)]

        header = {'entries': header_packs[0],
                  'names_size': header_packs[1],
                  'ot_size': header_packs[2]}

        # NAMES
        start_names_section = END_OF_HEADER
        end_names_section = start_names_section + header['names_size']
        self.NAMES = self.file_contents[start_names_section:end_names_section]

        # INDEX
        start_index_section = end_names_section
        end_index_section = start_index_section + header['entries'] * 2 * 4 # *2: pairs, *4: 32 bits
        self.INDEX = self.file_contents[start_index_section:end_index_section]

        # OT
        start_ot_section = end_index_section
        end_ot_section = start_ot_section + header['ot_size']
        self.OT = self.file_contents[start_ot_section:end_ot_section]

        # ADDR
        start_addr_section = end_ot_section
        end_addr_section = len(self.file_contents)
        self.ADDR = self.file_contents[start_addr_section:end_addr_section]

        self.relloc_addresses = [unpack(pack32) for pack32 in chunks(self.ADDR, 4)]

        ###########
        idx = 0
        while True:
            # vt_ptr = unpack(self.file_contents[start_ot_section + idx:start_ot_section + idx+4])
            # 432
            # print start_ot_section + idx
            name = self.get_entry_name(start_ot_section + idx)
            if name is None:
                self.dump_unknown(start_ot_section + idx)
                idx += WSIZE
            elif name in self.entries:
                idx += self.entries[name].dump(self, start_ot_section + idx)
            else:
                raise 'No entry for dumping {}'.format(name)
            print '--------------------'

    def dump_unknown(self, addr):
        print '{}: [{}]'.format(addr, unpack(self.file_contents[addr:addr+4]))

    ######################

    def register_object(self, name):
        self.current_object = ObjectEntry(name)
        self.entries[name] = self.current_object

    def register_class(self, name, super_class):
        self.current_class = ClassEntry(name, super_class)
        self.entries[name] = self.current_class

    def add_class_fields(self, fields):
        self.current_class.set_fields(fields)

    def add_slot_ref(self, name, value):
        self.current_object.add_slot_ref(name, value)

    def add_slot_literal_null(self, name):
        self.current_object.add_slot_literal_null(name)

    def add_slot_literal_num(self, name, value):
        self.current_object.add_slot_literal_num(name, value)

    def add_slot_literal_string(self, name, value):
        self.current_object.add_slot_literal_string(name, value)

    def add_slot_literal_array(self, name, value):
        self.current_object.add_slot_list(name, value)

    def add_slot_literal_dict(self, name, value):
        self.current_object.add_slot_dict(name, value)


Decompiler().decompile()
