from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
from pprint import pprint as P
from pdb import set_trace as br
import struct


class ObjectEntry(object):
    def __init__(self, name):
        self.name = name
        self.slots = []

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ref', 'name': name, 'value': value})

    def add_slot_literal_string(self, name, value):
        self.slots.append({'type': 'string', 'name': name, 'value': value})

    def add_slot_dict(self, name, value):
        self.slots.append({'type': 'empty_dict', 'name': name, 'value': value})

    def add_slot_list(self, name, value):
        self.slots.append({'type': 'empty_list', 'name': name, 'value': value})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'null', 'name': name})

    def add_slot_literal_num(self, name, value):
        self.slots.append({'type': 'int', 'name': name, 'value': value})

class ClassEntry(object):
    def __init__(self, name, super_class):
        self.name = name
        self.super_class = super_class
        self.fields = []
        if super_class != 'Object':
            raise Exception('TODO')

    def set_fields(self, fields):
        self.fields = fields


class Decompiler(ASTBuilder):
    def __init__(self):
        self.entries = []
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

    def load_image(self):
        def chunks(l, n):
            for i in xrange(0, len(l), n):
                yield l[i:i+n]

        END_OF_HEADER = 12

        file_contents = open("core.img", "rb").read()

        header_packs = [struct.unpack('I', pack32)[0] for pack32 in chunks(file_contents[0:END_OF_HEADER], 4)]

        header = {'entries': header_packs[0],
                  'names_size': header_packs[1],
                  'ot_size': header_packs[2]}

        start_names_section = END_OF_HEADER
        end_names_section = start_names_section + header['names_size']
        # NAMES section = file_contents[start_names_section:end_names_section]
        start_index_section = end_names_section
        end_index_section = start_index_section + header['entries'] * 2 * 4 # *2: pairs, *4: 32 bits
        # INDEX section = file_contents[start_index_section:end_index_section]
        start_ot_section = end_index_section
        end_ot_section = start_ot_section + header['ot_size']
        # OT section = file_contents[start_ot_section:end_ot_section]
        start_addr_section = end_ot_section
        end_addr_section = len(file_contents)
        # ADDR section = file_contents[start_addr_section:end_addr_section]

        relloc_addresses = [struct.unpack('I', pack32)[0] for pack32 in chunks(file_contents[start_addr_section:end_addr_section], 4)]

        br()
    ######################

    def register_object(self, name):
        self.current_object = ObjectEntry(name)
        self.entries.append(self.current_object)

    def register_class(self, name, super_class):
        self.current_class = ClassEntry(name, super_class)
        self.entries.append(self.current_class)

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


    ###########

    def dump(self):
        for entry in self.entries:
            print entry


Decompiler().decompile()
