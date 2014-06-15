from parser import MemeParser
from coretr import CoreTr
from astbuilder import *
from pprint import pprint as P
from pdb import set_trace as br
import math
import os
from . import utils


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
        obj_size = utils.WSIZE * len(self.slots)
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))

        for idx, slot in enumerate(self.slots):
            if slot['type'] == 'ptr':
                target_name = dec.get_entry_name(objs[idx])
                if target_name:
                    print '{}: #{} <{}>'.format(slot['name'], target_name, objs[idx])
                else:
                    print '{}: <{}>'.format(slot['name'], objs[idx])
            elif slot['type'] == 'int':
                print '{}: {}'.format(slot['name'], utils.untag(objs[idx]))
            else:
                print '{}: {}'.format(slot['name'], objs[idx])
        return obj_size

class ClassBehaviorEntry(object):
    def __init__(self, name, super_class):
        self.name = utils.behavior_label(name)
        self.super_class = super_class

    def dump(self, dec, ptr):
        print '[{}] -- {}'.format(ptr, self.name)
        slots = ['vt', 'delegate', 'parent', 'dict']
        obj_size = utils.WSIZE * len(slots)
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))
        for idx, slot in enumerate(slots):
            target_name = dec.get_entry_name(objs[idx])
            if target_name:
                print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
            else:
                print '{}: <{}>'.format(slot, objs[idx])
        return obj_size


class CompiledClassEntry(object):
    def __init__(self, name, super_class):
        self.name = utils.cclass_label(name)
        self.super_class = super_class

    def dump(self, dec, ptr):
        print '[{}] -- {}'.format(ptr, self.name)
        slots = ['vt', 'delegate', 'name', 'super_class_name',
                 'compiled_module', 'fields', 'methods', 'own_methods']
        obj_size = utils.WSIZE * len(slots)
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))
        for idx, slot in enumerate(slots):
            target_name = dec.get_entry_name(objs[idx])
            if target_name:
                print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
            else:
                print '{}: <{}>'.format(slot, objs[idx])
        return obj_size

class ClassEntry(object):
    def __init__(self, name, super_class, behavior, cclass):
        self.name = name
        self.super_class = super_class
        self.behavior = behavior
        self.cclass = cclass
        self.fields = []
        self.methods = []
        if super_class != 'Object':
            raise Exception('TODO')

    def set_fields(self, fields):
        self.fields = fields

    def add_method(self, name):
        self.methods.append(name)

    def dump(self, dec, ptr):
        print '[{}] -- {}'.format(ptr, self.name)
        slots = ['vt', 'delegate', 'parent', 'dict', 'compiled_class']
        obj_size = utils.WSIZE * len(slots) # total slots in a class object
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))
        for idx, slot in enumerate(slots):
            target_name = dec.get_entry_name(objs[idx])
            if target_name:
                print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
            else:
                print '{}: <{}>'.format(slot, objs[idx])
        return obj_size


class CoreCompiledModule(object):
    def dump(self, dec, ptr):
        print '[{}] {} instance'.format(ptr, '@core_compiled_module')
        slots = ['vt', 'delegate', 'name', 'license', 'params',
                 'compiled_functions', 'compiled_classes', 'parent_module']
        obj_size = utils.WSIZE * len(slots) # total slots in a class object
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))
        for idx, slot in enumerate(slots):
            target_name = dec.get_entry_name(objs[idx])
            if target_name:
                print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
            else:
                print '{}: <{}>'.format(slot, objs[idx])
        return obj_size

class CoreModule(object):
    def dump(self, dec, ptr):
        print '[{}] {} instance'.format(ptr, '@core_module')
        slots = ['vt', 'delegate', 'parent', 'dict', 'compiled_module']
        obj_size = utils.WSIZE * len(slots) # total slots in a class object
        objs = map(utils.unpack, utils.chunks(dec.file_contents[ptr:ptr+obj_size], utils.WSIZE))
        for idx, slot in enumerate(slots):
            target_name = dec.get_entry_name(objs[idx])
            if target_name:
                print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
            else:
                print '{}: <{}>'.format(slot, objs[idx])
        return obj_size

def dump_object_instance(dec, addr, class_or_behavior_name):
    print '[{}] {} instance'.format(addr, class_or_behavior_name)
    return 2 * utils.WSIZE #vt+delegate

def _str_from_string_instance(dec, addr):
    size = utils.unpack_tagged(dec.file_contents[addr+8:addr+12])
    string = dec.file_contents[addr+12:addr+12+size]
    chunk_size = int(math.ceil((len(string)+1) / float(utils.WSIZE)) * utils.WSIZE)
    return string, chunk_size

def dump_string_instance(dec, addr, class_or_behavior_name):
    print '[{}] {} instance'.format(addr, class_or_behavior_name)
    string, chunk_size = _str_from_string_instance(dec, addr)
    print '  *** "{}"'.format(string)
    return (3 * utils.WSIZE) + chunk_size

def dump_dictionary_instance(dec, addr, class_or_behavior_name):
    size = utils.unpack_tagged(dec.file_contents[addr+8:addr+12])
    if size == 0:
        print '[{}] {} instance (empty)'.format(addr, class_or_behavior_name)
        return 3 * utils.WSIZE
    else:
        print '[{}] {} instance ({})'.format(addr, class_or_behavior_name, size)
        objs = map(utils.unpack, utils.chunks(dec.file_contents[addr+12:addr+12+((size*2)*utils.WSIZE)], utils.WSIZE))
        for key_oop, val_oop in utils.chunks(objs,2):
            string, _ = _str_from_string_instance(dec, key_oop)
            print ' ** d[{}:{}] = <{}>'.format(key_oop, string, val_oop)
        return (3 * utils.WSIZE) + len(objs) * utils.WSIZE

def dump_compiled_function_instance(dec, addr, class_or_behavior_name):
    print '[{}] {} instance'.format(addr, class_or_behavior_name)
    slots = ['vt', 'delegate', 'name', 'params', 'prim_name', 'owner']
    obj_size = utils.WSIZE * len(slots) # total slots in a class object
    objs = map(utils.unpack, utils.chunks(dec.file_contents[addr:addr+obj_size], utils.WSIZE))
    for idx, slot in enumerate(slots):
        target_name = dec.get_entry_name(objs[idx])
        if target_name:
            print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
        else:
            print '{}: <{}>'.format(slot, objs[idx])
    return obj_size

def dump_function_instance(dec, addr, class_or_behavior_name):
    print '[{}] {} instance'.format(addr, class_or_behavior_name)
    slots = ['vt', 'delegate', 'compiled_function', 'module']
    obj_size = utils.WSIZE * len(slots) # total slots in a class object
    objs = map(utils.unpack, utils.chunks(dec.file_contents[addr:addr+obj_size], utils.WSIZE))
    for idx, slot in enumerate(slots):
        target_name = dec.get_entry_name(objs[idx])
        if target_name:
            print '{}: #{} <{}>'.format(slot, target_name, objs[idx])
        else:
            print '{}: <{}>'.format(slot, objs[idx])
    return obj_size


def dump_list_instance(dec, addr, class_or_behavior_name):
    size = utils.unpack_tagged(dec.file_contents[addr+8:addr+12])
    if size == 0:
        print '[{}] {} instance (empty)'.format(addr, class_or_behavior_name)
        return 3 * utils.WSIZE
    else:
        print '[{}] {} instance ({})'.format(addr, class_or_behavior_name, size)
        return (3 + size) * utils.WSIZE

class Decompiler(ASTBuilder):
    def __init__(self):
        self.entries = {'@core_compiled_module': CoreCompiledModule(),
                        '@core_module': CoreModule()}
        self.current_object = None

    def decompile(self):

        # loading core.md source structure

        self.line_offset = 0
        self.parser = MemeParser(open(os.path.join(os.path.dirname(__file__), 'core.md'), 'r').read())
        self.parser.i = self
        ast = self.parser.apply("start")[0]
        self.parser = CoreTr([ast])
        self.parser.i = self
        self.parser.apply('start')

        # loading binary image

        self.load_image()

    def dump_names(self):
        print '** NAMES **'
        for name_ptr, obj_ptr in utils.chunks([utils.unpack(pack32) for pack32 in utils.chunks(self.INDEX, 4)], 2):
            name = self.file_contents[name_ptr:self.file_contents.find('\0', name_ptr)]
            print name, obj_ptr

        print '======================='

    def get_entry_name(self, vt_ptr):
        for name_ptr, obj_ptr in utils.chunks([utils.unpack(pack32) for pack32 in utils.chunks(self.INDEX, 4)], 2):
            name = self.file_contents[name_ptr:self.file_contents.find('\0', name_ptr)]
            # print 'get_entry_name({}): {}/{} = {}'.format(vt_ptr, name_ptr, obj_ptr, name)
            if obj_ptr == vt_ptr:
                return name
        return None

    def load_image(self):

        END_OF_HEADER = 12

        self.file_contents = open("core.img", "rb").read()

        header_packs = [utils.unpack(pack32) for pack32 in utils.chunks(self.file_contents[0:END_OF_HEADER], 4)]

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

        self.relloc_addresses = [utils.unpack(pack32) for pack32 in utils.chunks(self.ADDR, 4)]

        print header
        self.dump_names()

        ###########
        idx = 0
        while True:
            current_addr = start_ot_section + idx

            if current_addr == end_ot_section:
                break

            # Entry classes om self.entries are for named objects in the index
            name = self.get_entry_name(current_addr)
            if name is None:
                idx += self.dump_anonymous(current_addr)
            elif name in self.entries:
                idx += self.entries[name].dump(self, current_addr)
            else:
                raise Exception('No entry for dumping {}'.format(name))
            print '--------------------'

    def dump_anonymous(self, addr):
        vt_addr = utils.unpack(self.file_contents[addr:addr+4])
        class_or_behavior_name = self.get_entry_name(vt_addr)
        if class_or_behavior_name is None:
            br()
        else:
            if class_or_behavior_name == 'Object':
                return dump_object_instance(self, addr, class_or_behavior_name)
            elif class_or_behavior_name == 'String':
                return dump_string_instance(self, addr, class_or_behavior_name)
            elif class_or_behavior_name == 'List':
                return dump_list_instance(self, addr, class_or_behavior_name)
            elif class_or_behavior_name == 'Dictionary':
                return dump_dictionary_instance(self, addr, class_or_behavior_name)
            elif class_or_behavior_name == 'Behavior':
                br()
            elif class_or_behavior_name == 'CompiledFunction':
                return dump_compiled_function_instance(self, addr, class_or_behavior_name)
            elif class_or_behavior_name == 'Function':
                return dump_function_instance(self, addr, class_or_behavior_name)
            else:
                br()
    ######################

    def register_object(self, name):
        self.current_object = ObjectEntry(name)
        self.entries[name] = self.current_object

    def register_class(self, name, super_class):
        # registering the behavior for that class
        behavior = ClassBehaviorEntry(name, super_class)
        self.entries[behavior.name] = behavior

        # registering the compiled class for that class
        cclass = CompiledClassEntry(name, super_class)
        self.entries[cclass.name] = cclass

        self.current_class = ClassEntry(name, super_class, behavior, cclass)
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

    def add_class_method(self, name, params, body_ast):
        self.current_class.add_method(name)

    def add_module_function(self, name, params, body_ast):
        pass

Decompiler().decompile()
