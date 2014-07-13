from . import bits
from . import opcode
from . import FRAME_TYPE_OBJECT, FRAME_TYPE_LITERAL_FRAME, FRAME_TYPE_BYTECODE_FRAME

from . import behavior_label, cclass_label, class_label, cfun_label, fun_label, cmod_label, mod_label
from pdb import set_trace as br

class Entry(object):
    def __init__(self):
        self.oop = None
    # def get_oop(self, vmem, dont_load=True):
    #     if self.oop is None:
    #         if dont_load:
    #             raise Exception('get_oop called prior to having an oop')
    #         self.oop = self.fill(vmem)
    #     return self.oop

class Object(Entry):
    def __init__(self, name):
        super(Object, self).__init__()
        self.name = name
        self.slots = []

    def label(self):
        return self.name

    def add_slot_literal_array(self, name, value):
        if len(value) > 0:
            raise Exception('todo')
        self.add_slot_empty_list(name)

    def add_slot_literal_dict(self, name, value):
        if len(value) > 0:
            raise Exception('todo')
        self.add_slot_empty_dict(name)

    def add_slot_ref(self, name, value):
        self.slots.append({'type': 'ref', 'name': name, 'value': value})

    def add_slot_literal_string(self, name, value):
        self.slots.append({'type': 'string', 'name': name, 'value': value})

    def add_slot_empty_dict(self, name):
        # TODO: this will become dict
        self.slots.append({'type': 'empty_dict', 'name': name})

    def add_slot_empty_list(self, name):
        # TODO: this will become list
        self.slots.append({'type': 'empty_list', 'name': name})

    def add_slot_literal_null(self, name):
        self.slots.append({'type': 'null', 'name': name})

    def add_slot_literal_num(self, name, value):
        self.slots.append({'type': 'int', 'name': name, 'value': value})

    def fill(self, vmem):
        # synth necessary objects:
        refs_to_literals = {}
        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'string':
                refs_to_literals[idx] = vmem.append_string_instance(slot['value'])
            elif slot['type'] == 'empty_list':
                refs_to_literals[idx] = vmem.append_empty_list()
            elif slot['type'] == 'empty_dict':
                refs_to_literals[idx] = vmem.append_empty_dict()

        # emit our object

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(len(self.slots) * bits.WSIZE)

        oop = vmem.append_label_ref(self.slots[0]['value'], self.name)

        for idx, slot in enumerate(self.slots[1:]):
            if slot['type'] == 'ref':
                vmem.append_label_ref(slot['value'])
            elif slot['type'] == 'null':
                vmem.append_null()
            elif slot['type'] == 'string':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_list':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'empty_dict':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'int':
                vmem.append_tagged_int(slot['value'])
            else:
                raise Exception('TODO')
        self.oop = oop
        return oop


class Behavior(Entry):
    def __init__(self, name, parent_name): #, dictionary):
        super(Behavior, self).__init__()
        self.name = name
        self.parent_name = parent_name
        self.parent_label = behavior_label(parent_name)
        # self.dictionary = dictionary
        if parent_name != 'Object':
            raise Exception('TODO')

    def label(self):
        return behavior_label(self.name)

    def fill(self, vmem):
        oop_dict = vmem.append_empty_dict()

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(4 * bits.WSIZE)

        oop = vmem.append_label_ref('Behavior', self.label())       # vt
        vmem.append_label_ref(self.parent_label)                  # delegate
        vmem.append_pointer_to(oop_dict)                          # dict: "own methods"
        vmem.append_int(256)                                      # size: 256 = dummy flag: "I am a behavior. no more fields after this"
        # no compiled_class
        self.oop = oop
        return oop


class Class(Entry):
    def __init__(self, imod, cclass):
        super(Class, self).__init__()
        self.imod = imod
        self.behavior = Behavior(cclass.name, cclass.super_name)
        self.cclass = cclass
        self.dictionary = {}

    def label(self):
        return class_label(self.cclass.name)

    def new_instance_method(self, name):
        cfun = self.cclass.new_instance_method(name)
        fun = Function(self.imod, cfun)
        self.dictionary[name] = fun
        return fun

    def fill(self, vmem):
        oop_vt = self.behavior.fill(vmem)

        oop_dict = vmem.append_sym_dict_emiting_entries(self.dictionary)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(5 * bits.WSIZE)

        # oop = vmem.append_label_ref(behavior_label(self.cclass.name),  self.label())  # vt
        oop = vmem.append_pointer_to(oop_vt, self.label()) # vt
        vmem.append_label_ref(class_label(self.cclass.super_name))                    # delegate
        vmem.append_pointer_to(oop_dict)                                              # dict: "methods"
        vmem.append_int(len(self.cclass.fields))                                      # payload
        vmem.append_label_ref(cclass_label(self.cclass.name))                       # compiled_class
        # vmem.append_pointer_to(self.cclass.oop)                                 # <-
        self.oop = oop
        return oop



class CompiledClass(Entry):
    def __init__(self, cmod, name, super_name, fields):
        super(CompiledClass, self).__init__()
        self.cmod = cmod
        self.name = name
        self.super_name = super_name
        self.fields = fields
        self.instance_methods = {}
        self.class_methods = {}

    def label(self):
        return cclass_label(self.name) #cmod.label() + '_' + self.name + "_CompiledClass"

    def new_ctor(self, name):
        fn = CompiledFunction(self.cmod, self, name, ctor=True)
        self.class_methods[name] = fn
        return fn

    def new_instance_method(self, name):
        fn = CompiledFunction(self.cmod, self, name)
        self.instance_methods[name] = fn
        return fn

    def new_class_method(self, name):
        fn = CompiledFunction(self.cmod, self, name)
        self.class_methods[name] = fn
        return fn

    def fill(self, vmem):
        # vt: CompiledClass
        # delegate: ...
        # module: ...
        # name: ...
        # super_class_name: ...
        # fields
        # methods
        # class_methods ...

        delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_super = vmem.append_string_instance(self.super_name)
        oop_fields = vmem.append_list_of_strings(self.fields)
        oop_methods = vmem.append_dict_emiting_entries(self.instance_methods)
        oop_class_methods = vmem.append_dict_emiting_entries(self.class_methods)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(8 * bits.WSIZE)

        oop = vmem.append_external_ref('CompiledClass', self.label()) # vt: CompiledClass
        vmem.append_pointer_to(delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_super)
        vmem.append_pointer_to(oop_fields)
        vmem.append_pointer_to(oop_methods)
        vmem.append_pointer_to(oop_class_methods)
        vmem.append_label_ref(self.cmod.label()) ####
        self.oop = oop
        return oop

class CompiledFunction(Entry):
    def __init__(self, cmod, owner, name, ctor=False):
        super(CompiledFunction, self).__init__()
        self.cmod = cmod
        self.name = name
        self.literal_frame = []
        self.bytecodes = []
        self.is_prim = False
        self.prim_name = ''
        self.owner = owner
        self.is_ctor = ctor
        self.has_env = False
        self.local_vars = []

    def body_processor(self):
        return self

    def uses_env(self, val):
        self.has_env = val

    def set_primitive(self, prim_name):
        self.prim_name = prim_name
        self.is_prim = True

    def label(self):
        return self.owner.label() + '_' + self.name + "_CompiledFunction"

    def literal_frame_label(self):
        return self.label() + '_literal_frame'

    def bytecode_label(self):
        return self.label() + '_bytecodes'

    def fill_literal_frame(self, vmem):
        if len(self.literal_frame) == 0:
            return 0

        # pre-append objects
        for lit in self.literal_frame:
            if lit['tag'] == 'number':
                pass
            elif lit['tag'] == 'symbol':
                lit['oop'] = vmem.append_symbol_instance(lit['value'])
            else:
                raise Exception('Todo')

        vmem.append_int(FRAME_TYPE_LITERAL_FRAME)
        vmem.append_int(len(self.literal_frame) * bits.WSIZE)

        vmem.label_current(self.literal_frame_label())

            # if lit['tag'] == 'number':
            #     oop = vmem.append_int(bits.tag(lit['value']))
            #     lit_frame.append(oop)


        # fill frame
        for lit in self.literal_frame:
            if lit['tag'] == 'number':
                vmem.append_int(bits.tag(lit['value']))
            elif lit['tag'] == 'symbol':
                vmem.append_pointer_to(lit['oop'])
            else:
                raise Exception('Todo')

        return len(self.literal_frame) * bits.WSIZE


    def fill_bytecodes(self, vmem):
        if len(self.bytecodes) == 0:
            return 0

        bytecodes = ''.join([bits.pack32(w) for w in self.bytecodes])
        vmem.append_int(FRAME_TYPE_BYTECODE_FRAME)
        vmem.append_int(bits.string_block_size('x' * ((len(self.bytecodes) * opcode.WORD_SIZE) + 1)))

        vmem.label_current(self.bytecode_label())
        vmem.append_string(bytecodes)
        return len(self.bytecodes) * opcode.WORD_SIZE

    def fill(self, vmem):
        # vt: CompiledFunction
        # delegate
        # name
        # params
        # is_ctor
        # is_prim
        # prim_name
        # flags: normal, setter, getter
        # getter/setter field index
        # owner
        # num_locals / env_size
        # literal frame size
        # literal_frame ptr
        # bytecode size
        # bytecode ptr

        # todo:
        #   uses env / env size
        #   num_locals

        oop_delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_params = vmem.append_list_of_strings(self.params)
        oop_prim_name = vmem.append_string_instance(self.prim_name)

        lit_frame_size = self.fill_literal_frame(vmem)
        bytecode_size = self.fill_bytecodes(vmem)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(16 * bits.WSIZE)

        oop = vmem.append_external_ref('CompiledFunction', self.label()) # CompiledFunction vt
        vmem.append_pointer_to(oop_delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_params)
        vmem.append_int(int(self.is_ctor))
        vmem.append_int(int(self.is_prim))
        vmem.append_pointer_to(oop_prim_name)

        vmem.append_int(0) # normal=0/getter=1/setter=2
        vmem.append_int(0) # getter/setter field index

        vmem.append_label_ref(self.owner.label())

        vmem.append_int(len(self.params))
        if self.has_env:
            raise Exception('Todo')
        else:
            vmem.append_int(len(self.local_vars))

        vmem.append_int(lit_frame_size)
        if lit_frame_size > 0:
            vmem.append_label_ref(self.literal_frame_label())
        else:
            vmem.append_null()

        vmem.append_int(bytecode_size)
        if bytecode_size > 0:
            vmem.append_label_ref(self.bytecode_label())
        else:
            vmem.append_null()

        self.oop = oop
        return oop

    def set_parameters(self, params):
        self.params = params

    def identifier_is_in_current_block(self, name):
        return name in self.local_vars or name in self.params

    def identifier_is_module_scoped(self, name):
        return name in self.cmod.classes or name in self.cmod.functions or name in self.cmod.params

    def index_for_literal(self, entry):
        if entry not in self.literal_frame:
            self.literal_frame.append(entry)
        return self.literal_frame.index(entry)

    def create_and_register_number_literal(self, num):
        entry = {"tag": "number", "value": num}
        return self.index_for_literal(entry)

    def create_and_register_symbol_literal(self, string):
        entry = {"tag": "symbol", "value": string}
        return self.index_for_literal(entry)

    def index_and_popop_for(self, name):
        if name in self.params:
            return 'pop_param', self.params.index(name)
        if name in self.local_vars:
            return 'pop_local', self.local_vars.index(name)
        else:
            raise Exception('todo')

    def index_and_pushop_for(self, name):
        if name in self.params:
            return 'push_param', self.params.index(name)
        if name in self.local_vars:
            return 'push_local', self.local_vars.index(name)
        if name in self.cmod.classes:
            raise Exception('todo')

    def emit_push_num_literal(self, num):
        idx = self.create_and_register_number_literal(num)
        self.bytecodes.append(opcode.bytecode_for("push_literal", idx))

    def emit_push_var(self, name):
        # `name` may be:
        #   -a local variable (env or stack allocated)      -> push_local / push_env
        #   -a function parameter (env or stack allocated)  -> push_param / push_env
        #   -a module parameter                             -> push_module, push_literal, send
        #   -a module entry (function or class)             -> push_module, push_literal, send

        if self.has_env:
            idx = self.local_vars.index(name)
            self.bytecodes.append(opcode.bytecode_for("push_env",idx))
        elif self.identifier_is_in_current_block(name):
            opname, idx = self.index_and_pushop_for(name)
            self.bytecodes.append(opcode.bytecode_for(opname,idx))
        elif self.identifier_is_module_scoped(name):
            idx = self.create_and_register_symbol_literal(name)
            self.bytecodes.append(opcode.bytecode_for("push_module", 0))
            self.bytecodes.append(opcode.bytecode_for("push_literal", idx))
            self.bytecodes.append(opcode.bytecode_for('send', 0))
        else:
            raise Exception('push_var: undeclared ' + name)

    def emit_return_top(self):
        self.bytecodes.append(opcode.bytecode_for("ret_top",0))

    def emit_return_this(self):
        self.bytecodes.append(opcode.bytecode_for("ret_this",0))

    def emit_var_decl(self, name):
        if self.has_env:
            idx = self.add_env(name)
            self.bytecodes.append(opcode.bytecode_for("pop_env",idx))
        else:
            self.local_vars.append(name)
            opname, idx = self.index_and_popop_for(name)
            self.bytecodes.append(opcode.bytecode_for(opname,idx))

    def emit_send_or_local_call(self, name, arity):
        if name in self.local_vars:
            raise Exception('todo')
        elif name in self.params:
            raise Exception('todo')
        elif name in self.cmod.functions:
            idx = self.create_and_register_symbol_literal(name)
            self.bytecodes.append(opcode.bytecode_for('push_module', 0))
            self.bytecodes.append(opcode.bytecode_for('push_literal', idx))
            self.bytecodes.append(opcode.bytecode_for('send', arity))
        elif name in self.cmod.params:
            raise Exception('todo')
        else:
            raise Exception('todo')
        # if name in module.params ...
        # if name in module entries
        # if name in self.params ...
        # if name in self.local vars ...
        # if name in self.env ...

    def emit_send(self, selector, arity):
        idx = self.create_and_register_symbol_literal(selector)
        self.bytecodes.append(opcode.bytecode_for('push_literal', idx))
        self.bytecodes.append(opcode.bytecode_for('send', arity))


class Function(Entry):
    def __init__(self, imod, cfun):
        super(Function, self).__init__()
        self.imod = imod
        self.cfun = cfun

    def body_processor(self):
        return self.cfun

    def set_parameters(self, params):
        self.cfun.set_parameters(params)

    def uses_env(self, val):
        self.cfun.uses_env(val)

    def set_primitive(self, prim_name):
        self.cfun.set_primitive(prim_name)

    def label(self):
        return self.cfun.name

    def fill(self, vmem):
        delegate = vmem.append_object_instance()

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(4 * bits.WSIZE)

        oop = vmem.append_label_ref('Function', self.label())   # vt
        vmem.append_pointer_to(delegate)                        # delegate
        vmem.append_label_ref(self.cfun.label())                # compiled_function
        vmem.append_label_ref(self.imod.label())                # module
        self.oop = oop
        return oop


class CompiledModule(Entry):
    def __init__(self, name):
        super(CompiledModule, self).__init__()
        self.name = name
        self.params = []
        self.functions = {}
        self.classes = {}

    def entry_labels(self):
        return self.functions.keys() + self.classes.keys()

    def label(self):
        return cmod_label(self.name)

    # def num_top_level_entries(self):
    #     return len(self.functions) + len(self.classes)

    # def classes_and_functions_names(self):
    #     return self.functions.keys() + self.classes.keys()

    def new_function(self, name):
        fn = CompiledFunction(self, self, name)
        self.functions[name] = fn
        return fn

    def new_class(self, name, parent, fields):
        klass = CompiledClass(self, name, parent, fields)
        self.classes[name] = klass
        return klass

    def fill(self, vmem):
        vmem.append_label_ref(self.label())

        # first word on object table is a pointer to the CompiledModule
        delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_license = vmem.append_string_instance("")
        oop_params = vmem.append_list_of_strings([])
        oop_functions = vmem.append_dict_emiting_entries(self.functions)
        oop_classes = vmem.append_dict_emiting_entries(self.classes)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(8 * bits.WSIZE)

        oop = vmem.append_external_ref('CompiledModule', self.label()) # vt: CompiledModule
        vmem.append_pointer_to(delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_license)
        vmem.append_pointer_to(oop_params)
        vmem.append_pointer_to(oop_functions)
        vmem.append_pointer_to(oop_classes)
        vmem.append_null()                        # parent_module
        self.oop = oop
        return oop

class CoreModule(Entry):
    # core's Module instance (conflated approach)
    def __init__(self, cmod):
        super(CoreModule, self).__init__()
        self.cmod = cmod
        self.functions = {}
        self.classes = []
        self.objects = []

    def append_class(self, klass):
        self.classes.append(klass)

    def append_function(self, name, fun):
        self.functions[name] = fun

    def append_object(self, obj):
        self.objects.append(obj)

    def entry_labels(self):
        top_objects = ['Behavior', 'Object_Behavior', 'Object']
        res = [f.label() for f in self.functions.values()]
        res += [c.label() for c in self.classes]
        res += [o.label() for o in self.objects if o.label() in top_objects]
        return res + [self.label()]

    def label(self):
        return '@core_module'

    def fill(self, vmem):
        oop_dict = vmem.append_sym_dict_emiting_entries(self.functions)

        obj_oops = [o.fill(vmem) for o in self.objects]
        class_oops = [c.fill(vmem) for c in self.classes]

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(4 * bits.WSIZE)

        oop = vmem.append_label_ref(self.label(), self.label()) # vt
        vmem.append_null()                                      # delegate
        vmem.append_pointer_to(oop_dict)                        # dict
        vmem.append_label_ref(self.cmod.label())                # compiled_module
        # for klass in self.compiler.top_level_classes:
        #     vmem.append_label_ref(klass.label)
        self.oop = oop
        return oop


class CoreCompiledModule(CompiledModule):
    def __init__(self):
        super(CoreCompiledModule, self).__init__('core')
        self.imod = CoreModule(self)

    def entry_labels(self):
        return self.imod.entry_labels()

    def new_function(self, name):
        cfun = super(CoreCompiledModule, self).new_function(name)
        fn = Function(self.imod, cfun)
        self.imod.append_function(name, fn)
        return fn

    def new_class(self, name, parent, fields):
        cclass = super(CoreCompiledModule, self).new_class(name, parent, fields)
        klass = Class(self.imod, cclass)
        self.imod.append_class(klass)
        return klass

    def new_object(self, name):
        obj = Object(name)
        self.imod.append_object(obj)
        return obj

    def fill(self, vmem):
        super(CoreCompiledModule, self).fill(vmem)
        self.imod.fill(vmem)
