from . import bits
from . import opcode
from . import (OrderedDict,
               FRAME_TYPE_OBJECT,
               FRAME_TYPE_LITERAL_FRAME,
               FRAME_TYPE_BYTECODE_FRAME,
               FRAME_TYPE_EXCEPTIONS_FRAME)

from . import (behavior_label,
               cclass_label,
               class_label,
               cfun_label,
               fun_label,
               cmod_label,
               mod_label,
               closure_name)

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
    def __init__(self, name, imod):
        super(Object, self).__init__()
        self.name = name
        self.slots = []
        self.imod = imod

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
            elif slot['type'] == 'dict':
                fun_dict = {}
                for name, cfun in slot['value'].items():
                    cfun.fill(vmem)
                    fun_dict[name] = Function(self.imod, cfun)
                refs_to_literals[idx] = vmem.append_sym_dict_emiting_entries(fun_dict)
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
            elif slot['type'] == 'dict':
                vmem.append_pointer_to(refs_to_literals[idx])
            elif slot['type'] == 'int':
                vmem.append_tagged_int(slot['value'])
            else:
                raise Exception('TODO')
        self.oop = oop
        return oop

## these need to know the CompiledFunction owner (which s Object_CompiledClass)

class Object_ObjectBehavior(Object):
    def new_ctor(self, name, params):
        owner = self.imod.object_by_name('Object_CompiledClass')
        d = [s for s in self.slots if s['type'] == 'dict']
        if len(d) == 0:
            dslot = {"type":"dict", name: "dict", 'value':{}}
            self.slots.insert(2,dslot) #dict is slot in idx==2
        else:
            dslot = d[0]

        fn = CompiledFunction(self.imod.cmod, owner, name, params, ctor=True)
        dslot['value'][name] = fn
        return fn

    def new_function(self, name, params):
        owner = self.imod.object_by_name('Object_CompiledClass')
        d = [s for s in self.slots if s['type'] == 'dict']
        if len(d) == 0:
            dslot = {"type":"dict", name: "dict", 'value':{}}
            self.slots.insert(2,dslot) #dict is slot in idx==2
        else:
            dslot = d[0]

        fn = CompiledFunction(self.imod.cmod, owner, name, params)
        dslot['value'][name] = fn
        return fn

class Object_Object(Object):
    def new_function(self, name, params):
        owner = self.imod.object_by_name('Object_CompiledClass')
        d = [s for s in self.slots if s['type'] == 'dict']
        if len(d) == 0:
            dslot = {"type":"dict", name: "dict", 'value':{}}
            self.slots.insert(2,dslot) #dict is slot in idx==2
        else:
            dslot = d[0]

        fn = CompiledFunction(self.imod.cmod, self, name, params)
        dslot['value'][name] = fn
        return fn

class Behavior(Entry):
    def __init__(self, name, parent_name): #, dictionary):
        super(Behavior, self).__init__()
        self.name = name
        self.parent_name = parent_name
        self.parent_label = behavior_label(parent_name)
        self.dictionary = {}
        if parent_name != 'Object':
            raise Exception('TODO')

    def label(self):
        return behavior_label(self.name)

    def fill(self, vmem):
        oop_dict = vmem.append_sym_dict_emiting_entries(self.dictionary)

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

    def new_instance_method(self, name, params):
        cfun = self.cclass.new_instance_method(name, params)
        fun = Function(self.imod, cfun)
        self.dictionary[name] = fun
        return fun

    def new_class_method(self, name, params):
        cfun = self.cclass.new_class_method(name, params)
        fun = Function(self.imod, cfun)
        self.behavior.dictionary[name] = fun
        return fun

    def new_ctor(self, name, params):
        cfun = self.cclass.new_ctor(name, params)
        fun = Function(self.imod, cfun)
        self.behavior.dictionary[name] = fun
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
        # vmem.append_pointer_to(self.cclass.oop)                                   # <-
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

    def new_ctor(self, name, params):
        fn = CompiledFunction(self.cmod, self, name, params, ctor=True)
        self.class_methods[name] = fn
        return fn

    def new_instance_method(self, name, params):
        fn = CompiledFunction(self.cmod, self, name, params)
        self.instance_methods[name] = fn
        return fn

    def new_class_method(self, name, params):
        fn = CompiledFunction(self.cmod, self, name, params)
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


class VariableStorage(object):
    # requirements:
    #
    #  -add: requires current scope only (to check redeclaration)
    #  -index: requires stack of vars to lookup lexically enclosed vars
    #          ie. sibling closures with the same var name should use different locations
    #          for it.
    # -size/env_offset:  requires accumulated scopes

    def __init__(self, cfun, outer_cfun=None, storage=None):
        self.outer_cfun = outer_cfun
        self.parent_storage = storage
        if storage:
            self.variables = storage.variables
        else:
            self.variables = OrderedDict()
        self.variables[cfun] = []

    def has(self, cfun, name):
        try:
            self.index(cfun, name)
            return True
        except Exception:
            return False

    def add_names(self, cfun, params):
        for name in params:
            self.add(cfun, name)

    def add(self, cfun, name):
        if name in self.variables[cfun]:
            raise Exception('redeclaration of ' + name)
        self.variables[cfun].append(name)
        return self.index(cfun, name)

    def index(self, cfun, name):
        if name not in self.variables[cfun]:
            if self.parent_storage is None:
                raise Exception("Undeclared " + name)
            else:
                return self.parent_storage.index(self.outer_cfun, name)
        else:
            return lambda: self.env_offset(cfun) + self.variables[cfun].index(name)

    def total(self):
        return len(self._flat(self.variables.values()))

    def _flat(self, lst):
        return reduce(lambda x, y: x + y, lst, [])

    def env_offset(self, cfun):
        idx = self.variables.keys().index(cfun)
        return len(self._flat(self.variables.values()[:idx]))


class CompiledFunction(Entry):
    def __init__(self, cmod, owner, name, params, ctor=False, env_storage=None, is_top_level=True, outer_cfun=None):
        super(CompiledFunction, self).__init__()
        self.cmod = cmod

        self.name = name
        self.params = params
        self.is_ctor = ctor
        self.is_prim = False
        self.prim_name = ''
        self.owner = owner
        self.is_top_level = is_top_level
        self.outer_cfun = outer_cfun

        if env_storage:
            self.has_env = True
            self.var_declarations = VariableStorage(self, outer_cfun, env_storage)
        else:
            self.has_env = False
            self.var_declarations = VariableStorage(self)
        self.var_declarations.add_names(self, params)

        self.literal_frame = []
        self.bytecodes = opcode.Bytecodes()
        self.exceptions_frame = []

        self.accessor_flag = 0
        self.accessor_field = 0

        self._label = None

    def set_getter(self, idx):
        self.accessor_flag = 1 # normal=0/getter=1/setter=2
        self.accessor_field = idx


    def body_processor(self):
        return self

    def uses_env(self, val):
        self.has_env = val

    def set_primitive(self, prim_name):
        self.prim_name = prim_name
        self.is_prim = True

    def label(self):
        if self._label is None:
            self._label = cfun_label(self.owner.label(),
                                    self.name, isinstance(self.owner, CompiledClass))
        return self._label

    def literal_frame_label(self):
        return self.label() + '_literal_frame'

    def bytecode_label(self):
        return self.label() + '_bytecodes'

    def exceptions_frame_label(self):
        return self.label() + '_exceptions'

    def fill_literal_frame(self, vmem):
        if len(self.literal_frame) == 0:
            return 0

        # pre-append objects
        for lit in self.literal_frame:
            if lit['tag'] == 'number':
                pass
            elif lit['tag'] == 'symbol':
                lit['oop'] = vmem.append_symbol_instance(lit['value'])
            elif lit['tag'] == 'string':
                lit['oop'] = vmem.append_string_instance(lit['value'])
            elif lit['tag'] == 'cfun':
                lit['oop'] = lit['value'].fill(vmem)
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
            elif lit['tag'] == 'string':
                vmem.append_pointer_to(lit['oop'])
            elif lit['tag'] == 'cfun':
                vmem.append_pointer_to(lit['oop'])
            else:
                raise Exception('Todo')

        return len(self.literal_frame) * bits.WSIZE


    def fill_bytecodes(self, vmem):
        if len(self.bytecodes) == 0:
            return 0

        bytecodes = ''.join([bits.pack32(w) for w in self.bytecodes.words()])

        vmem.append_int(FRAME_TYPE_BYTECODE_FRAME)
        vmem.append_int(bits.string_block_size(bytecodes + "\0"))

        vmem.label_current(self.bytecode_label())
        vmem.append_string(bytecodes)
        return len(self.bytecodes) * opcode.WORD_SIZE

    def fill_exceptions_frame(self, vmem):
        if len(self.exceptions_frame) == 0:
            return 0

        type_oops = []
        for entry in self.exceptions_frame:
            if entry['type'] is None:
                type_oops.append(None)
            else:
                type_oops.append(vmem.append_string_instance(entry['type']))

        vmem.append_int(FRAME_TYPE_EXCEPTIONS_FRAME)
        vmem.append_int(len(self.exceptions_frame) * 4 * bits.WSIZE)

        vmem.label_current(self.exceptions_frame_label())

        for idx, entry in enumerate(self.exceptions_frame):
            vmem.append_int(entry['start'])
            vmem.append_int(entry['catch'])
            vmem.append_int(entry['local_pos']()) # lifted from VariableStorage
            if type_oops[idx] is None:
                vmem.append_null()
            else:
                vmem.append_pointer_to(type_oops[idx])

        return len(self.exceptions_frame)

    def fill(self, vmem):
        oop_delegate = vmem.append_object_instance()
        oop_name = vmem.append_string_instance(self.name)
        oop_params = vmem.append_list_of_strings(self.params)
        oop_prim_name = vmem.append_string_instance(self.prim_name)

        lit_frame_size = self.fill_literal_frame(vmem)
        bytecode_size = self.fill_bytecodes(vmem)
        exception_frames = self.fill_exceptions_frame(vmem)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(22 * bits.WSIZE)

        oop = vmem.append_external_ref('CompiledFunction', self.label()) # CompiledFunction vt
        vmem.append_pointer_to(oop_delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_params)
        vmem.append_int(int(self.is_ctor))
        vmem.append_int(int(self.is_prim))
        vmem.append_pointer_to(oop_prim_name)

        vmem.append_int(self.accessor_flag) # normal=0/getter=1/setter=2
        vmem.append_int(self.accessor_field) # getter/setter field index

        vmem.append_label_ref(self.owner.label())

        vmem.append_int(len(self.params))  # 10

        vmem.append_int(self.has_env)
        vmem.append_int(self.is_top_level)

        if self.outer_cfun is None:
            vmem.append_null()
        else:
            vmem.append_label_ref(self.outer_cfun.label())

        # local size or env size
        if self.is_top_level:
            vmem.append_int(self.var_declarations.total())
        else:
            vmem.append_int(0) # closures do not need to allocate space in ep or stack

        # env offset
        vmem.append_int(self.var_declarations.env_offset(self))

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

        vmem.append_int(exception_frames) # 20
        if exception_frames > 0:
            vmem.append_label_ref(self.exceptions_frame_label())
        else:
            vmem.append_null()

        self.oop = oop
        return oop

    #############

    def new_closure(self, params):
        self.has_env = True
        return CompiledFunction(self.cmod, self.owner, closure_name(), params,
                                env_storage=self.var_declarations,
                                is_top_level=False, outer_cfun=self)

    def add_exception_entry(self, label_begin_try, label_begin_catch, catch_type, var_idx):
        self.exceptions_frame.append({
            'start': label_begin_try(),
            'catch': label_begin_catch(),
            'type': catch_type,
            'local_pos': var_idx})


    def current_label(self):
        return self.bytecodes.new_label(current=True)

    def identifier_is_param(self, name):
        return name in self.params

    def identifier_is_decl(self, name):
        return self.var_declarations.has(self, name)

    def identifier_is_module_scoped(self, name):
        # this wont work if the class or function wasn;t compiled yet
        return name in self.cmod.top_level_names

    def identifier_is_prime(self, name):
        return name in ['Object', 'String', 'List', 'Exception']

    def index_for_literal(self, entry):
        if entry not in self.literal_frame:
            self.literal_frame.append(entry)
        return self.literal_frame.index(entry)

    def index_for_top_level(self, name):
        if name in self.cmod.params:
            return self.cmod.params.index(name)
        else:
            # TODO: classes.keys() is not reliable
            # -if a new class appears latter, this pos may change
            # -references to classes that werent compiled yet wont be found
            return len(self.cmod.params) + self.cmod.classes.keys().index(name)

    def create_and_register_number_literal(self, num):
        entry = {"tag": "number", "value": num}
        return self.index_for_literal(entry)

    def create_and_register_symbol_literal(self, string):
        entry = {"tag": "symbol", "value": string}
        return self.index_for_literal(entry)

    def create_and_register_string_literal(self, string):
        entry = {"tag": "string", "value": string}
        return self.index_for_literal(entry)

    def create_and_register_closure_literal(self, cfun):
        entry = {"tag": "cfun", "value": cfun}
        return self.index_for_literal(entry)


    def emit_push_var(self, name):
        if self.has_env and \
           (not self.identifier_is_module_scoped(name) and \
            not self.identifier_is_prime(name)):
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("push_env",idx)
        elif self.identifier_is_param(name):
            idx = self.params.index(name)
            self.bytecodes.append("push_param", idx)
        elif self.identifier_is_decl(name):
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("push_local", idx)
        elif self.identifier_is_module_scoped(name) or self.identifier_is_prime(name):
            idx = self.create_and_register_symbol_literal(name)
            self.bytecodes.append("push_module", 0)
            self.bytecodes.append("push_literal", idx)
            self.bytecodes.append('send', 0)
        else:
            raise Exception('push_var: undeclared ' + name)

    def emit_local_assignment(self, name):
        if self.has_env:
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("pop_env",idx)
        elif self.identifier_is_decl(name):
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("pop_local", idx)
        elif self.identifier_is_param(name):
            idx = self.params.index(name)
            self.bytecodes.append("pop_param", idx)
        else:
            raise Exception('local_assignment: undeclared ' + name)

    def emit_return_top(self):
        self.bytecodes.append("ret_top",0)

    def emit_return_null(self):
        self.bytecodes.append('push_bin', 0)
        self.bytecodes.append("ret_top",0)

    def emit_return_this(self):
        self.bytecodes.append("ret_this",0)

    def emit_var_decl(self, name):
        self.var_declarations.add(self, name)
        idx = self.var_declarations.index(self, name)
        if self.has_env:
            self.bytecodes.append("pop_env",idx)
        else:
            self.bytecodes.append("pop_local", idx)

    def emit_field_assignment(self, field):
        idx = self.owner.fields.index(field)
        self.bytecodes.append('pop_field', idx)

    def emit_pop(self):
        self.bytecodes.append('pop', 0)

    def emit_send_or_local_call(self, name, arity):
        if self.identifier_is_module_scoped(name):
            idx = self.create_and_register_symbol_literal(name)
            self.bytecodes.append('push_module', 0)
            self.bytecodes.append('push_literal', idx)
            self.bytecodes.append('send', arity)
        elif self.has_env:
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("push_env", idx)
            self.bytecodes.append("call", arity)
        elif self.identifier_is_param(name):
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("push_param", idx)
            self.bytecodes.append("call", arity)
        elif self.identifier_is_decl(name):
            idx = self.var_declarations.index(self, name)
            self.bytecodes.append("push_local", idx)
            self.bytecodes.append("call", arity)
        else:
            raise Exception('Undeclared ' + name)

    def emit_call(self, arity):
        self.bytecodes.append('call', arity)

    def emit_send(self, selector, arity):
        idx = self.create_and_register_symbol_literal(selector)
        self.bytecodes.append('push_literal', idx)
        self.bytecodes.append('send', arity)

    def emit_super_ctor_send(self, selector, arity):
        idx = self.create_and_register_symbol_literal(selector)
        self.bytecodes.append('push_literal', idx)
        self.bytecodes.append('super_ctor_send', arity)


    def emit_binary(self, selector):
        idx = self.create_and_register_symbol_literal(selector)
        self.bytecodes.append('push_literal', idx)
        self.bytecodes.append('send', 1)

    def emit_unary(self, selector):
        idx = self.create_and_register_symbol_literal(selector)
        self.bytecodes.append('push_literal', idx)
        self.bytecodes.append('send', 0)

    def emit_jz(self):
        lb = self.bytecodes.new_label()
        self.bytecodes.append('jz', lb)
        return lb

    def emit_jmp(self):
        lb = self.bytecodes.new_label()
        self.bytecodes.append('jmp', lb)
        return lb

    def emit_push_num_literal(self, num):
        idx = self.create_and_register_number_literal(num)
        self.bytecodes.append("push_literal", idx)

    def emit_push_this(self):
        self.bytecodes.append('push_this', 0)

    def emit_push_str_literal(self, string):
        idx = self.create_and_register_string_literal(string)
        self.bytecodes.append('push_literal', idx)

    def emit_push_sym_literal(self, sym):
        idx = self.create_and_register_symbol_literal(sym)
        self.bytecodes.append('push_literal', idx)

    def emit_push_null(self):
        self.bytecodes.append('push_bin', 0)

    def emit_push_true(self):
        self.bytecodes.append('push_bin', 1)

    def emit_push_false(self):
        self.bytecodes.append('push_bin', 2)

    def emit_push_module(self):
        self.bytecodes.append('push_module', 0)

    def emit_push_context(self):
        self.bytecodes.append('push_context', 0)

    def emit_push_closure(self, fn):
        idx_cfun = self.create_and_register_closure_literal(fn)
        idx_selector = self.create_and_register_symbol_literal("new_context")
        self.bytecodes.append("push_module", 0)
        self.bytecodes.append("push_ep", 0)
        self.bytecodes.append('push_literal', idx_cfun)
        self.bytecodes.append('push_literal', idx_selector)
        self.bytecodes.append('send', 2)


    def emit_push_field(self, field):
        idx = self.owner.fields.index(field)
        self.bytecodes.append('push_field', idx)

    def bind_catch_var(self, name):
        if not self.var_declarations.has(self, name):
            self.var_declarations.add(self, name)
        self.emit_local_assignment(name)

    def emit_catch_jump(self):
        self.bytecodes.append("jmp", 0)
        # hack
        return len(self.bytecodes) - 1

    def emit_try_catch(self, lb_begin_try, lb_begin_catch, jmp_pos, catch_type, catch_name):
        # hack
        blen = len(self.bytecodes)
        self.bytecodes.lst[jmp_pos].arg = lambda: blen - jmp_pos

        var_idx = self.var_declarations.index(self, catch_name)
        self.add_exception_entry(lb_begin_try, lb_begin_catch, catch_type, var_idx)


    def emit_push_list(self, length):
        idx_length = self.create_and_register_number_literal(length)
        idx_selector = self.create_and_register_symbol_literal("new")
        idx_append = self.create_and_register_symbol_literal("prepend")
        idx_klass = self.create_and_register_symbol_literal("List")

        self.bytecodes.append("push_module", 0)
        self.bytecodes.append('push_literal', idx_klass)
        self.bytecodes.append('send', 0)

        self.bytecodes.append('push_literal', idx_selector)
        self.bytecodes.append('send', 0)

        for i in range(0, length):
            self.bytecodes.append('push_literal', idx_append)
            self.bytecodes.append('send', 1)

    def emit_push_index(self):
        idx_selector = self.create_and_register_symbol_literal("index")
        self.bytecodes.append('push_literal', idx_selector)
        self.bytecodes.append('send', 1)

class Function(Entry):
    def __init__(self, imod, cfun):
        super(Function, self).__init__()
        self.imod = imod
        self.cfun = cfun

    def body_processor(self):
        return self.cfun

    def uses_env(self, val):
        self.cfun.uses_env(val)

    def set_primitive(self, prim_name):
        self.cfun.set_primitive(prim_name)

    def label(self):
        return fun_label(self.cfun.label())

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


def create_module_to_string(cmod):
    cfun = CompiledFunction(cmod, cmod, 'toString', [])
    cfun.set_primitive('module_to_string')
    return cfun

class CompiledModule(Entry):
    def __init__(self, name):
        super(CompiledModule, self).__init__()
        self.name = name
        self.params = []
        self.default_params = {}
        self.aliases = {}
        self.functions = {'toString': create_module_to_string(self)}
        self.classes = {}

        # eager loading of all top level names
        self.top_level_names = []

    def module_alias(self, libname, aliases):
        for alias in aliases:
            self.aliases[alias] = libname
            self.add_top_level_name(alias)

    def add_top_level_name(self, name):
        self.top_level_names.append(name)

    def entry_labels(self):
        return self.functions.keys() + self.classes.keys()

    def label(self):
        return cmod_label(self.name)

    def set_params(self, params):
        self.params = params
        self.top_level_names += params

    def add_default_param(self, lhs, ns, name):
        # TODO: support everything else
        self.default_params[name] = name # {"lhs": lhs, "ns": ns, "name": name}

    def new_function(self, name, params):
        fn = CompiledFunction(self, self, name, params)
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
        oop_params = vmem.append_list_of_strings(self.params)
        oop_default_params = vmem.append_string_dict(self.default_params)
        oop_aliases = vmem.append_string_dict(self.aliases)
        oop_functions = vmem.append_dict_emiting_entries(self.functions)
        oop_classes = vmem.append_dict_emiting_entries(self.classes)

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int(10 * bits.WSIZE)

        oop = vmem.append_external_ref('CompiledModule', self.label()) # vt: CompiledModule
        vmem.append_pointer_to(delegate)
        vmem.append_pointer_to(oop_name)
        vmem.append_pointer_to(oop_license)
        vmem.append_pointer_to(oop_params)
        vmem.append_pointer_to(oop_default_params)
        vmem.append_pointer_to(oop_aliases)
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

    def object_by_name(self, name):
        return [obj for obj in self.objects if obj.name == name][0]

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


    def create_getter(self, name, idx, vmem):
        cfun = CompiledFunction(self.cmod, self.cmod, 'get_' + name, [])
        cfun.set_getter(idx)
        cfun.fill(vmem)
        return Function(self, cfun)

    def emit_dict(self, vmem):
        d = dict(self.functions.items())

        idx = 4 # start after vt, delegate, dict, compiled_module

        for obj in self.objects:
            obj.fill(vmem)
            d[obj.label()] = self.create_getter(obj.label(), idx, vmem)
            idx += 1

        for klass in self.classes:
            klass.fill(vmem)
            d[klass.label()] = self.create_getter(klass.label(), idx, vmem)
            idx +=1

        return vmem.append_sym_dict_emiting_entries(d)

    def fill(self, vmem):
        oop_dict = self.emit_dict(vmem)

        # obj_oops = [o.fill(vmem) for o in self.objects]
        # class_oops = [c.fill(vmem) for c in self.classes]

        vmem.append_int(FRAME_TYPE_OBJECT)
        vmem.append_int((4 + len(self.objects) + len(self.classes)) * bits.WSIZE)

        oop = vmem.append_label_ref(self.label(), self.label()) # vt
        vmem.append_null()                                      # delegate
        vmem.append_pointer_to(oop_dict)                        # dict
        vmem.append_label_ref(self.cmod.label())                # compiled_module

        for obj in self.objects:
            vmem.append_label_ref(obj.label())

        for klass in self.classes:
            vmem.append_label_ref(klass.label())

        self.oop = oop
        return oop


class CoreCompiledModule(CompiledModule):
    def __init__(self):
        super(CoreCompiledModule, self).__init__('core')
        self.imod = CoreModule(self)

    def entry_labels(self):
        return self.imod.entry_labels()

    def new_function(self, name, params):
        cfun = super(CoreCompiledModule, self).new_function(name, params)
        fn = Function(self.imod, cfun)
        self.imod.append_function(name, fn)
        return fn

    def new_class(self, name, parent, fields):
        cclass = super(CoreCompiledModule, self).new_class(name, parent, fields)
        klass = Class(self.imod, cclass)
        self.imod.append_class(klass)
        return klass

    def new_object(self, name):
        if name == 'Object_Behavior':
            obj = Object_ObjectBehavior(name, self.imod)
        elif name == 'Object':
            obj = Object_Object(name, self.imod)
        else:
            obj = Object(name, self.imod)
        self.imod.append_object(obj)
        return obj

    def fill(self, vmem):
        super(CoreCompiledModule, self).fill(vmem)
        self.imod.fill(vmem)