meme foo

requires bits, opcode, mmc
where
  bits   = central:memescript/bits
  opcode = central:memescript/opcode
  mmc    = central:memescript/mmc


class OrderedDict < Dictionary
fields: order;
init new: fun() {
  super.new();
  @order = [];
}
instance_method set: fun(key, val) {
  super(key, val);
  @order.append(key);
}
instance_method keys: fun() {
  return @order;
}
instance_method values: fun() {
  return @order.map(fun(k) { this[k] });
}
end


class Entry
fields: oop, label_counter;
init new: fun() {
  @oop = null;
  @label_counter = 0;
}
instance_method set_oop: fun(oop) {
  @oop = oop;
  return oop;
}
instance_method oop: fun() {
  return @oop;
}
instance_method label_counter: fun() {
  var ret = @label_counter;
  @label_counter = @label_counter + 1;
  return ret;
}
end


class MMObject < Entry
fields: name, slots, imod;
init new: fun(name, imod) {
  super.new();
  @name = name;
  @slots = [];
  @imod = imod;
}
instance_method imod: fun() {
  return @imod;
}
instance_method slots: fun() {
  return @slots;
}
instance_method label: fun() {
  return @name;
}
instance_method add_slot_literal_array: fun(name, value) {
  if (value.size > 0) {
    Exception.throw("todo");
  }
  this.add_slot_empty_list(name);
}
instance_method add_slot_literal_dict: fun(name, value) {
  if (value.size > 0) {
    Exception.throw("todo");
  }
  this.add_slot_empty_dict(name);
}
instance_method add_slot_ref: fun(name, value) {
  this.slots.append({:type: :ref, :name: name, :value: value});
}
instance_method add_slot_literal_string: fun(name, value) {
  this.slots.append({:type: :string, :name: name, :value: value});
}
instance_method add_slot_empty_dict: fun(name) {
  // TODO: this will become dict
  this.slots.append({:type: :empty_dict, :name: name});
}
instance_method add_slot_empty_list: fun(name) {
  // TODO: this will become list
  this.slots.append({:type: :empty_list, :name: name});
}
instance_method add_slot_literal_null: fun(name) {
  this.slots.append({:type: :null, :name: name});
}
instance_method add_slot_literal_num: fun(name, value) {
  this.slots.append({:type: :int, :name: name, :value: value});
}
instance_method fill: fun(vmem) {
  // synth necessary objects:
  var refs_to_literals = {};
  @slots.from(1).each(fun(idx, slot) {
    if (slot[:type] == :string) {
      refs_to_literals[idx] = vmem.append_string_instance(slot[:value]);
    } elif (slot[:type] == :empty_list) {
      refs_to_literals[idx] = vmem.append_empty_list();
    } elif (slot[:type] == :empty_dict) {
      refs_to_literals[idx] = vmem.append_empty_dict();
    } elif (slot[:type] == :dict) {
      var fun_dict = {};
      slot[:value].each(fun(name, cfun) {
        cfun.fill(vmem);
        fun_dict[name] = Function.new(@imod, cfun);
      });
      refs_to_literals[idx] = vmem.append_sym_dict_emiting_entries(fun_dict);
    }
  });

  // emit our object

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(@slots.size * bits.WSIZE, null);

  var oop = vmem.append_label_ref(@slots[0][:value], @name);

  @slots.from(1).each(fun(idx, slot) {
    if (slot[:type] == :ref) {
      vmem.append_label_ref(slot[:value], null);
    } elif (slot[:type] == :null) {
      vmem.append_null(null);
    } elif (slot[:type] == :string) {
      vmem.append_pointer_to(refs_to_literals[idx], null);
    } elif (slot[:type] == :empty_list) {
      vmem.append_pointer_to(refs_to_literals[idx], null);
    } elif (slot[:type] == :empty_dict) {
      vmem.append_pointer_to(refs_to_literals[idx], null);
    } elif (slot[:type] == :dict) {
      vmem.append_pointer_to(refs_to_literals[idx], null);
    } elif (slot[:type] == :int) {
      vmem.append_tagged_int(slot[:value], null);
    } else {
      Exception.throw("TODO");
    }
  });
  return this.set_oop(oop);
}

end


//// these need to know the CompiledFunction owner (which s Object_CompiledClass)

class Object_Object_Behavior < MMObject
instance_method new_ctor: fun(name) {
  var owner = this.imod.object_by_name("Object_CompiledClass");
  var d = this.slots.filter(fun(s) { s[:type] == :dict });
  var dslot = null;
  if (d.size == 0) {
    dslot = {:type: :dict, name: :dict, :value: {}};
    this.slots.insert(2, dslot); //dict is slot in idx==2
  } else {
    dslot = d[0];
  }
  var fn = CompiledFunction.new(this.imod.cmod, owner, name, [],
  true, null, true, null, null);
  dslot[:value][name] = fn;
  return fn;
}
instance_method new_function: fun(name, params) {
  var owner = this.imod.object_by_name("Object_CompiledClass");
  var d = this.slots.filter(fun(s) { s[:type] == :dict });
  var dslot = null;
  if (d.size == 0) {
    dslot = {:type: :dict, name: :dict, :value: {}};
    this.slots.insert(2, dslot); //dict is slot in idx==2
  } else {
    dslot = d[0];
  }
  var fn = CompiledFunction.new(this.imod.cmod, owner, name, params,
      false, null, true, null, null);
  dslot[:value][name] = fn;
  return fn;
}
end

class Object_Object < MMObject
instance_method new_function: fun(name, params) {
  var owner = this.imod.object_by_name("Object_CompiledClass");
  var d = this.slots.filter(fun (s) { s[:type] == :dict });
  var dslot = null;
  if (d.size == 0) {
    dslot = {:type: :dict, name: :dict, :value: {}};
    this.slots.insert(2, dslot); //dict is slot in idx==2
  } else {
    dslot = d[0];
  }
  var fn = CompiledFunction.new(this.imod.cmod, this, name, params,
      false, null, true, null, null);
  dslot[:value][name] = fn;
  return fn;
}
end

class Behavior < Entry
fields: name, parent_name, parent_label, dictionary;
init new: fun(name, parent_name) {
  super.new();
  @name = name;
  @parent_name = parent_name;
  @parent_label = mmc.behavior_label(parent_name);
  @dictionary = {};
}
instance_method label: fun() {
  return mmc.behavior_label(@name);
}
instance_method fill: fun(vmem) {
  var oop_dict = vmem.append_sym_dict_emiting_entries(@dictionary);

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(4 * its.WSIZE, null);

  var oop = vmem.append_label_ref("Behavior", this.label); //vt
  vmem.append_label_ref(@parent_label, null);              //delegate
  vmem.append_pointer_to(oop_dict, null);                  //dict: own methods

  vmem.append_int(256, null);                              //dummy flag: "I am a behavior. no more fields after this"
  return this.set_oop(oop);
}
end

class Class < Entry
fields: imod, behavior, cclass, dictionary;
init new: fun(imod, cclass) {
  super.new();
  @imod = imod;
  @behavior = Behavior.new(cclass.name, cclass.super_name);
  @cclass = cclass;
  @dictionary = {};
}
instance_method label: fun() {
  return mmc.class_label(@cclass.name);
}
instance_method new_instance_method: fun(name) {
  var cfun = @cclass.new_instance_method(name);
  var fn = Function.new(@imod, cfun);
  @dictionary[name] = fn;
  return fn;
}
instance_method new_class_method: fun(name) {
  var cfun = @cclass.new_class_method(name);
  var fn = Function.new(@imod, cfun);
  @behavior.dictionary[name] = fn;
  return fn;
}
instance_method new_ctor: fun(name) {
  var cfun = @cclass.new_ctor(name);
  var fn = Function.new(@imod, cfun);
  @behavior.dictionary[name] = fn;
  return fn;
}
instance_method fill: fun(vmem) {
  var oop_vt = @behavior.fill(vmem);

  var oop_dict = vmem.append_sym_dict_emiting_entries(@dictionary);

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(5 * bits.WSIZE, null);

  oop_vt = @behavior.fill(vmem);

  // oop = vmem.append_label_ref(behavior_label(self.cclass.name),  self.label())  // vt
  var oop = vmem.append_pointer_to(oop_vt, this.label); // vt
  vmem.append_label_ref(mmc.class_label(@cclass.super_name), null);              // delegate
  vmem.append_pointer_to(oop_dict, null);                                        // dict: "methods"
  vmem.append_int(@cclass.fields_list.size, null);                                  // payload
  vmem.append_label_ref(mmc.cclass_label(@cclass.name), null);                   // compiled_class
  // vmem.append_pointer_to(self.cclass.oop)                                     // <-
  return this.set_oop(oop);
}
end

class CompiledClass < Entry
fields: cmod, name, super_name, fields_list, instance_methods, class_methods;
init new: fun(cmod, name, super_name, fields_list) {
  super.new();
  @cmod = cmod;
  @name = name;
  @super_name = super_name;
  @fields_list = fields_list;
  @instance_methods = {};
  @class_methods = {};
}
instance_method fields_list: fun() {
  return @fields_list;
}
instance_method label: fun() {
  return mmc.cclass_label(@name); //cmod.label() + '_' + self.name + "_CompiledClass"
}
instance_method new_ctor: fun(name) {
  var fn = CompiledFunction.new(@cmod, this, name, [], true,
      null, true, null, null);
  @class_methods[name] = fn;
  return fn;
}
instance_method new_instance_method: fun(name) {
  var fn = CompiledFunction.new(@cmod, this, name, [],
      false, null, true, null, null);
  @instance_methods[name] = fn;
  return fn;
}
instance_method new_class_method: fun(name) {
  var fn = CompiledFunction.new(@cmod, this, name, [],
      false, null, true, null, null);
  @class_methods[name] = fn;
  return fn;
}
instance_method fill: fun(vmem) {
  // vt: CompiledClass
  // delegate: ...
  // module: ...
  // name: ...
  // super_class_name: ...
  // fields
  // methods
  // class_methods ...

  var delegate = vmem.append_object_instance();
  var oop_name = vmem.append_string_instance(@name);
  var oop_super = vmem.append_string_instance(@super_name);
  var oop_fields = vmem.append_list_of_strings(@fields_list);
  var oop_methods = vmem.append_sym_dict_emiting_entries(@instance_methods);
  var oop_class_methods = vmem.append_sym_dict_emiting_entries(@class_methods);

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(8 * bits.WSIZE, null);

  var oop = vmem.append_external_ref("CompiledClass", this.label); // vt: CompiledClass
  vmem.append_pointer_to(delegate, null);
  vmem.append_pointer_to(oop_name, null);
  vmem.append_pointer_to(oop_super, null);
  vmem.append_label_ref(@cmod.label, null); //
  vmem.append_pointer_to(oop_fields, null);
  vmem.append_pointer_to(oop_methods, null);
  vmem.append_pointer_to(oop_class_methods, null);
  return this.set_oop(oop);
}
end

class VariableStorage
    // requirements:
    //
    //  -add: requires current scope only (to check redeclaration)
    //  -index: requires stack of vars to lookup lexically enclosed vars
    //          ie. sibling closures with the same var name should use different locations
    //          for it.
    // -size/env_offset:  requires accumulated scopes

fields: outer_cfun, parent_storage, variables;
init new: fun(cfun, outer_cfun, storage) { //outer_cfun/storage defaults to null
  @outer_cfun = outer_cfun;
  @parent_storage = storage;
  if (storage) {
    @variables = storage.variables;
  } else {
    @variables = OrderedDict.new; //env needs vars ordered by insert order.
  }
  @variables[cfun] = [];
}
instance_method variables: fun() {
  return @variables;
}
instance_method is_visible: fun(cfun, name) {
  var idx = @variables.keys.pos(cfun);
  return this._flat(@variables.values().range(0, idx + 1)).has(name);
}
instance_method add_names: fun(cfun, names) {
  names.each(fun(_, name) {
    this.add(cfun, name);
  });
}
instance_method add: fun(cfun, name) {
  if (@variables[cfun].has(name)) {
    Exception.throw("redeclaration of " + name + " in " + cfun.name);
  }
  @variables[cfun].append(name);
  return this.pos(cfun, name);
}
instance_method pos: fun(cfun, name) {
  return this._pos(cfun, name);
}
instance_method _pos: fun(cfun, name) {
  if (!@variables[cfun].has(name)) {
    if (@parent_storage == null) {
      Exception.throw("Undeclared " + name);
    } else {
      return @parent_storage.pos(@outer_cfun, name);
    }
  } else {
    return fun() { this.env_offset(cfun) + @variables[cfun].pos(name) };
  }
}
instance_method total: fun() {
  return this._flat(@variables.values).size;
}
instance_method _flat: fun(lst) {
  return lst.reduce([], fun(x,y) { x + y });
}
instance_method env_offset: fun(cfun) {
  var idx = @variables.keys.pos(cfun);
  var offset = this._flat(@variables.values.range(0, idx)).size;
  return offset;
}
instance_method env_table: fun() {
  return this._flat(@variables.values);
}
end

class CompiledFunction < Entry
fields: cmod, name, params, is_ctor, is_prim, prim_name, owner, is_top_level,
        outer_cfun, top_level_cfun, text, start_line, line_mapping,
        location_mapping, has_env, var_declarations, literal_frame,
        bytecodes, exceptions_frame, accessor_flag, accessor_field,
        closures, should_wrap_catch_for_non_local_return, label,
        var_arg;

init new: fun(cmod, owner, name, params, is_ctor, env_storage, is_top_level, outer_cfun, top_level_cfun) {
  //defaults:
  //-params = []
  //-ctor = False
  //-env_storage = None
  //-is_top_level = True
  //-outer_cfun = None
  //-top_level_cfun = None
  @cmod = cmod;
  @name = name;
  @params = params;
  @is_ctor = is_ctor;
  @is_prim = false;
  @prim_name = "";
  @owner = owner;
  @is_top_level = is_top_level;
  @outer_cfun = outer_cfun;
  @top_level_cfun = top_level_cfun;
  @text = "";
  @start_line = -1;
  @line_mapping = {};
  @location_mapping = {};

  if (env_storage) {
    @has_env = true;
    @var_declarations = VariableStorage.new(this, outer_cfun, env_storage);
  } else {
    @has_env = false;
    @var_declarations = VariableStorage.new(this, null, null);
  }
  @var_declarations.add_names(this, params);

  @literal_frame = [];
  @bytecodes = opcode.Bytecodes.new;
  @exceptions_frame = [];

  @accessor_flag = 0;
  @accessor_field = 0;

  @closures = [];

  @should_wrap_catch_for_non_local_return = false;

  @label = null;
  @var_arg = false;
}
instance_method set_text: fun(text) {
  @text = text;
}
instance_method set_line: fun(head) {
  @start_line = head.start_line;
}
instance_method set_params: fun(params) {
  @params = params;
  @var_declarations.add_names(this, params);
}
instance_method set_vararg: fun(name) {
  @var_arg = true;
}
instance_method declare_var: fun(name) {
  @var_declarations.add(this, name);
}
instance_method declare_vars: fun(names) {
  @var_declarations.add_names(names);
}
instance_method set_getter: fun(idx) {
  @accessor_flag = 1; // normal=0/getter=1/setter=2
  @accessor_field = idx;
}
instance_method body_processor: fun() {
  return this;
}
instance_method uses_env: fun(val) {
  @has_env = val;
}
instance_method set_primitive: fun(prim_name) {
  @prim_name = prim_name;
  @is_prim = true;
}
instance_method top_level_cfun: fun() {
  return @top_level_cfun;
}

instance_method label: fun() {
  if (@label == null) {
    @label = mmc.cfun_label(@owner.label_counter, @owner.label, @name, Mirror.vtFor(@owner) == CompiledClass);
  }
  return @label;
}
instance_method literal_frame_label: fun() {
  return this.label + "_literal_frame";
}
instance_method bytecode_label: fun() {
  return this.label + "_bytecodes";
}
instance_method exceptions_frame_label: fun() {
  return this.label + "_exceptions";
}
instance_method fill_literal_frame: fun(vmem) {
  if (@literal_frame.size == 0) {
    return 0;
  }

  //pre-append objects
  @literal_frame.each(fun(_, lit) {
    if (lit[:tag] == :number) {
      //nothing
    } elif (lit[:tag] == :symbol) {
      lit[:oop] = vmem.append_symbol_instance(lit[:value]);
    } elif (lit[:tag] == :string) {
      lit[:oop] = vmem.append_string_instance(lit[:value]);
    } elif (lit[:tag] == :cfun) {
      lit[:oop] = lit[:value].fill(vmem);
    } elif (lit[:tag] == :core) {
      //nothing
    } else {
      Exception.throw("TODO");
    }
  });

  vmem.append_int(mmc.FRAME_TYPE_LITERAL_FRAME, null);
  vmem.append_int(@literal_frame.size * bits.WSIZE, null);

  vmem.label_current(this.literal_frame_label);

  @literal_frame.each(fun(_, lit) {
    if (lit[:tag] == :number) {
      vmem.append_int(bits.tag(lit[:value]), null);
    } elif (lit[:tag] == :symbol) {
      vmem.append_pointer_to(lit[:oop], null);
    } elif (lit[:tag] == :string) {
      vmem.append_pointer_to(lit[:oop], null);
    } elif (lit[:tag] == :cfun) {
      vmem.append_pointer_to(lit[:oop], null);
    } elif (lit[:tag] == :core) {
      vmem.append_external_ref(lit[:value], null); //unusually reusing literal_frame to store core object references
    } else {
      Exception.throw("TODO");
    }
  });
  return @literal_frame.size * bits.WSIZE;
}
instance_method fill_bytecodes: fun(vmem) {
  if (@bytecodes.size == 0) {
    return 0;
  }

  var bytecodes = @bytecodes.words.map(fun(w) { bits.pack32(w) }).join("");

  vmem.append_int(mmc.FRAME_TYPE_BYTECODE_FRAME, null);
  vmem.append_int(bits.string_block_size(bytecodes + "\0"), null);

  vmem.label_current(this.bytecode_label);
  vmem.append_string(bytecodes, null);
  return @bytecodes.size * opcode.WORD_SIZE;
}
instance_method fill_exceptions_frame: fun(vmem) {
  if (@exceptions_frame.size == 0) {
    return 0;
  }

  var type_oops = [];
  @exceptions_frame.each(fun(_, entry) {
    if (entry[:type] == null) {
      type_oops.append(null);
    } else {
      type_oops.append(vmem.append_string_instance(entry[:type]));
    }
  });

  vmem.append_int(mmc.FRAME_TYPE_EXCEPTIONS_FRAME, null);
  vmem.append_int(@exceptions_frame.size * mmc.EXCEPTION_FRAME_SIZE * bits.WSIZE, null);

  vmem.label_current(this.exceptions_frame_label);

  @exceptions_frame.each(fun(idx, entry) {
    vmem.append_int(entry[:start], null);
    vmem.append_int(entry[:catch], null);
    if (type_oops[idx] == null) {
      vmem.append_null(null);
    } else {
      vmem.append_pointer_to(type_oops[idx], null);
    }
  });

  return @exceptions_frame.size;
}
instance_method wrap_catch_for_non_local_return: fun() {
  var lb_begin_try = opcode.Label.new(@bytecodes, 0);
  var lb_begin_catch = this.current_label(true);
  var catch_type = "NonLocalReturn";

  //taking out the valuefrom exception object -- it is on top of stack
  var idx_val = this.create_and_register_symbol_literal("value");
  @bytecodes.append(:push_literal, idx_val);
  @bytecodes.append(:send, 0);
  @bytecodes.append(:ret_top, 0);
  this.add_exception_entry(lb_begin_try, lb_begin_catch, catch_type);
}
instance_method fill: fun(vmem) {
  if (@should_wrap_catch_for_non_local_return) {
    this.wrap_catch_for_non_local_return();
  }

  var oop_delegate = vmem.append_object_instance();
  var oop_name = vmem.append_string_instance(@name);
  var oop_params = vmem.append_list_of_strings(@params);
  var oop_prim_name = vmem.append_symbol_instance(@prim_name);
  var oop_text = vmem.append_string_instance(@text);
  var oop_line_mappings = vmem.append_int_to_int_dict(@line_mapping);
  var oop_loc_mappings = vmem.append_int_to_int_list(@location_mapping);
  var oop_env_table = vmem.append_list_of_symbols(@var_declarations.env_table);

  var lit_frame_size = this.fill_literal_frame(vmem);
  var bytecode_size = this.fill_bytecodes(vmem);
  var exception_frames = this.fill_exceptions_frame(vmem);

  var oop_closures = vmem.append_list_of_oops_for_labels(@closures.map(fun(x) { x.label }));

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(28 * bits.WSIZE, null);

  var oop = vmem.append_external_ref("CompiledFunction", this.label); // ComppiledFunction vt
  vmem.append_pointer_to(oop_delegate, null);
  vmem.append_pointer_to(oop_name, null);
  vmem.append_pointer_to(oop_params, null);
  vmem.append_int(@is_ctor, null);
  vmem.append_int(@is_prim, null);
  vmem.append_pointer_to(oop_prim_name, null);

  vmem.append_int(@accessor_flag, null); //normal=0/getter=1/setter=2
  vmem.append_int(@accessor_field, null); //getter/setter field index

  vmem.append_label_ref(@owner.label, null);

  vmem.append_int(@params.size, null); //10

  vmem.append_int(@has_env, null);
  vmem.append_int(@is_top_level, null);

  if (@outer_cfun == null) {
    vmem.append_null(null);
  } else {
    vmem.append_label_ref(@outer_cfun.label, null);
  }

  // local size or env size
  vmem.append_int(@var_declarations.total, null);

  vmem.append_int(@var_declarations.env_offset(this), null);

  vmem.append_int(lit_frame_size, null);
  if (lit_frame_size > 0) {
    vmem.append_label_ref(this.literal_frame_label, null);
  } else {
    vmem.append_null(null);
  }

  vmem.append_int(bytecode_size, null);
  if (bytecode_size > 0) {
    vmem.append_label_ref(this.bytecode_label, null);
  } else {
    vmem.append_null(null);
  }

  vmem.append_int(exception_frames, null); //20
  if (exception_frames > 0) {
    vmem.append_label_ref(this.exceptions_frame_label, null);
  } else {
    vmem.append_null(null);
  }

  vmem.append_pointer_to(oop_env_table, null);
  vmem.append_pointer_to(oop_text, null);
  vmem.append_pointer_to(oop_line_mappings, null);
  vmem.append_pointer_to(oop_loc_mappings, null);
  vmem.append_pointer_to(oop_closures, null);
  vmem.append_int(@var_arg, null);
  return this.set_oop(oop);
}
instance_method new_closure: fun(params) {
  @has_env = true;
  var top_level_cfun = null;
  if (@is_top_level) {
    top_level_cfun = this;
  } else {
    top_level_cfun = @top_level_cfun;
  }

  var cfun = CompiledFunction.new(@cmod, @owner, mmc.closure_name(@owner.label_counter), params,
                                  false, @var_declarations, false, this, top_level_cfun);

  if (@is_top_level) {
    @closures.append(cfun);
  }
  return cfun;
}
instance_method add_exception_entry: fun(label_begin_try, label_begin_catch, catch_type) {
  @exceptions_frame.append({
    :start: label_begin_try.value,
    :catch: label_begin_catch.value,
    :type: catch_type});
}
instance_method current_label: fun(current) { //current defaults to true
  return @bytecodes.new_label(current);
}
instance_method identifier_in_scope: fun(cfun, name) {
  return @var_declarations.is_visible(cfun, name);
}
instance_method identifier_is_module_scoped: fun(name) {
  // this wont work if the class or function wasn;t compiled yet
  return @cmod.top_level_names.has(name);
}
instance_method identifier_is_prime: fun(name) {
  return  ["Object", "String", "List", "Exception", "Context", "CompiledFunction"].has(name);
}
instance_method index_for_literal: fun(entry) {
  if (!@literal_frame.has(entry)) {
    @literal_frame.append(entry);
  }
  return @literal_frame.pos(entry);
}
instance_method index_for_top_level: fun(name) {
  if (@cmod.params.has(name)) {
    return @cmod.params.pos(name);
  } else {
    // TODO: classes.keys() is not reliable
    // -if a new class appears latter, this pos may change
    // -references to classes that werent compiled yet wont be found
    return @cmod.params.size + @cmod.classes.keys.pos(name);
  }
}
instance_method create_and_register_number_literal: fun(num) {
  var entry = {:tag: :number, :value: num};
  return this.index_for_literal(entry);
}
instance_method create_and_register_symbol_literal: fun(string) {
  var entry = {:tag: :symbol, :value: string};
  return this.index_for_literal(entry);
}
instance_method create_and_register_string_literal: fun(string) {
  var entry = {:tag: :string, :value: string};
  return this.index_for_literal(entry);
}
instance_method create_and_register_closure_literal: fun(cfun) {
  var entry = {:tag: :cfun, :value: cfun};
  return this.index_for_literal(entry);
}
instance_method create_and_register_core: fun(class_name) {
  // unusually reusing literal_frame to store core objects
  var entry = {:tag: :core, :value: class_name};
  return this.index_for_literal(entry);
}

instance_method emit_push_var: fun(ast, name) {
  var bpos = this.current_bytecode_pos();

  var idx = null;
  if (this.identifier_in_scope(this, name)) {
    idx = @var_declarations.pos(this, name);
    @bytecodes.append(:push_local, idx);
  } elif (this.identifier_is_module_scoped(name) or this.identifier_is_prime(name)) {
    idx = this.create_and_register_symbol_literal(name);
    @bytecodes.append(:push_module, 0);
    @bytecodes.append(:push_literal, idx);
    @bytecodes.append(:send, 0);
  } else {
    // raise Exception('push_var: undeclared ' + name)
    // for now, lets assume its a module instead of raising,
    // to make it easy for dynamic eval() code
    idx = this.create_and_register_symbol_literal(name);
    @bytecodes.append(:push_module, 0);
    @bytecodes.append(:push_literal, idx);
    @bytecodes.append(:send, 0);
  }
  this.update_line_mapping(bpos, ast);
}
// instance_method emit_push_var: fun(_, name) {
//   var idx = null;
//   if (this.identifier_in_scope(this, name)) {
//     idx = @var_declarations.pos(this, name);
//     @bytecodes.append(:push_local, idx);
//   } elif (this.identifier_is_module_scoped(name) or this.identifier_is_prime(name)) {
//     idx = this.create_and_register_symbol_literal(name);
//     @bytecodes.append(:push_module, 0);
//     @bytecodes.append(:push_literal, idx);
//     @bytecodes.append(:send, 0);
//   } else {
//     // raise Exception('push_var: undeclared ' + name)
//     // for now, lets assume its a module instead of raising,
//     // to make it easy for dynamic eval() code
//     idx = this.create_and_register_symbol_literal(name);
//     @bytecodes.append(:push_module, 0);
//     @bytecodes.append(:push_literal, idx);
//     @bytecodes.append(:send, 0);
//   }
// }
instance_method emit_local_assignment: fun(ast, name) {
  var bpos = this.current_bytecode_pos();

  var idx = 0;
  if (this.identifier_in_scope(this, name)) {
    idx = @var_declarations.pos(this, name);
    @bytecodes.append(:pop_local, idx);
  } else {
    Exception.throw("local assignment: undeclared: " + name);
  }
  this.update_line_mapping(bpos, ast);
}
instance_method emit_return_top: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:ret_top, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method set_should_wrap_catch_for_non_local_return: fun(val) {
  @should_wrap_catch_for_non_local_return = val;
}

instance_method emit_non_local_return: fun(ast) {
  var bpos = this.current_bytecode_pos();
  var idx_class = this.create_and_register_symbol_literal("NonLocalReturn");
  var idx_throw = this.create_and_register_symbol_literal("throw");
  @bytecodes.append(:push_module, 0);
  @bytecodes.append(:push_literal, idx_class);
  @bytecodes.append(:send, 0);
  @bytecodes.append(:push_literal, idx_throw);
  @bytecodes.append(:send, 1);
  @top_level_cfun.set_should_wrap_catch_for_non_local_return(true);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_return_null: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_bin, 0);
  @bytecodes.append(:ret_top, 0);
  this.update_line_mapping(bpos, ast);
}

instance_method emit_end_body: fun(ast) {
  var bpos = this.current_bytecode_pos();
  if (@is_top_level) {
    @bytecodes.append(:ret_this, 0);
  } else {
    @bytecodes.append(:push_bin, 0);
    @bytecodes.append(:ret_top, 0);
  }
  this.update_line_mapping(bpos, ast);
}
instance_method current_bytecode_pos: fun() {
  return @bytecodes.size;
}
instance_method update_line_mapping: fun(bpos, ast) {
  if (ast == null) {
    return null;
  }
  if (ast.start_line == null) {
    return null;
  }

  var start_line = null;
  var end_line = null;

  if (@outer_cfun) {
    start_line = ast.start_line;
    end_line = ast.end_line;
  } else {
    start_line = ast.start_line;
    end_line = ast.end_line;
  }

  @location_mapping[bpos] = [start_line, ast.start_col, end_line, ast.end_col];
  @line_mapping[bpos] = start_line;
}
instance_method emit_var_decl: fun(ast, name) {
  var bpos = this.current_bytecode_pos();
  this.declare_var(name);
  var idx = @var_declarations.pos(this, name);
  @bytecodes.append(:pop_local, idx);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_field_assignment: fun(ast, field) {
  var bpos = this.current_bytecode_pos();
  var idx = @owner.fields_list.pos(field);
  @bytecodes.append(:pop_field, idx);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_index_assignment: fun(ast) {
  var bpos = this.current_bytecode_pos();
  var idx_selector = this.create_and_register_symbol_literal("set");
  @bytecodes.append(:push_literal,idx_selector);
  @bytecodes.append(:send, 2);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_pop: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:pop, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_send_or_local_call: fun(ast, name, arity) {
  var bpos = this.current_bytecode_pos();
  var idx = null;
  if (this.identifier_in_scope(this, name)) {
    idx = @var_declarations.pos(this, name);
    @bytecodes.append(:push_local, idx);
    @bytecodes.append(:call, arity);
  } elif (this.identifier_is_module_scoped(name)) {
    idx = this.create_and_register_symbol_literal(name);
    @bytecodes.append(:push_module, 0);
    @bytecodes.append(:push_literal, idx);
    @bytecodes.append(:send, arity);
  } else {
    // raise Exception('Undeclared ' + name)
    // for now, lets assume its a module instead of raising,
    // to make it easy for dynamic eval() code
    idx = this.create_and_register_symbol_literal(name);
    @bytecodes.append(:push_module, 0);
    @bytecodes.append(:push_literal, idx);
    @bytecodes.append(:send, arity);
  }
  this.update_line_mapping(bpos, ast);
}
instance_method emit_call: fun(ast, arity) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:call, arity);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_send: fun(ast, selector, arity) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_symbol_literal(selector);
  @bytecodes.append(:push_literal, idx);
  @bytecodes.append(:send, arity);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_super_send: fun(ast, arity) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:super_send, arity);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_super_ctor_send: fun(ast, selector, arity) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_symbol_literal(selector);
  @bytecodes.append(:push_literal, idx);
  @bytecodes.append(:super_ctor_send, arity);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_binary: fun(ast, selector) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_symbol_literal(selector);
  @bytecodes.append(:push_literal, idx);
  @bytecodes.append(:send, 1);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_unary: fun(ast, selector) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_symbol_literal(selector);
  @bytecodes.append(:push_literal, idx);
  @bytecodes.append(:send, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_jz: fun(lb) { //lb defaults to null
  if (!lb) {
    lb = @bytecodes.new_label(null);
  }
  @bytecodes.append(:jz, lb);
  return lb;
}
instance_method emit_jmp: fun(lb) {
  if (!lb) {
    lb = @bytecodes.new_label(null);
  }
  @bytecodes.append(:jmp, lb);
  return lb;
}
instance_method emit_jmp_back: fun(lb) {
  if (!lb) {
    lb = @bytecodes.new_label(null);
  }
  @bytecodes.append(:jmpb, lb);
  return lb;
}
instance_method emit_push_num_literal: fun(ast, num) {
  if (num <= 4611686018427387903) {
    var bpos = this.current_bytecode_pos();
    var idx = this.create_and_register_number_literal(num);
    @bytecodes.append(:push_literal, idx);
    this.update_line_mapping(bpos, ast);
  } else {
    Exception.throw("Integer is too big: " + num.toString);
  }
}
instance_method emit_push_this: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_this, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_str_literal: fun(ast, string) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_string_literal(string);
  @bytecodes.append(:push_literal, idx);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_sym_literal: fun(ast, sym) {
  var bpos = this.current_bytecode_pos();
  var idx = this.create_and_register_symbol_literal(sym);
  @bytecodes.append(:push_literal, idx);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_null: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_bin, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_true: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_bin, 1);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_false: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_bin, 2);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_module: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_module, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_context: fun(ast) {
  var bpos = this.current_bytecode_pos();
  @bytecodes.append(:push_context, 0);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_closure: fun(ast, fn) {
  var bpos = this.current_bytecode_pos();
  var idx_cfun = this.create_and_register_closure_literal(fn);
  var idx_selector = this.create_and_register_symbol_literal("new_context");
  @bytecodes.append(:push_fp, 0);
  @bytecodes.append(:push_module, 0);
  @bytecodes.append(:push_literal, idx_cfun);
  @bytecodes.append(:push_literal, idx_selector);
  @bytecodes.append(:send, 2);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_field: fun(ast, field) {
  var bpos = this.current_bytecode_pos();
  var idx = @owner.fields_list.pos(field);
  @bytecodes.append(:push_field, idx);
  this.update_line_mapping(bpos, ast);
}
instance_method bind_catch_var: fun(name) {
  if (!this.identifier_in_scope(this, name)) {
    this.declare_var(name);
  }
  this.emit_local_assignment(null, name);
}
instance_method emit_catch_jump: fun() {
  @bytecodes.append(:jmp, 0); // arg 0 will be substituted later
  //hack
  return @bytecodes.size - 1;
}
instance_method emit_try_catch: fun(lb_begin_try, lb_begin_catch, jmp_pos, catch_type) {
  var blen = @bytecodes.size;
  @bytecodes.list[jmp_pos].set_arg(opcode.Value.new(blen - jmp_pos));
  this.add_exception_entry(lb_begin_try, lb_begin_catch, catch_type);
}
instance_method emit_push_list: fun(ast, length) {
  var bpos = this.current_bytecode_pos();
  var idx_klass = this.create_and_register_symbol_literal("List");
  var idx_new_from_stack = this.create_and_register_symbol_literal("new_from_stack");

  @bytecodes.append(:push_module, 0);
  @bytecodes.append(:push_literal, idx_klass); //unusually reusing literal_frame to store core object reference
  @bytecodes.append(:send, 0);

  @bytecodes.append(:push_literal, idx_new_from_stack);
  @bytecodes.append(:send, length);
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_dict: fun(ast, length) {
  var bpos = this.current_bytecode_pos();
  var idx_length = this.create_and_register_number_literal(length);
  var idx_selector = this.create_and_register_symbol_literal("new");
  var idx_set = this.create_and_register_symbol_literal("set");
  var idx_klass = this.create_and_register_symbol_literal("Dictionary");

  @bytecodes.append(:push_module, 0);
  @bytecodes.append(:push_literal, idx_klass);
  @bytecodes.append(:send, 0);

  @bytecodes.append(:push_literal, idx_selector);
  @bytecodes.append(:send, 0);

  [0].times(length).each(fun(_,__) { //we need better loops :)
    @bytecodes.append(:push_literal, idx_set);
    @bytecodes.append(:send, 2);
  });
  this.update_line_mapping(bpos, ast);
}
instance_method emit_push_index: fun(ast) {
  var bpos = this.current_bytecode_pos();
  var idx_selector = this.create_and_register_symbol_literal("index");
  @bytecodes.append(:push_literal, idx_selector);
  @bytecodes.append(:send, 1);
  this.update_line_mapping(bpos, ast);
}
end

class Function < Entry
fields: imod, cfun;
init new: fun(imod, cfun) {
  @imod = imod;
  @cfun = cfun;
}
instance_method set_vararg: fun(name) {
  @cfun.set_vararg(name);
}
instance_method set_params: fun(params) {
  @cfun.set_params(params);
}
instance_method body_processor: fun() {
  return @cfun;
}
instance_method uses_env: fun(val) {
  @cfun.uses_env(val);
}
instance_method set_line: fun(line) {
  cfun.set_line(line);
}
instance_method set_text: fun(text) {
  @cfun.set_text(text);
}
instance_method set_primitive: fun(prim_name) {
  @cfun.set_primitive(prim_name);
}
instance_method label: fun() {
  return mmc.fun_label(@cfun.label);
}
instance_method fill: fun(vmem) {
  var delegate = vmem.append_object_instance();

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(4 * bits.WSIZE, null);

  var oop = vmem.append_label_ref("Function", this.label); //vt
  vmem.append_pointer_to(delegate, null);                  //delegate
  vmem.append_label_ref(@cfun.label, null);                //compiled_function

  vmem.append_label_ref(@imod.label, null);                //module
  //4: env
  return this.set_oop(oop);
}
end

create_module_to_string: fun(cmod) {
  var cfun = CompiledFunction.new(cmod, cmod, "toString", [],
      false, null, true, null, null);
  cfun.set_primitive("module_to_string");
  return cfun;
}

class CompiledModule < Entry
fields: name, params, default_locations, imports, top_level_names, functions, classes, meta_vars;
init new: fun(name) {
  super.new();
  @name = name;
  @params = [];
  @meta_vars = {};
  @default_locations = {};
  @imports = {};

  @top_level_names = [];
  @functions = {"toString": create_module_to_string(this)};
  @classes = {};
}
instance_method add_meta: fun(key, val) {
  @meta_vars[key] = val;
}
instance_method set_params: fun(params) {
  @params = params;
  params.each(fun(_, p) { this.add_top_level_name(p) });
}
instance_method add_default_location: fun(name, path) {
  @default_locations[name] = path;
}
instance_method add_import: fun(name, from) {
  @imports[name] = from;
  this.add_top_level_name(name);
}

instance_method top_level_names: fun() {
  return @top_level_names;
}
instance_method add_top_level_name: fun(name) {
  @top_level_names.append(name);
}
instance_method entry_labels: fun() {
  return @functions.keys() + @classes.keys();
}
instance_method label: fun() {
  return mmc.cmod_label(@name);
}

instance_method new_function: fun(name, params) {
  var fn = CompiledFunction.new(this, this, name, params,
      false, null, true, null, null);
  @functions[name] = fn;
  return fn;
}
instance_method new_class: fun(name, parent, fields_list) {
  var klass = CompiledClass.new(this, name, parent, fields_list);
  @classes[name] = klass;
  return klass;
}
instance_method fill: fun(vmem) {


  // first word on object table is a pointer to the CompiledModule
  vmem.append_label_ref(this.label, null);

  var delegate = vmem.append_object_instance();
  var oop_name = vmem.append_string_instance(@name);
  var oop_params = vmem.append_list_of_strings(@params);
  var oop_meta_vars = vmem.append_symbol_dict(@meta_vars);
  var oop_def_locations = vmem.append_symbol_dict(@default_locations);
  var oop_imports = vmem.append_symbol_dict(@imports);
  var oop_functions = vmem.append_sym_dict_emiting_entries(@functions);
  var oop_classes = vmem.append_sym_dict_emiting_entries(@classes);

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int(10 * bits.WSIZE, null);

  var oop = vmem.append_external_ref("CompiledModule", this.label); // vt: CompiledModule
  vmem.append_pointer_to(delegate, null);
  vmem.append_pointer_to(oop_name, null);
  vmem.append_pointer_to(oop_params, null);
  vmem.append_pointer_to(oop_meta_vars, null);
  vmem.append_pointer_to(oop_def_locations, null);
  vmem.append_pointer_to(oop_imports, null);
  vmem.append_pointer_to(oop_functions, null);
  vmem.append_pointer_to(oop_classes, null);
  vmem.append_null(null);                        // parent_module
  return this.set_oop(oop);
}
end

class CoreModule < Entry
fields: cmod, functions, classes, objects;
init new: fun(cmod) {
  @cmod = cmod;
  @functions = {};
  @classes = [];
  @objects = [];
}
instance_method object_by_name: fun(name) {
  return @objects.filter(fun(obj) { obj.name == name })[0];
}
instance_method append_class: fun(klass) {
  @classes.append(klass);
}
instance_method append_function: fun(name, fn) {
  @functions[name] = fn;
}
instance_method append_object: fun(obj) {
  @objects.append(obj);
}
instance_method entry_labels: fun() {
  var top_objects = ["Behavior", "Object_Behavior", "Object"];
  var res = @functions.values.map(fun(f) { f.label });
  res = res + @classes.map(fun(c) { c.label });
  res = res + @objects.filter(fun(o) { top_objects.has(o.label) }).map(
     fun(o) { o.label });
  return res + [this.label];
}
instance_method label: fun() {
  return "@core_module";
}
instance_method create_getter: fun(name, idx, vmem) {
  var cfun = CompiledFunction.new(@cmod, @cmod, "get_" + name, [],
      false, null, true, null, null);
  cfun.set_getter(idx);
  cfun.fill(vmem);
  return Function.new(this, cfun);
}
instance_method emit_dict: fun(vmem) {
  var d = @functions.copy; //?
  var idx = 4;

  @objects.each(fun(_, obj) {
    obj.fill(vmem);
    d[obj.label] = this.create_getter(obj.label, idx, vmem);
    idx = idx + 1;
  });

  @classes.each(fun(_, klass) {
    klass.fill(vmem);
    d[klass.label] = this.create_getter(klass.label, idx, vmem);
    idx = idx + 1;
  });
  return vmem.append_sym_dict_emiting_entries(d);
}
instance_method fill: fun(vmem) {
  var oop_dict = this.emit_dict(vmem);

  vmem.append_int(mmc.FRAME_TYPE_OBJECT, null);
  vmem.append_int((4 + @objects.size + @classes.size) * bits.WSIZE, null);

  var oop = vmem.append_label_ref(this.label, this.label); //vt
  vmem.append_null(null);
  vmem.append_pointer_to(oop_dict, null);
  vmem.append_label_ref(@cmod.label, null);

  @objects.each(fun(_,obj) {
    vmem.append_label_ref(obj.label, null);
  });

  @classes.each(fun(_, klass) {
    vmem.append_label_ref(klass.label, null);
  });

  return this.set_oop(oop);
}
end

class CoreCompiledModule < CompiledModule
fields: imod;
init new: fun(imod) {
  super.new("core");
  @imod = CoreModule.new(this);
}
instance_method label: fun() {
  return "CoreCompiledModule";
}
instance_method entry_labels: fun() {
  return this.imod.entry_labels();
}
instance_method new_function: fun(name, params) {
  var cfun = super(name, params);
  var fn = Function.new(@imod, cfun);
  @imod.append_function(name, fn);
  return fn;
}
instance_method new_class: fun(name, parent, fields_list) {
  var cclass = super(name, parent, fields_list);
  var klass = Class.new(@imod, cclass);
  @imod.append_class(klass);
  return klass;
}
instance_method new_object: fun(name) {
  var obj = null;
  if (name == "Object_Behavior") {
    obj = Object_ObjectBehavior.new(name, @imod);
  } elif (name == "Object") {
    obj = Object_Object.new(name, @imod);
  } else {
    obj = MMObject.new(name, @imod);
  }
  return obj;
}
instance_method fill: fun(vmem) {
  super(vmem);
  @imod.filL(vmem);
}
end
