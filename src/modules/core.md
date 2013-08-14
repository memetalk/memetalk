module core() {

  object Behavior {
    _vt: Behavior; //self-referencing
    parent: null;
    functions {
      toSource: fun() {
          <primitive "object_to_source">
      }
      toString: fun() {
          <primitive "object_to_string">
      }
    }
  }

  object ObjectBehavior {
    _vt: Behavior;
    parent: null;
    functions {
      init new: fun() {
        return this;
      }
      toString: fun() {
          <primitive "object_to_string">
      }
      toSource: fun() {
          <primitive "object_to_source">
      }
      ==: fun(other) {
          <primitive "object_equal">
      }
    }
  }

  object Object_CompiledClass {
    _vt: CompiledClass; //declared below
    _delegate: null;
    name: "Object";
    super_class_name: "";
    fields: [];
    methods: {};
    own_methods: {};
  }

  object Object {
    _vt: ObjectBehavior;
    parent: null;
    size: 0;
    compiled_class: Object_CompiledClass;
    functions {
      toString: fun() {
          <primitive "object_to_string">
      }
      toSource: fun() {
          <primitive "object_to_source">
      }
      ==: fun(other) {
          <primitive "object_equal">
      }
      !=: fun(other) {
          <primitive "object_not_equal">
      }
    }
  }

  class CompiledClass {
    fields: module, name, super_class_name, fields, methods, own_methods;
    instance_method fullName: fun() {
      return @module.fullName() + "/" + @name;
    }
  }

  class CompiledFunction {
    fields: _delegate, body, env_table, env_table_skel, fun_literals, is_ctor,
            is_prim, is_top_level, is_embedded, name, params, prim_name, 
            uses_env, outer_cfun, owner, text;
    init newTopLevel: fun(name, text, cmodule) {
      <primitive "compiled_function_new_top_level">
    }
    init newClosure: fun(text, cfun, is_embedded) {
      <primitive "compiled_function_new_closure">
    }
    instance_method asContextWithFrame: fun(imodule, frame) {
      <primitive "compiled_function_as_context_with_frame">
    }
    instance_method asContextWithVars: fun(imodule, vars) {
      <primitive "compiled_function_as_context_with_vars">
    }
    instance_method instantiate: fun(imodule) {
      <primitive "compiled_function_instantiate">
    }
    instance_method name: fun() {
      return @name;
    }
    instance_method parameters: fun() {
      return @params;
    }
    instance_method text: fun() {
      return @text;
    }
    instance_method setCode: fun(code) {
      <primitive "compiled_function_set_code">
    }
    instance_method ast: fun() {
      return @body;
    }
    instance_method isEmbedded: fun() {
      return @is_embedded;
    }
    instance_method isTopLevel: fun() {
      return @is_top_level;
    }
    instance_method owner: fun() {
      return @owner;
    }
    instance_method topLevelCompiledFunction: fun() {
      if (this.isTopLevel()) {
        return null;
      } else {
        if (@outer_cfun.isTopLevel()) {
          return @outer_cfun;
        } else {
          return @outer_cfun.topLevelCompiledFunction();
        }
      }
    }
    instance_method fullName: fun() {
      if (this.isTopLevel()) {
        return @owner.fullName() + "::" + @name;
      } else {
        return this.topLevelCompiledFunction().fullName() + "[" + @name + "]";
      }
    }
  }

  class Function {
    fields: compiled_function, module, _delegate;
    instance_method compiledFunction: fun() {
      return @compiled_function;
    }
    instance_method apply: fun(args) {
      <primitive "function_apply">
    }
  }

  class Context {
    fields: _delegate, compiled_function, env, module;
    instance_method apply: fun(args) {
      <primitive "context_apply">
    }
    instance_method getEnv: fun() {
      <primitive "context_get_env">
    }
    instance_method compiledFunction: fun() {
      return @compiled_function;
    }
  }

  class CompiledModule {
    fields: _delegate, name, filepath, params, default_params, aliases, compiled_functions, compiled_classes;
    instance_method name: fun() {
      return @name;
    }
    instance_method setName: fun(name) {
      @name = name;
    }
    instance_method fullName: fun() {
      return this.name();
    }
    instance_method filepath: fun() {
      return @filepath;
    }
    instance_method params: fun() {
      return @params;
    }
    instance_method default: fun() {
      return @default_params;
    }
    instance_method aliases: fun() {
      return @aliases;
    }
    instance_method compiled_functions: fun() {
      return @compiled_functions;
    }
    instance_method compiled: fun() {
      return @compiled_classes;
    }
    instance_method removeFunction: fun(name) {
      <primitive "compiled_module_remove_function">
    }
    instance_method addFunction: fun(cfun) {
      <primitive "compiled_module_add_function">
    }
  }

  object ModuleBehavior { //the behavior of module instances
    _vt: Behavior;
    parent: ObjectBehavior;
  }

  class String {
    instance_method size: fun() {
      <primitive "string_size">
    }
    instance_method +: fun(arg) {
      <primitive "string_concat">
    }
    instance_method from: fun(idx) {
      <primitive "string_from">
    }
    instance_method count: fun(sub) {
      <primitive "string_count">
    }
  }

  class Dictionary {
    instance_method +: fun(arg) {
      <primitive "dictionary_plus">
    }
    instance_method each: fun(fn) {
      <primitive "dictionary_each">
    }
    instance_method has: fun(key) {
      <primitive "dictionary_has">
    }
  }

  class List {
    instance_method each: fun(fn) {
      <primitive "list_each">
    }
    instance_method get: fun(n) {
      <primitive "list_get">
    }
    instance_method size: fun() {
      <primitive "list_size">
    }
    instance_method map: fun(fn) {
      <primitive "list_map">
    }
    instance_method +: fun(arg) {
      <primitive "list_plus">
    }
    instance_method has: fun(value) {
      <primitive "list_has">
    }
  }

  class Number {
    instance_method +: fun(arg) {
      <primitive "number_plus">
    }
    instance_method -: fun(arg) {
      <primitive "number_minus">
    }
    instance_method <: fun(arg) {
      <primitive "number_lst">
    }
    instance_method <=: fun(arg) {
      <primitive "number_lsteq">
    }
    instance_method >=: fun(arg) {
      <primitive "number_grteq">
    }
  }

  class Exception {
    fields: value;
    init new: fun(value) {
      @value = value;
    }
    instance_method raise: fun() {
       <primitive "exception_raise">
    }
    instance_method value: fun() {
      return @value;
    }
    class_method throw: fun(value) {
      var self = Exception.new(value);
      self.raise();
    }
  }

  // temporary home for infrastructure classes

  class Mirror {
    fields: mirrored;
    init new: fun(mirrored) {
      @mirrored = mirrored;
    }
    instance_method fields: fun() {
      <primitive "mirror_fields">
    }
    instance_method valueFor: fun(name) {
      <primitive "mirror_value_for">
    }
    instance_method setValueFor: fun(name, value) {
      <primitive "mirror_set_value_for">
    }
    class_method vtFor: fun(obj) {
      <primitive "mirror_vt">
    }
  }

  class VMProcess {
    instance_method stackFrames: fun() {
      <primitive "vmprocess_stack_frames">
    }
    instance_method stepInto: fun() {
      <primitive "vmprocess_step_into">
    }
    instance_method stepOver: fun() {
      <primitive "vmprocess_step_over">
    }
    instance_method stepOut: fun() {
      <primitive "vmprocess_step_out">
    }
    instance_method continue: fun() {
      <primitive "vmprocess_continue">
    }
    instance_method rewind: fun() {
      <primitive "vmprocess_rewind">
    }
    class_method debug: fun(fn, args) {
      <primitive "vmprocess_debug">
    }
  }

  class VMStackFrame {
    instance_method modulePointer: fun() {
      <primitive "vmstackframe_module_pointer">
    }
    instance_method contextPointer: fun() {
      <primitive "vmstackframe_context_pointer">
    }
    instance_method receiverPointer: fun() {
      <primitive "vmstackframe_receiver_pointer">
    }
    instance_method environmentPointer: fun() {
      <primitive "vmstackframe_environment_pointer">
    }
    instance_method instructionPointer: fun() {
      <primitive "vmstackframe_instruction_pointer">
    }
    instance_method localVars: fun() {
      <primitive "vmstackframe_local_vars">
    }
    instance_method instanceVars: fun() {
      <primitive "vmstackframe_instance_vars">
    }
    instance_method moduleVars: fun() {
      <primitive "vmstackframe_module_vars">
    }
  }

  //temporary home for some infrastructure functions
  get_current_process: fun() {
    <primitive "get_current_process">
  }

  get_compiled_module: fun(module) {
    <primitive "get_compiled_module">
  }

  modules_path: fun() {
    <primitive "modules_path">
  }

  available_modules: fun() {
    <primitive "available_modules">
  }

  get_module: fun(name) {
    <primitive "get_module">
  }
}
