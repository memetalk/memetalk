module core() {

  object Behavior {
    _vt: Behavior; //self-referencing
    parent: null;
    functions {
      fun toSource() {
          <primitive "object_to_source">
      }
      fun toString() {
          <primitive "object_to_string">
      }
    }
  }

  object ObjectBehavior {
    _vt: Behavior;
    parent: null;
    functions {
      init new() {
        return this;
      }
      fun toString() {
          <primitive "object_to_string">
      }
      fun toSource() {
          <primitive "object_to_source">
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
      fun toString() {
          <primitive "object_to_string">
      }
      fun toSource() {
          <primitive "object_to_source">
      }
      fun ==(other) {
          <primitive "object_equal">
      }
      fun !=(other) {
          <primitive "object_not_equal">
      }
    }
  }

  class CompiledClass {
    fields: name, super_class_name, fields, methods, own_methods;
  }

  class CompiledFunction {
    fields: _delegate, body, env_table, env_table_skel, fun_literals, is_ctor,
            is_prim, name, params, prim_name, uses_env, outter_cfun, owner,
            text;
    init new(text, parameters, module, env) {
      <primitive "compiled_function_new">
    }
    fun asContext(imodule, self, env) {
      <primitive "compiled_function_as_context">
    }
    fun instantiate(imodule) {
      <primitive "compiled_function_instantiate">
    }
    fun name() {
      return @name;
    }
    fun parameters() {
      return @params;
    }
    fun text() {
      return @text;
    }
    fun setCode(code) {
      <primitive "compiled_function_set_code">
    }
    fun ast() {
      return @body;
    }
  }

  class Function {
    fields: compiled_function, module, _delegate;
    fun compiledFunction() {
      return @compiled_function;
    }
    fun apply(args) {
      <primitive "function_apply">
    }
  }

  class Context {
    fields: _delegate, compiled_function, env, module;
    fun apply(args) {
      <primitive "context_apply">
    }
    fun getEnv() {
      <primitive "context_get_env">
    }
    fun compiledFunction() {
      return @compiled_function;
    }
  }

  class CompiledModule {
    fields: _delegate, name, filepath, params, default_params, aliases, compiled_functions, compiled_classes;
    fun name() {
      return @name;
    }
    fun filepath() {
      return @filepath;
    }
    fun params() {
      return @params;
    }
    fun default_params() {
      return @default_params;
    }
    fun aliases() {
      return @aliases;
    }
    fun compiled_functions() {
      return @compiled_functions;
    }
    fun compiled_classes() {
      return @compiled_classes;
    }
  }

  object ModuleBehavior { //the behavior of module instances
    _vt: Behavior;
    parent: ObjectBehavior;
  }

  class String {
    fun size() {
      <primitive "string_size">
    }
    fun +(arg) {
      <primitive "string_concat">
    }
    fun from(idx) {
      <primitive "string_from">
    }
  }

  class Dictionary {
    fun +(arg) {
      <primitive "dictionary_plus">
    }
    fun each(fn) {
      <primitive "dictionary_each">
    }
    fun has(key) {
      <primitive "dictionary_has">
    }
  }

  class List {
    fun each(fn) {
      <primitive "list_each">
    }
    fun get(n) {
      <primitive "list_get">
    }
    fun size() {
      <primitive "list_size">
    }
    fun map(fn) {
      <primitive "list_map">
    }
    fun +(arg) {
      <primitive "list_plus">
    }
    fun has(value) {
      <primitive "list_has">
    }
  }

  class Number {
    fun +(arg) {
      <primitive "number_plus">
    }
    fun -(arg) {
      <primitive "number_minus">
    }
    fun <(arg) {
      <primitive "number_lst">
    }
    fun <=(arg) {
      <primitive "number_lsteq">
    }
  }

  class Exception {
    fields: value;
    init new(value) {
      @value = value;
    }
    func throw(value) {
      var self = Exception.new(value);
      self.raise();
    }
    fun raise() {
       <primitive "exception_raise">
    }
    fun value() {
      return @value;
    }
  }

  // temporary home for infrastructure classes

  class Mirror {
    fields: mirrored;
    init new(mirrored) {
      @mirrored = mirrored;
    }
    fun fields() {
      <primitive "mirror_fields">
    }
    fun valueFor(name) {
      <primitive "mirror_value_for">
    }
    fun setValueFor(name, value) {
      <primitive "mirror_set_value_for">
    }
  }

  class VMProcess {
    fun stackFrames() {
      <primitive "vmprocess_stack_frames">
    }
    fun stepInto() {
      <primitive "vmprocess_step_into">
    }
    fun stepOver() {
      <primitive "vmprocess_step_over">
    }
    fun stepOut() {
      <primitive "vmprocess_step_out">
    }
    fun continue() {
      <primitive "vmprocess_continue">
    }
    fun rewind() {
      <primitive "vmprocess_rewind">
    }
    fun localVars() {
      <primitive "vmprocess_local_vars">
    }
    fun contextPointer() {
      <primitive "vmprocess_context_pointer">
    }
    fun instructionPointer() {
      <primitive "vmprocess_instruction_pointer">
    }
    fun modulePointer() {
      <primitive "vmprocess_module_pointer">
    }
    func debug(fn, args) {
      <primitive "vmprocess_debug">
    }
  }

  class VMStackFrame {
    fun modulePointer() {
      <primitive "vmstackframe_module_pointer">
    }
    fun contextPointer() {
      <primitive "vmstackframe_context_pointer">
    }
    fun receiverPointer() {
      <primitive "vmstackframe_receiver_pointer">
    }
    fun environmentPointer() {
      <primitive "vmstackframe_environment_pointer">
    }
    fun instructionPointer() {
      <primitive "vmstackframe_instruction_pointer">
    }
    fun localVars() {
      <primitive "vmstackframe_local_vars">
    }
    fun instanceVars() {
      <primitive "vmstackframe_instance_vars">
    }
    fun moduleVars() {
      <primitive "vmstackframe_module_vars">
    }
  }

  //temporary home for some infrastructure functions

  fun get_compiled_module(module) {
    <primitive "get_compiled_module">
  }

  fun modules_path() {
    <primitive "modules_path">
  }

  fun available_modules() {
    <primitive "available_modules">
  }

  fun get_module(name) {
    <primitive "get_module">
  }
}
