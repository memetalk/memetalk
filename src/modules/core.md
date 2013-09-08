.license
Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
.endlicense

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
      !=: fun(other) {
          <primitive "object_not_equal">
      }
      send: fun(selector, args) {
          <primitive "object_send">
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
      send: fun(selector, args) {
          <primitive "object_send">
      }
    }
  }

  class CompiledClass {
    fields: module, name, super_class_name, fields, methods, own_methods;
    instance_method fullName: fun() {
      return @module.fullName() + "/" + @name;
    }
    instance_method constructors: fun() {
      <primitive "compiled_class_constructors">
    }
    instance_method name: fun() {
      return @name;
    }
    instance_method module: fun() {
      return @module;
    }
    instance_method rename: fun(name) {
      <primitive "compiled_class_rename">
    }
    instance_method fields: fun() {
      return @fields;
    }
    instance_method setFields: fun(fields) {
      <primitive "compiled_class_set_fields">
    }
    instance_method super_class_name: fun() {
      return @super_class_name;
    }
    instance_method instanceMethods: fun() {
      return @methods;
    }
    instance_method classMethods: fun() {
      <primitive "compiled_class_class_methods">
    }
    instance_method addMethod: fun(cfun, flag) {
      <primitive "compiled_class_add_method">
    }
    instance_method removeMethod: fun(name, flag) {
      <primitive "compiled_class_remove_method">
    }
    instance_method methodFlag: fun(cfun) { //null | :class_method | :instance_method
      <primitive "compiled_class_method_flag">
    }
  }

  class CompiledFunction {
    fields: _delegate, body, env_table, env_table_skel, fun_literals, is_ctor,
            is_prim, is_top_level, is_embedded, name, params, prim_name,
            uses_env, outer_cfun, owner, text;
    //flag: :module_function | :instance_method | :class_method | :constructor
    init newTopLevel: fun(name, text, owner, flag) {
      <primitive "compiled_function_new_top_level">
    }
    init newClosure: fun(text, outer_cfun, is_embedded) {
      <primitive "compiled_function_new_closure">
    }
    instance_method asContextWithFrame: fun(frame) {
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
    instance_method env_table: fun() {
      return @env_table;
    }
    instance_method isEmbedded: fun() {
      return @is_embedded;
    }
    instance_method isTopLevel: fun() {
      return @is_top_level;
    }
    instance_method is_constructor: fun() {
      return @is_ctor;
    }
    instance_method setCtor: fun(val) {
      @is_ctor = !!val;
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
        if (@owner) {
          return @owner.fullName() + "::" + @name;
        } else {
          return "<deep-sea>::" + @name; //We have no class [eg. Behavior function]
        }
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
    class_method withVars: fun(text, vars, imod) {
      var cmod = get_compiled_module(imod);
      var code = "fun() {" + text + "}";
      var cfn = CompiledFunction.newClosure(code, thisContext.compiledFunction(), false);
      return cfn.asContextWithVars(imod, vars);
    }

    class_method withFrame: fun(text, frame) {
      <primitive "context_with_frame">
    }
  }

  class CompiledModule {
    fields: _delegate, name, params, default_params, aliases, compiled_functions, compiled_classes;
    instance_method name: fun() {
      return @name;
    }
    instance_method setName: fun(name) {
      @name = name;
    }
    instance_method fullName: fun() {
      return this.name();
    }
    instance_method setParams: fun(params) {
      @params = params;
    }
    instance_method params: fun() {
      return @params;
    }
    instance_method defaultParameterFor: fun(name) {
      <primitive "compiled_module_default_parameter_for">
    }
    instance_method setDefaultParameter: fun(name, m) {
      <primitive "compiled_module_set_default_parameter">
    }
    instance_method default_parameters: fun() {
      return @default_params;
    }
    instance_method aliases: fun() {
      return @aliases;
    }
    instance_method compiled_functions: fun() {
      return @compiled_functions;
    }
    instance_method compiled_classes: fun() {
      return @compiled_classes;
    }
    instance_method newClass: fun(name, super_name) {
      <primitive "compiled_module_new_class">
    }
    instance_method addClass: fun(klass) {
      <primitive "compiled_module_add_class">
    }
    instance_method removeClass: fun(name) {
      <primitive "compiled_module_remove_class">
    }
    instance_method removeFunction: fun(name) {
      <primitive "compiled_module_remove_function">
    }
    instance_method addFunction: fun(cfun) {
      <primitive "compiled_module_add_function">
    }
    instance_method instantiate: fun(args) {
      <primitive "compiled_module_instantiate">
    }
  }

  object ModuleBehavior { //the behavior of module instances
    _vt: Behavior;
    parent: ObjectBehavior;
    dict: {};
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
    instance_method rindex: fun(arg) {
      <primitive "string_rindex">
    }
    instance_method count: fun(sub) {
      <primitive "string_count">
    }
    instance_method substring: fun(from, count) {
      <primitive "string_substring">
    }
    instance_method split: fun(sep) {
      <primitive "string_split">
    }
    instance_method toSymbol: fun() {
      <primitive "string_to_symbol">
    }
    instance_method charCode: fun() {
      <primitive "string_char_code">
    }
  }
  class Symbol {
   fields: self;
   instance_method toString: fun() {
     <primitive "symbol_to_string">
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
    instance_method map: fun(fn) {
      <primitive "dictionary_map">
    }
    instance_method set: fun(key,value) {
      <primitive "dictionary_set">
    }
    instance_method remove: fun(key) {
      <primitive "dictionary_remove">
    }
    instance_method size: fun() {
      <primitive "dictionary_size">
    }
    instance_method sortedEach: fun(fn) {
      <primitive "dictionary_sorted_each">
    }
  }

  class List {
    init new: fun(lst) {
      <primitive "list_ctor">
    }
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
    instance_method prepend: fun(arg) {
      <primitive "list_prepend">
    }
    instance_method append: fun(arg) {
      <primitive "list_append">
    }
    instance_method has: fun(value) {
      <primitive "list_has">
    }
    instance_method toString: fun() {
      <primitive "list_to_string">
    }
    instance_method join: fun(sep) {
      <primitive "list_join">
    }
    instance_method first: fun() {
      <primitive "list_first">
    }
    instance_method rest: fun() {
      <primitive "list_rest">
    }
    instance_method reversed: fun() {
      <primitive "list_reversed">
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
    fields: message, py_exception, py_trace;
    init new: fun(message) {
      @message = message;
    }
    instance_method throw: fun() {
       <primitive "exception_throw">
    }
    instance_method message: fun() {
      return @message;
    }
    instance_method py_trace: fun() {
      return @py_trace;
    }
    instance_method type: fun() {
      <primitive "exception_type">
    }
    class_method throw: fun(value) {
      var self = Exception.new(value);
      self.throw();
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

  class VMRemoteProcess {
   fields: self, procid, frames;
    init new: fun(procid) {
      @procid = procid;
    }
    instance_method initComWithTarget: fun() {
      <primitive "vmremoteprocess_init_com_with_target">
    }
    instance_method stepInto: fun() {
      <primitive "vmremoteprocess_step_into">
    }
    instance_method stepOver: fun() {
      <primitive "vmremoteprocess_step_over">
    }
    instance_method continue: fun() {
      <primitive "vmremoteprocess_continue">
    }
    instance_method stackFrames: fun() {
      <primitive "vmremoteprocess_stack_frames">
    }
    instance_method evalInFrame: fun(text, frame_index) {
      <primitive "vmremoteprocess_eval_in_frame">
    }
    instance_method updateObject: fun(obj) {
      <primitive "vmremoteprocess_update_object">
    }
    instance_method reloadFrame: fun(line) {
      <primitive "vmremoteprocess_reload_frame">
    }
  }
  class VMProcess {
    fields: procid;
    instance_method debug: fun() {
      <primitive "vmprocess_debug">
    }
    instance_method debugFn: fun(fn) {
      <primitive "vmprocess_debug_fn">
    }
    instance_method stackFrames: fun() {
      <primitive "vmprocess_stack_frames">
    }
    instance_method debugOnException: fun() {
      <primitive "vmprocess_debug_on_exception">
    }
    class_method current: fun() {
      <primitive "vmprocess_current">
    }
    // init spawn: fun() {
    //   <primitive "vmprocess_spawn">
    // }
    // instance_method isRunning: fun() {
    //   <primitive "vmprocess_is_running">
    // }
    // class_method spawnWithFun: fun(fn) {
    //   <primitive "vmprocess_spawn_with_fun">
    // }
    // instance_method stepOut: fun() {
    //   <primitive "vmprocess_step_out">
    // }
    // instance_method exec_module: fun(mname, fname, args) {
    //   <primitive "vmprocess_exec_module">
    // }
    // instance_method rewindAndBreak: fun(frames_count, to_line) {
    //   <primitive "vmprocess_rewind_and_break">
    // }
    // instance_method eval: fun(text, frame_level) {
    //   <primitive "vmprocess_eval">
    // }
    //
    // instance_method setDebuggerProcess: fun(arg) {
    //   <primitive "vmprocess_set_debugger_process">
    // }
    //
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
    instance_method receiverDataPointer: fun() {
      <primitive "vmstackframe_receiver_data_pointer">
    }
    instance_method environmentPointer: fun() {
      <primitive "vmstackframe_environment_pointer">
    }
    instance_method instructionPointer: fun() {
      <primitive "vmstackframe_instruction_pointer">
    }
    instance_method locals: fun() {
      <primitive "vmstackframe_locals">
    }
  }

  //temporary home for some infrastructure functions

  get_compiled_module: fun(module) {
    <primitive "get_compiled_module">
  }

  get_compiled_class: fun(klass) {
    <primitive "get_compiled_class">
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

  save_module: fun(name) {
    <primitive "save_module">
  }

  break_at: fun(cfun, line) {
    <primitive "break_at">
  }

  exception_unprotected: fun(fn) {
    <primitive "exception_unprotected">
  }

  exit: fun(code) {
    <primitive "exit">
  }

  http_get: fun(url) {
    <primitive "http_get">
  }
  parse_json: fun(str) {
    <primitive "parse_json">
  }
}
