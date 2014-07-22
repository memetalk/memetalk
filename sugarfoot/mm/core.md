.license
.endlicense

.preamble()

.code

  object Behavior
    _vt: Behavior; //self-referencing
    _delegate: null;
    dict: {};
//    size: 0;
  end

  object Object_Behavior
    _vt: Behavior;
    _delegate: null;
    dict: {};
//    size: 0;
  end

  object Object_CompiledClass
    _vt: CompiledClass; //declared below
    _delegate: null;
    name: "Object";
    super_class_name: "";
    fields: [];
    methods: {};
    own_methods: {};
  end

  object Object
    _vt: Object_Behavior;
    _delegate: null;
    size: 0;
    compiled_class: Object_CompiledClass;
    functions {
       !: fun() {
         <primitive "object_not">
       }
    }
  end

  class CompiledClass
    fields: module, name, super_class_name,
            fields, methods, own_methods;
  end

  class CompiledFunction
  instance_method new_context: fun(ep, module) {
    return Context.new(this, ep, module);
  }
  end

  class CompiledModule
    instance_method instantiate: fun(args) {
      <primitive "compiled_module_instantiate">
    }
  end

  class Null
  end

  class Boolean
  end

  class String
  end

  class Symbol
  end

  class Dictionary
  end

  class List
  instance_method append: fun(x) {
    <primitive "list_append">
  }
  instance_method prepend: fun(x) {
    <primitive "list_prepend">
  }
  instance_method index: fun(idx) {
    <primitive "list_index">
  }
  instance_method each: fun(fn) {
    <primitive "list_each">
  }
  class_method new: fun() {
    <primitive "list_new">
  }
  end

  class Number
    instance_method +: fun(arg) {
      <primitive "number_sum">
    }
    instance_method -: fun(arg) {
      <primitive "number_sub">
    }
    instance_method *: fun(arg) {
      <primitive "number_mul">
    }
    instance_method <: fun(arg) {
      <primitive "number_lt">
    }
  end


  class Function
  end

  class Context
  fields: compiled_function, env, module;
  init new: fun(cfun, env, module) {
    @compiled_function = cfun;
    @env = env;
    @module = module;
  }
  end

class Exception
  fields: message;
  init new: fun(message) {
    @message = message;
  }
  instance_method message: fun() {
    return @message;
  }
  instance_method throw: fun() {
    <primitive "exception_throw">
  }
  class_method throw: fun(msg) {
    Exception.new(msg).throw;
  }
end

.end
