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

  object Object_CompiledClass
    _vt: CompiledClass; //declared below
    _delegate: null;
    name: "Object";
    super_class_name: "";
    fields: [];
    methods: {};
    own_methods: {};
  end

  object Object_Behavior
    _vt: Behavior;
    _delegate: null;
    size: 0;
    functions {
      init new: fun() {
        return this;
      }
       // id: fun() {
       //   <primitive "id">
       // }
       toString: fun() {
         <primitive "behavior_to_string">
       }
       ==: fun(other) {
         <primitive "equal">
       }
    }
  end

  object Object
    _vt: Object_Behavior;
    _delegate: null;
    size: 0;
    compiled_class: Object_CompiledClass;
    functions {
       // id: fun() {
       //   <primitive "id">
       // }
       toString: fun() {
         <primitive "object_to_string">
       }
       !: fun() {
         <primitive "object_not">
       }
       ==: fun(other) {
         <primitive "equal">
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
  end

  class Null
  instance_method toString: fun() {
    return "null";
  }
  end

  class Boolean
  instance_method toString: fun() {
    if (this == true) {
      return "true";
    } else {
      return "false";
    }
  }
  end

  class String
  instance_method toString: fun() {
    return this;
  }
  instance_method +: fun(other) {
    <primitive "string_append">
  }
  end

  class Symbol
  instance_method toString: fun() {
    <primitive "symbol_to_string">
  }
  end

  class Dictionary
  instance_method toString: fun() {
    <primitive "dictionary_to_string">
  }
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
  instance_method toString: fun() {
    <primitive "list_to_string">
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
    instance_method toString: fun() {
      <primitive "number_to_string">
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
  instance_method message: fun() {
    return @message;
  }
  instance_method toString: fun() {
    return this.message.toString();
  }
  class_method throw: fun(msg) {
    this.new(msg).throw;
  }
end

.end
