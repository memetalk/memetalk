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
       toSource: fun() {
         <primitive "behavior_to_source">
       }
       ==: fun(other) {
         <primitive "equal">
       }
       !=: fun(other) {
         return !(this == other);
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
       toSource: fun() {
         <primitive "object_to_source">
       }
       !: fun() {
         <primitive "object_not">
       }
       ==: fun(other) {
         <primitive "equal">
       }
       !=: fun(other) {
         return !(this == other);
       }
    }
  end

  class CompiledClass
    fields: module, name, super_class_name,
            fields, methods, own_methods;
  end

  class CompiledFunction
    init withEnv: fun(text, vars, cmod) {
      <primitive "compiled_function_with_env">
    }
    instance_method new_context: fun(ep, module) {
      return Context.new(this, ep, module);
    }
    instance_method asContextWithVars: fun(imod, vars) {
      <primitive "compiled_function_as_context_with_vars">
    }
  end

  class CompiledModule
  end

  class Null
    instance_method toString: fun() {
      return "null";
    }
    instance_method toSource: fun() {
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
  instance_method and: fun(other) {
    if (this) {
      if (other) {
         return true;
      }
    }
    return false;
  }
  end

  class String
  instance_method toString: fun() {
    return this;
  }
  instance_method +: fun(other) {
    <primitive "string_append">
  }
  instance_method count: fun(other) {
    <primitive "string_count">
  }
  instance_method size: fun() {
    <primitive "string_size">
  }
  instance_method ==: fun(other) {
    <primitive "string_equal">
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
  instance_method set: fun(key, value) {
    <primitive "dictionary_set">
  }
  instance_method index: fun(idx) {
    <primitive "dictionary_index">
  }
  instance_method +: fun(other) {
    <primitive "dictionary_plus">
  }
  class_method new: fun() {
    <primitive "dictionary_new">
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
    fields: compiled_function, module;
    instance_method compiledFunction: fun() {
      return @compiled_function;
    }
    instance_method getEnv: fun() {
      <primitive "function_get_env">
    }
  end

  class Context
  fields: compiled_function, module, env;
  init new: fun(cfun, env, module) {
    @compiled_function = cfun;
    @env = env;
    @module = module;
  }
  instance_method compiledFunction: fun() {
    return @compiled_function;
  }
  instance_method getEnv: fun() {
    <primitive "context_get_env">
  }
  class_method withVars: fun(text, vars, imod) {
    var cmod = get_compiled_module(imod);
    var code = "fun() {" + text + "}";
    var cfn = CompiledFunction.withEnv(code, vars, cmod);
    return cfn.asContextWithVars(imod, vars);
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
    return "Exception: " + this.message.toString();
  }
  class_method throw: fun(msg) {
    this.new(msg).throw;
  }
end

class ImportError < Exception
  instance_method toString: fun() {
    return "ImportError: " + this.message.toString();
  }
end

class DoesNotUnderstand < Exception
  instance_method toString: fun() {
    return "DoesNotUnderstand: " + this.message.toString();
  }
end

class ArityError < Exception
  instance_method toString: fun() {
    return "ArityError: " + this.message.toString();
  }
end

class CompileError < Exception
  instance_method toString: fun() {
    return "CompileError: " + this.message.toString();
  }
end

  class Mirror
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
  end

// temps
  get_compiled_module: fun(module) {
    <primitive "get_compiled_module">
  }

.end
