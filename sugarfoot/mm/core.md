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
    fields: name, super_class_name, fields,
            methods, class_methods, module;
    instance_method fullName: fun() {
      return @module.fullName() + ":" + @name;
    }
  end

  class CompiledFunction
   fields: name, params, is_ctor, is_prim, prim_name, accessor_flags,
           accessor_field, owner, len_params, has_env, is_top_level,
           outer_cfun, storage_size, env_offset, lit_frame_size,
           literal_frame_addr, bytecode_size, bytecode_addr,
           exception_frames_len, exception_frame_addr,
           env_table, text, line_mappings,
           loc_mappings;
    init withEnv: fun(text, scope_names, cmod) {
      <primitive "compiled_function_with_env">
    }
    init withFrame: fun(text, frame, cmod) {
      <primitive "compiled_function_with_frame">
    }
    instance_method new_context: fun(fp, module) {
      <primitive "compiled_function_new_context">
      //return Context.new(this, fp, module);
    }
    instance_method asContextWithVars: fun(imod, vars_dict) {
      <primitive "compiled_function_as_context_with_vars">
    }
    instance_method text: fun() {
      <primitive "compiled_function_get_text">
    }
    // instance_method line_mapping: fun() {
    //   <primitive "compiled_function_get_line_mapping">
    // }
    // instance_method loc_mapping: fun() {
    //   <primitive "compiled_function_get_loc_mapping">
    // }
    instance_method source_location_for: fun(frame) {
      <primitive "compiled_function_loc_for">
    }
    instance_method lineFor: fun(frame) {
      if (@is_prim) {
        return 0;
      } else {
        return this.source_location_for(frame)[0];
      }
    }
    instance_method fullName: fun() {
      return @owner.fullName + "/" + @name;
    }
    instance_method isTopLevel: fun() {
      return @is_top_level;
    }
    instance_method outerCompiledFunction: fun() {
      return @outer_cfun;
    }
    instance_method env_table: fun() {
      return @env_table;
    }
    instance_method recompile: fun(text) {
      <primitive "compiled_function_recompile">
    }
  end

  class CompiledModule
   fields: name, license, params, default_params, aliases, functions, classes,
           parent_module;
  instance_method fullName: fun() {
    return @name;
  }
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
  instance_method toSource: fun() {
    return "\"" + this.replace_all("\"", "\\"") + "\"";
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
  instance_method rindex: fun(arg) {
    <primitive "string_rindex">
  }
  instance_method from: fun(idx) {
    <primitive "string_from">
  }
  instance_method replace_all: fun(what, value) {
    <primitive "string_replace_all">
  }
  end

  class Symbol
  instance_method toString: fun() {
    <primitive "symbol_to_string">
  }
  instance_method toSource: fun() {
    return ":" + this.toString;
  }
  end

  class Dictionary
  instance_method toString: fun() {
    <primitive "dictionary_to_string">
  }
  instance_method toSource: fun() {
    <primitive "dictionary_to_source">
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
  instance_method has: fun(key) {
    <primitive "dictionary_has">
  }
  instance_method keys: fun() {
    <primitive "dictionary_keys">
  }
  instance_method size: fun() {
    <primitive "dictionary_size">
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
  instance_method map: fun(fn) {
    <primitive "list_map">
  }
  instance_method has: fun(value) {
    <primitive "list_has">
  }
  instance_method toString: fun() {
    <primitive "list_to_string">
  }
  instance_method toSource: fun() {
    <primitive "list_to_source">
  }
  instance_method size: fun() {
    <primitive "list_size">
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
    instance_method >: fun(arg) {
      <primitive "number_gt">
    }
    instance_method >=: fun(arg) {
      <primitive "number_gteq">
    }
    instance_method toString: fun() {
      <primitive "number_to_string">
    }
    instance_method toSource: fun() {
      <primitive "number_to_source">
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
    <primitive "context_new">
    // @compiled_function = cfun;
    // @module = module;
    // @env = env;
  }
  instance_method compiledFunction: fun() {
    return @compiled_function;
  }
  instance_method getEnv: fun() {
    <primitive "context_get_env">
  }
  class_method withVars: fun(code, vars, imod) {
    var cmod = get_compiled_module(imod);
    var cfn = CompiledFunction.withEnv(code, vars.keys(), cmod);
    return cfn.asContextWithVars(imod, vars);
  }
  class_method withFrame: fun(code, frame, imod) {
    <primitive "context_with_frame">
    // var cmod = get_compiled_module(imod);
    // var cfn = CompiledFunction.withFrame(code, frame, cmod);
    // return cfn.new_context(frame.fp, imod);
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

class TypeError < Exception
  instance_method toString: fun() {
    return "TypeError: " + this.message.toString();
  }
end

class IndexError < Exception
  instance_method toString: fun() {
    return "IndexError: " + this.message.toString();
  }
end

class InternalError < Exception
  instance_method toString: fun() {
    return "InternalError: " + this.message.toString();
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
    //instance_method fields: fun() { //strings only
    //  <primitive "mirror_fields">
    //}
    instance_method entries: fun() { //can be a list of anything
      <primitive "mirror_entries">
    }
    instance_method valueFor: fun(entry) { //requirement: entry \in self.entries()
      <primitive "mirror_value_for">
    }
    instance_method setValueFor: fun(entry, value) { //requirement: entry \in self.entries()
      <primitive "mirror_set_value_for">
    }
    class_method vtFor: fun(obj) {
      <primitive "mirror_vt">
    }
  end


// temps
  class Process
    fields: self;
  instance_method stepInto: fun() {
    <primitive "process_step_into">
  }
  instance_method stepOver: fun() {
    <primitive "process_step_over">
  }
  instance_method stepOverLine: fun() {
    <primitive "process_step_over_line">
  }
  instance_method stepOut: fun() {
    <primitive "process_step_out">
  }
  instance_method reloadFrame: fun() {
    <primitive "process_reload_frame">
  }
  instance_method recompileCurrentFunction: fun(text) {
    this.cp.compiledFunction.recompile(text);
  }
  instance_method returnFromFrame: fun(val) {
    <primitive "process_return_from_frame">
  }

  instance_method cp: fun() {
     <primitive "process_cp">
  }
  // instance_method ip: fun() {
  //   <primitive "process_ip">
  // }
  instance_method frames: fun() {
    <primitive "process_frames">
  }
  // instance_method apply: fun(fn) {
  //   <primitive "process_apply">
  // }
  // instance_method evalInFrame: fun(text, frame_idx) {
  //   <primitive "process_eval_in_frame">
  // }
  end

  class Frame
  fields: self;
  instance_method cp: fun() {
    <primitive "frame_cp">
  }
  instance_method ip: fun() {
    <primitive "frame_ip">
  }
  instance_method fp: fun() {
    <primitive "frame_fp">
  }
  instance_method rp: fun() {
    <primitive "frame_rp">
  }
  instance_method dp: fun() {
    <primitive "frame_dp">
  }
  instance_method get_local_value: fun(idx) {
    <primitive "frame_get_local_value">
  }
  end

  get_current_process: fun() {
    <primitive "get_current_process">
  }

  get_current_frame: fun() {
    <primitive "get_current_frame">
  }

  get_compiled_module: fun(module) {
    <primitive "get_compiled_module">
  }

  get_module: fun(name) {
    <primitive "get_module">
  }

  modules_path: fun() {
     <primitive "modules_path">
  }

  debug: fun() {
     <primitive "test_debug">
  }
.end
