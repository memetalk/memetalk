.license
.endlicense

.preamble()

.code

  object Behavior
    _vt: Behavior; //self-referencing
    _delegate: null;
  end

  object Object_Behavior
    _vt: Behavior;
    _delegate: null;
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
  end

  class CompiledClass
    fields: module, name, super_class_name,
            fields, methods, own_methods;
  end

  class CompiledModule
    instance_method instantiate: fun(args) {
      <primitive "compiled_module_instantiate">
    }
  end

  class String
  end

  class Symbol
  end

  class Dictionary
  end

  class List
  end

  class Number
  end

  class CompiledFunction
  end

  class Function
  end

  class Context
  end
.end
