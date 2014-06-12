.license
.endlicense

.preamble()

.code

  object Behavior
    _vt: Behavior; //self-referencing
    _delegate: null;
    parent: null;
  end

  object ObjectBehavior
    _vt: Behavior;
    _delegate: null;
    parent: null;
  end

  object Object_CompiledClass
    _vt: CompiledClass; //declared below
    _delegate: null;
    parent: null;
    name: "Object";
    super_class_name: "";
    fields: [];
    methods: {};
    own_methods: {};
  end

  object Object
    _vt: ObjectBehavior;
    _delegate: null;
    parent: null;
    size: 0;
    compiled_class: Object_CompiledClass;
  end

  class CompiledClass
    fields: module, name, super_class_name,
            fields, methods, own_methods;
  end

  // class String
  // end

  // class Symbol
  // end

  class Dictionary
  end

  class List
  end

  // class Number
  // end

  // class CompiledFunction
  // end

  // class Function
  // end

  // class Context
  // end

.end
