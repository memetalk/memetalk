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
  end

  object Object
    _vt: ObjectBehavior;
    _delegate: null;
    parent: null;
    compiled_class: Object_CompiledClass;
  end

  class CompiledClass
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

  // class CompiledFunction
  // end

  // class Function
  // end

  // class Context
  // end

.end
