start = ['module' :license :meta :pre code_sec]

code_sec = ['.code' !(self.i.new_module()):modobj [definition(modobj)*]]

definition :modobj =  function_definition(modobj)
                   |  class_definition(modobj)

function_definition :modobj = ['fun' :name  !(modobj.new_function(name)):fnobj
                                params:p  !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj)]]

class_definition :modobj =  ['class' [:name :parent]
                              ['fields' :fields]
                                !(modobj.new_class(name, parent, fields)):klass
                              constructors(klass)
                              [instance_method(klass)*]
                              [class_method(klass)*]]

constructor :modobj = ['ctor' :name !(modobj.new_ctor(name)):fnobj
                        params:p !(fnobj.set_parameters(p))
                        :uses_env !(fnobj.uses_env(uses_env))
                        ['body' body(fnobj)]]

constructors :modobj = ['ctors' [constructor(modobj)*]]
                     | ['ctors' []]

instance_method :klass = ['fun' :name  !(klass.new_instance_method(name)):fnobj
                                params:p !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj)]]

class_method :klass = ['fun' :name  !(klass.new_class_method(name)):fnobj
                                params:p !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj)]]

params = ['params' []]  -> []
       | ['params' :xs] -> xs

body :fnobj = [(expr(fnobj)+):b] -> b
            | [['primitive' ['literal-string' :name]]:ast (:ignore)*]   -> fnobj.set_primitive(name)

exprs :fnobj = expr(fnobj)+

expr :fnobj =  ['var-def' :id expr(fnobj)]       -> fnobj.emit_var_decl(id)
            | ['return' expr(fnobj)]             -> fnobj.emit_return_top()
            | ['return-this']                    -> fnobj.emit_return_this()
            | ['send-or-local-call' :name args(fnobj):arity]         -> fnobj.emit_send_or_local_call(name, arity)
            | ['send' :e :s args(fnobj):arity] apply('expr' fnobj e) -> fnobj.emit_send(s, arity)
            | atom(fnobj)

atom :fnobj = ['literal-number' :x]   -> fnobj.emit_push_num_literal(x)
              | ['id' :name]          -> fnobj.emit_push_var(name)

args :fnobj =  ['args' arglist(fnobj):arity] -> arity
            |  ['args' []] -> 0

arglist :fnobj = [expr(fnobj)+]:x -> len(x)
