start = ['module' :license :meta :pre code_sec]

code_sec = ['.code' !(self.i.new_module()):modobj [definition(modobj)*]]

definition :modobj =  function_definition(modobj)

function_definition :modobj = ['fun' :name  !(modobj.new_function(name)):fnobj
                                params:p   !(fnobj.set_parameters(p))
                              ['body' body(fnobj)]]

params = ['params' []]  -> []
       | ['params' :xs] -> xs

body :fnobj = [(expr(fnobj)+):b] -> b

exprs :fnobj = expr(fnobj)+

expr :fnobj = ['return' expr(fnobj):x]   -> fnobj.emit_return(x)
            | ['return-this']            -> fnobj.emit_return_this()
            | atom(fnobj)

atom :fnobj = ['literal-number' :x]   -> fnobj.emit_push_num_literal(x)
