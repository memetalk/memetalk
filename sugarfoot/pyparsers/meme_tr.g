start = ['module' :license :meta :pre code_sec]

code_sec = ['.code' !(self.i.new_module()):modobj [definition(modobj)*]]

definition :modobj =  function_definition(modobj)
                   |  class_definition(modobj)
                   |  object_definition(modobj)

function_definition :modobj = ['fun' :name  !(modobj.new_function(name)):fnobj
                                params:p  !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj.body_processor())]]

class_definition :modobj =  ['class' [:name :parent]
                              ['fields' :fields]
                                !(modobj.new_class(name, parent, fields)):klass
                              constructors(klass)
                              [instance_method(klass)*]
                              [class_method(klass)*]]

constructor :klass = ['ctor' :name !(klass.new_ctor(name)):fnobj
                        params:p !(fnobj.set_parameters(p))
                        :uses_env !(fnobj.uses_env(uses_env))
                        ['body' body(fnobj.body_processor())]]

constructors :klass = ['ctors' [constructor(klass)*]]
                     | ['ctors' []]

instance_method :klass = ['fun' :name  !(klass.new_instance_method(name)):fnobj
                                params:p !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj.body_processor())]]

class_method :klass = ['fun' :name  !(klass.new_class_method(name)):fnobj
                                params:p !(fnobj.set_parameters(p))
                                :uses_env !(fnobj.uses_env(uses_env))
                                ['body' body(fnobj.body_processor())]]

params = ['params' []]  -> []
       | ['params' :xs] -> xs


object_definition :modobj = ['object' :name !(modobj.new_object(name)):obj
                          [obj_slot(obj)+] [obj_function(obj)*]]

obj_slot :obj = ['slot' obj_slot_value(obj)]

obj_slot_value :obj  =  :name ['literal-number' :x]       -> obj.add_slot_literal_num(name,x)
                     |  :name ['literal-string' :x]       -> obj.add_slot_literal_string(name,x)
                     |  :name ['literal' 'null']          -> obj.add_slot_literal_null(name)
                     |  :name ['literal-array' (:any)*:x] -> obj.add_slot_literal_array(name, x)
                     |  :name ['literal-dict' (:any)*:x]  -> obj.add_slot_literal_dict(name, x)
                     |  :name :x                          -> obj.add_slot_ref(name, x)

obj_function :obj = constructor(obj)
                  | function_definition(obj)


body :fnobj = [(expr(fnobj)+) ['end-body']] -> fnobj.emit_return_this()
            | [['primitive' ['literal-string' :name]]:ast (:ignore)*]   -> fnobj.set_primitive(name)

exprs :fnobj = expr(fnobj)+

expr :fnobj = ['var-def' :id expr(fnobj)]              -> fnobj.emit_var_decl(id)
            | ['return' expr(fnobj)]                   -> fnobj.emit_return_top()
            | ['super-ctor-send' :s args(fnobj):arity] -> fnobj.emit_super_ctor_send(s, arity)
            | ['send-or-local-call' :name args(fnobj):arity]         -> fnobj.emit_send_or_local_call(name, arity)
            | ['send' :e :s args(fnobj):arity] apply('expr' fnobj e) -> fnobj.emit_send(s, arity)
            | ['pop' expr(fnobj)]                -> fnobj.emit_pop()
            | ['not' expr(fnobj)]                -> fnobj.emit_unary('~')
            | ['negative' expr(fnobj)]           -> fnobj.emit_unary('!')
            | ['and' :e expr(fnobj) apply('expr' fnobj e)]    -> fnobj.emit_binary('and')
            | ['+'  :e expr(fnobj) apply('expr' fnobj e)]      -> fnobj.emit_binary('+')
            | ['-'  :e expr(fnobj) apply('expr' fnobj e)]      -> fnobj.emit_binary('-')
            | ['*'  :e expr(fnobj) apply('expr' fnobj e)]      -> fnobj.emit_binary('*')
            | ['<'  :e expr(fnobj) apply('expr' fnobj e)]      -> fnobj.emit_binary('<')
            | ['<=' :e expr(fnobj) apply('expr' fnobj e)]     -> fnobj.emit_binary('<=')
            | ['>'  :e expr(fnobj) apply('expr' fnobj e)]      -> fnobj.emit_binary('>')
            | ['>=' :e expr(fnobj) apply('expr' fnobj e)]     -> fnobj.emit_binary('>=')
            | ['==' :e expr(fnobj) apply('expr' fnobj e)]     -> fnobj.emit_binary('==')
            | ['!=' :e expr(fnobj) apply('expr' fnobj e)]     -> fnobj.emit_binary('!=')
            | ['if' expr(fnobj)
                !(fnobj.emit_jz()):label [expr(fnobj)*]] -> label.as_current()
            | ['if/else' expr(fnobj)
                !(fnobj.emit_jz()):lb1 [expr(fnobj)*]
                 !(fnobj.emit_jmp()):lb2 !(lb1.as_current())
                [expr(fnobj)*]]                  -> lb2.as_current()
            | ['try'
                !(fnobj.current_label()):label_begin_try
                  [expr(fnobj)*]
                !(fnobj.current_label()):label_begin_catch
                  catch_decl:cp  [expr(fnobj)*]]
                !(fnobj.current_label()):label_out
              -> fnobj.emit_try_catch(label_begin_try, label_begin_catch, label_out, cp[0], cp[1])
            | assignment(fnobj)
            | atom(fnobj)

catch_decl = ['catch' ['id' :type] :id]  -> (type, id)
           | ['catch' :id]               -> (None, id)


assignment :fnobj = ['=' ['id' :v] expr(fnobj)]    -> fnobj.emit_local_assignment(v)
                  | ['=' ['field' :f] expr(fnobj)] -> fnobj.emit_field_assignment(f)

atom :fnobj = ['literal-number' :x]   -> fnobj.emit_push_num_literal(x)
            | ['literal' 'this']      -> fnobj.emit_push_this()
            | ['literal-string' :x]   -> fnobj.emit_push_str_literal(x)
            | ['literal-symbol' :x]   -> fnobj.emit_push_sym_literal(x)
            | ['literal' 'null']      -> fnobj.emit_push_null()
            | ['literal' 'true']      -> fnobj.emit_push_true()
            | ['literal' 'false']     -> fnobj.emit_push_false()
            | ['literal' 'module']    -> fnobj.emit_push_module()
            | ['literal' 'context']   -> fnobj.emit_push_context()
            | ['id' :name]            -> fnobj.emit_push_var(name)
            | ['field' :name]         -> fnobj.emit_push_field(name)

args :fnobj =  ['args' []] -> 0
            |  ['args' arglist(fnobj):arity] -> arity

arglist :fnobj = [expr(fnobj)+]:x -> len(x)
