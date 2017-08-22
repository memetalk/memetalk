.preamble(ometa_base)
  ometa_base: meme:ometa_base;
  [OMetaBase] <= ometa_base;
.code

class MemeScriptTranslator < OMetaBase
fields: proc;
init new: fun(proc, input) {
  super.new(input);
  @proc = proc;
}
<ometa>
start = [:module _:license _:meta
         !{@proc.new_module()}:modobj
         preamble(modobj) code_sec(modobj)];

preamble :modobj = [:preamble _:params !{modobj.set_params(params)}
                   [module_default_param(modobj)*] [module_alias(modobj)*]];

module_default_param :modobj = [:param _:lhs [:library _:ns _:name]]
                               => modobj.add_default_param(lhs, ns, name);

module_alias :modobj = [:alias _:libname _:alias] => modobj.module_alias(libname, alias);


code_sec :modobj = [:code ~~[load_top_level_name(modobj)*] [definition(modobj)*]];

load_top_level_name :modobj = [:class [:name _] _*] => modobj.add_top_level_name(name)
                            | [:object _:name _*] => modobj.add_top_level_name(name)
                            | [:fun _:name _*] => modobj.add_top_level_name(name)
                            ;

definition :modobj =  function_definition(modobj)
                   |  class_definition(modobj)
                   |  object_definition(modobj)
                   ;

function_definition :modobj =  !{this.input.head()}:ast
                               [:fun _:name params:p  !{modobj.new_function(name, p)}:fnobj
                                     !{fnobj.set_line(ast)}
                                     _:uses_env !{fnobj.uses_env(uses_env)}
                                     !{fnobj.body_processor}:bproc
                                     [:body body(bproc)]] !{fnobj.set_text(ast.text)};


class_definition :modobj =  [:class [:name _:parent]
                              [:fields _:fields]
                                !{modobj.new_class(name, parent, fields)}:klass
                              constructors(klass)
                              [instance_method(klass)*]
                              [class_method(klass)*]];

constructor :klass =  !{this.input.head()}:ast
                        [:ctor _:name params:p !{klass.new_ctor(name, p)}:fnobj
                             !{fnobj.set_line(ast)}
                             _:uses_env !{fnobj.uses_env(uses_env)}
                             !{fnobj.body_processor}:bproc
                             [:body body(bproc)]]  !{fnobj.set_text(ast.text)};

constructors :klass = [:ctors [constructor(klass)*]]
                     | [:ctors []];

instance_method :klass = !{this.input.head()}:ast
                           [:fun _:name params:p !{klass.new_instance_method(name, p)}:fnobj
                                  !{fnobj.set_line(ast)}
                                  _:uses_env !{fnobj.uses_env(uses_env)}
                                  !{fnobj.body_processor}:bproc
                                  [:body body(bproc)]]  !{fnobj.set_text(ast.text)};

class_method :klass = !{this.input.head()}:ast
                         [:fun _:name  !{klass.new_class_method(name)}:fnobj fparams(fnobj):p !{fnobj.set_params(p)}
                          !{fnobj.set_line(ast)}
                          _:uses_env !{fnobj.uses_env(uses_env)}
                          !{fnobj.body_processor}:bproc
                          [:body body(bproc)]]  !{fnobj.set_text(ast.text)};

fparams :obj = [:params [fparam(obj)*:x]] => x;

fparam :obj  = [:var-arg _:x !{obj.set_vararg(x)}] => x
               | _:x
               ;

params = [:params [param*:x]] => x;

param = [:var-arg _:x] => x
        | _:x
        ;

object_definition :modobj = [:object _:name !{modobj.new_object(name)}:obj
                            [obj_slot(obj)+] [obj_function(obj)*]];

obj_slot :obj = [:slot obj_slot_value(obj)];

obj_slot_value :obj  =  _:name [:literal-number _:x]       => obj.add_slot_literal_num(name,x)
                     |  _:name [:literal-string _:x]       => obj.add_slot_literal_string(name,x)
                     |  _:name [:literal :null]            => obj.add_slot_literal_null(name)
                     |  _:name [:literal-array [anything*]:x] => obj.add_slot_literal_array(name, x)
                     |  _:name [:literal-dict anything*:x]  => obj.add_slot_literal_dict(name, x)
                     |  _:name _:x                          => obj.add_slot_ref(name, x)
                     ;

obj_function :obj = constructor(obj)
                  | function_definition(obj);


body :fnobj = [expr(fnobj)* [:end-body]]:ast => fnobj.emit_return_this(ast)
            | [[:primitive [:literal-string _:name]] anything*]   => fnobj.set_primitive(name)
            ;

exprs :fnobj = [expr(fnobj)*];

expr_elif :fnobj :lb = [:elif expr(fnobj) !{fnobj.emit_jz()}:label [expr(fnobj)* !{fnobj.emit_jmp(lb)}]] !{label.as_current()}
                     ;

stm :fnobj :ast = :var-def _:id expr(fnobj) =>  fnobj.emit_var_decl(ast, id)
               | :return expr(fnobj)       =>  fnobj.emit_return_top(ast)
               | :return-null => fnobj.emit_return_null(ast)
               | :return-top => fnobj.emit_return_top(ast)
               | :non-local-return expr(fnobj) =>  fnobj.emit_non_local_return(ast)
               | :super-ctor-send _:s args(fnobj):arity => fnobj.emit_super_ctor_send(ast, s, arity)
               | :send-or-local-call _:name args(fnobj):arity => fnobj.emit_send_or_local_call(ast, name, arity)
               | :super-send args(fnobj):arity => fnobj.emit_super_send(ast, arity)
               | :send _:e _:s args(fnobj):arity expr(fnobj, e) => fnobj.emit_send(ast, s, arity)
               | :call _:e args(fnobj):arity expr(fnobj, e) => fnobj.emit_call(ast, arity)
               | :expression expr(fnobj) => fnobj.emit_pop(ast)
               | :not expr(fnobj) => fnobj.emit_unary(ast, "!")
               | :negative expr(fnobj) => fnobj.emit_unary(ast, "neg")
               | :bit-neg expr(fnobj) => fnobj.emit_unary(ast, "~")
               | :and _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "and")
               | :or _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "or")
               | :+  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "+")
               | :-  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "-")
               | :*  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "*")
               | :/  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "/")
               | :&  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "&")
               | :|  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "|")
               | :<  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "<")
               | :<  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "<")
               | :<<  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "<<")
               | :>>  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, ">>")
               | :<= _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "<=")
               | :>  _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, ">")
               | :>= _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, ">=")
               | :== _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "==")
               | :!= _:e expr(fnobj) expr(fnobj, e) => fnobj.emit_binary(ast, "!=")
               | :if expr(fnobj) !{fnobj.emit_jz()}:label [expr(fnobj)* !{fnobj.emit_jmp()}:lb2] !{label.as_current()} [expr_elif(fnobj, lb2)*] [expr(fnobj)*] !{lb2.as_current()}
               | :while !{fnobj.current_label(false)}:lbcond
                   expr(fnobj)
                   !{fnobj.emit_jz()}:lbend [expr(fnobj)*] !{fnobj.emit_jmp_back(lbcond.as_current())} => lbend.as_current()
               | :try
                  !{fnobj.current_label()}:label_begin_try
                    [expr(fnobj)*]
                  !{fnobj.emit_catch_jump()}:end_pos
                  !{fnobj.current_label()}:label_begin_catch
                    catch_decl:cp
                  !{fnobj.bind_catch_var(cp[1])}
                    [expr(fnobj)*] => fnobj.emit_try_catch(label_begin_try, label_begin_catch, end_pos, cp[0])
               | := [:id _:v] expr(fnobj)    => fnobj.emit_local_assignment(ast, v)
               | := [:index _:lhs expr(fnobj)] expr(fnobj) expr(fnobj, lhs)  => fnobj.emit_index_assignment(ast)
               | := [:field _:f] expr(fnobj) => fnobj.emit_field_assignment(ast, f)
               | :literal-number _:x => fnobj.emit_push_num_literal(ast, x)
               | :literal :this => fnobj.emit_push_this(ast)
               | :literal-string _:x => fnobj.emit_push_str_literal(ast, x)
               | :literal-symbol _:x => fnobj.emit_push_sym_literal(ast, x)
               | :literal :null       => fnobj.emit_push_null(ast)
               | :literal :true       => fnobj.emit_push_true(ast)
               | :literal :false      => fnobj.emit_push_false(ast)
               | :literal :module     => fnobj.emit_push_module(ast)
               | :literal :context    => fnobj.emit_push_context(ast)
               | :id _:name             => fnobj.emit_push_var(ast, name)
               | :field _:name          => fnobj.emit_push_field(ast, name)
               | :literal-array  _:e exprs(fnobj, e)      => fnobj.emit_push_list(ast, len(e))
               | :literal-dict   dict_pairs(fnobj):p   => fnobj.emit_push_dict(ast, len(p))
               | :index _:e expr(fnobj) expr(fnobj, e)    => fnobj.emit_push_index(ast)
    ;

expr :fnobj = !{this.input.head()}:ast [stm(fnobj, ast)]
            | funliteral(fnobj)
            ;


catch_decl = [:catch [:id _:type] _:id]  => [type, id]
           | [:catch _:id]               => [null, id]
           ;

dict_pairs :fnobj = {[:pair expr(fnobj) expr(fnobj)]}*:e => e;

funliteral :fnobj = !{this.input.head()}:ast [:fun-literal  params:p
                       !{fnobj.new_closure(p)}:fn
                       !{fn.set_line(ast)}
                       [:body [expr(fn)*]]]:ast
                     !{fn.set_text(ast.text)}
                      => fnobj.emit_push_closure(ast, fn);

cfunliteral :fnobj =  !{this.input.head()}:ast
                      !{fnobj.set_line(ast)}
                      [expr(fnobj)*] => fnobj;

args :fnobj =  [:args []] => 0
            |  [:args arglist(fnobj):arity] => arity
            ;

arglist :fnobj = [expr(fnobj)+]:x => x.size;

</ometa>

end //MemeScriptTranslator

.endcode
