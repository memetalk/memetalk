load_module = ['module' :name
               params:p
               !(self.i.l_module(name, p))
               default_params
               module_aliases
               module_definitions]

default_params = ['default-params' [mparam*]]

mparam = ['param' :name ['library' :spec :args]] -> self.i.l_default_p_lib(name,spec,args)
       | ['param' :name ['uri' :uri :args]]      -> self.i.l_default_p_uri(name, uri, args)


module_aliases = ['aliases' [module_alias*]]

module_alias = ['alias' :libname :alias] -> self.i.l_module_alias(libname, alias)


load_body = body


module_definitions = ['defs' []]
                   | ['defs' [definition+]]

definition = class_definition
           | function_definition


class_definition =  ['class' [:name :parent]
                       ['fields' :fields]
                         !(self.i.l_begin_class(name, parent, fields))
                       constructors
                       [function_definition*]] -> self.i.l_end_class()

constructors = ['ctors' [constructor*]]
             | ['ctors' []]

constructor = ['ctor':type :name
               !(self.i.l_begin_function(name, True))
                 params:p
               !(self.i.l_set_function_parameters(p))
                ['body' body:b]]:f
                 -> self.i.l_end_function(type,b,f)

function_definition = [('fun'|'func'):type :name
                         !(self.i.l_begin_function(name, False))
                      params:p
                         !(self.i.l_set_function_parameters(p))
                      ['body' body:b]]:f
                       -> self.i.l_end_function(type,b,f)

params = ['params' []]  -> []
       | ['params' :xs] -> xs

body = [(expr+):b] -> b


expr = ['var-def' :id !(self.i.l_var_def(id)) expr]
     | ['fun-literal'
                !(self.i.l_enter_literal_fun())
                  params:p
                !(self.i.l_set_fun_literal_parameters(p))
                  ['body' :b
                !(self.i.l_literal_fun_body(b))
                  apply('body' b)]]:f -> self.i.l_done_literal_function(f)
    | ['super-ctor-send' :s ['args' [expr*]]]
    | ['call' expr ['args' [expr*]]]
    | ['setter' expr :s ['args' [expr*]]]
    | ['getter' expr :s]
    | ['send-or-call' :e ['args' [expr*]]]
    | ['send' expr :s ['args' [expr*]]]
    | ['if' :c [expr*]]
    | ['if' :c [expr*] [expr*]]
    | ['while' expr [expr*]]
    | ['literal-dict'  ['pair' expr*]*]
    | [:tag expr*]
    | :x
