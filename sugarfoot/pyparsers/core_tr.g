start = ['module' :license :meta :pre code_sec]

code_sec = ['.code' [definition*]]

definition = object_definition | class_definition | function_definition

object_definition = ['object' :name !(self.i.register_object(name)) [obj_slot+] [obj_function*]]

class_definition = ['class' [:name :parent] !(self.i.register_class(name,parent))
                    ['fields' :fields !(self.i.add_class_fields(fields))]
                    class_constructors
                    [imethod_definition*]
                    [cmethod_definition*]]

function_definition = ['fun' :name  params:p
                      ['body' body:b]] -> self.i.add_module_function(name, p, b)

class_constructors = ['ctors' [class_constructor*]]
                   | ['ctors' []]

class_constructor = ['ctor' :name params:p
                     ['body' body:b]]:f -> self.i.add_class_ctor(name, p, b)

imethod_definition = ['fun' :name  params:p
                      ['body' body:b]] -> self.i.add_class_method(name, p, b)

cmethod_definition = ['fun' :name  params:p
                      ['body' body:b]] -> self.i.add_class_self_method(name, p, b)

obj_slot = ['slot' :name obj_slot_value(name)]

obj_slot_value :name =  ['literal-number' :x]       -> self.i.add_slot_literal_num(name,x)
                     |  ['literal-string' :x]       -> self.i.add_slot_literal_string(name,x)
                     |  ['literal' 'null']          -> self.i.add_slot_literal_null(name)
                     |  ['literal-array' (:any)*:x] -> self.i.add_slot_literal_array(name, x)
                     |  ['literal-dict' (:any)*:x]  -> self.i.add_slot_literal_dict(name, x)
                     |  :x                          -> self.i.add_slot_ref(name, x)

obj_function = constructor
             | obj_function_definition

constructor = ['ctor' :name params:p
                ['body' body:b]]:f -> self.i.add_fun(name, p, b, True)

obj_function_definition = ['fun' :name  params:p
                           ['body' body:b]] -> self.i.add_fun(name, p, b, False)

params = ['params' []]  -> []
       | ['params' :xs] -> xs

body = [(expr+):b] -> b

exprs = expr+

expr = ['var-def' :id expr]
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
