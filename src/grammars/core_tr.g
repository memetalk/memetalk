start = ['module' :name params:p module_definitions]

module_definitions = ['defs' []]
                   | ['defs' [definition+]]


definition = object_definition | class_definition | function_definition

object_definition = ['object' :name !(self.i.register_object(name)) [obj_slot+] [obj_function*]]

class_definition = ['class' [:name :parent] !(self.i.register_class(name,parent))
                    ['fields' :fields !(self.i.add_class_fields(fields))]
                    class_constructors
                    [method_definition*]]

function_definition = ['fun' :name  params:p
                      ['body' body:b]] -> self.i.add_module_function(name, p, b)

class_constructors = ['ctors' [class_constructor*]]
                   | ['ctors' []]

class_constructor = ['ctor' :name params:p
                     ['body' body:b]]:f -> self.i.add_class_ctor(name, p, b)

method_definition = ['fun' :name  params:p
                      ['body' body:b]] -> self.i.add_class_method(name, p, b)
                   | ['func' :name  params:p
                      ['body' body:b]] -> self.i.add_class_self_method(name, p, b)

obj_slot = ['slot' :name obj_slot_value:v] -> self.i.add_slot(name,v)

obj_slot_value =  ['literal-number' :x]       -> str(x)
               |  ['literal-string' :x]       -> self.i.to_source(x)
               |  ['literal' 'null']          -> 'None'
               |  ['literal-array' (:any)*:x] -> self.i.to_source([])
               |  ['literal-dict' (:any)*:x]  -> self.i.to_source({})
               |  :x                          -> x

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

expr = ['var-def' :id !(self.i.l_var_def(id)) expr]
     | ['fun-literal'
         !(self.i.l_enter_literal_fun())
         params:p
         !(self.i.l_set_fun_literal_parameters(p))
         ['body' :b
            !(self.i.l_literal_fun_body(b))
          apply('body' b)]
            !(self.i.l_done_literal_function())]
     | ['send' :r :s ['args' [expr+]]]
     | [:tag expr*]
     | :x
