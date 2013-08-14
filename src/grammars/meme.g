alpha =  '+' | '*' | '-' | '/' | '=' | '<' | '>' | '?' | '!'

keyword = (token("fun") | token("var")) ~letterOrDigit

id = spaces ~keyword (letter | '_'):x (letterOrDigit|'_')*:xs !(xs.insert(0, x)) -> ''.join(xs)

alpha_name = spaces ~keyword (alpha | letter | '_'):x (letterOrDigit|'_'|alpha)*:xs !(xs.insert(0, x)) -> ''.join(xs)

letter_or_digit_string = (letter | '_'):x (letterOrDigit|'_')*:xs -> x + ''.join(xs)

space = comment | anything:c ?(c.isspace())
comment = "/*"  (~"*/" anything)* "*/"
        | "//" (~"\n" anything)* "\n"

spaces = space*

start = token("module") id:name module_params:p
           module_preamble*:pre
           module_alias*:aliases
         token("{")
           module_decl*:d
         token("}") -> ["module", name, ["params", p],
                        ["default-params", pre], ["aliases", aliases], ["defs", d]]

module_params = params
              | -> []

module_preamble = id:name token(":") module_spec:s token(";") -> ["param", name, s]

module_spec = library_identifier:lid params:a -> ["library", lid, a]
            | uri:uri params:a -> ["uri", uri, a]

uri = spaces (letterOrDigit|'/'|'.'|':')+:rest -> ''.join(rest)

library_identifier = id:ns token("/") id:mname token("/") version:v -> [ns, mname, v]

version = version:a "." digit+:b -> ''.join(a)+"."+''.join(b)
        | digit+:x -> ''.join(x)

module_alias = spaces token("[") idlist:lst token("]") token("<=") id:x token(";") -> ["alias", x, lst]

module_decl = class_decl | top_level_fun | top_level_fn

class_decl = token("class") id:name (token("<") id | token("<") token("null") | -> "Object"):parent token("{")
                fields:f
                constructors:c
                metho_decl*:m
              token("}") -> ['class', [name, parent], f, c, m]

fields = token("fields") token(":") idlist:xs token(";") -> ['fields', xs]
      | -> ["fields", []]

constructors = constructor*:c -> ["ctors", c]

constructor = spaces
              token("init") alpha_name:name token(":") !(self.input.position):begin
              token("fun") params:p token("{")
                  top_fun_body:body !(self.input.position):end
                token("}")
             -> self.i.ast(begin,['ctor', name, ["params", p],
                  ['body', body + [self.i.sint_ast(end,['return-this'])]]])

top_level_fn = spaces alpha_name:name token(":") !(self.input.position):begin
                expr:e token(";") -> self.i.ast(begin,['fun', name, ['params', []],
                                                              ['body', [e]]])

top_level_fun = spaces alpha_name:name token(":") !(self.input.position):begin
                token("fun")  params:p token("{")
                  top_fun_body:body !(self.input.position):end
                token("}")
                  -> self.i.ast(begin,['fun', name, ["params", p],
                                              ['body', body + [self.i.sint_ast(end,['return-this'])]]])

metho_decl = spaces token("instance_method") alpha_name:name token(":")
             !(self.input.position):begin
                token("fun") params:p token("{")
                  top_fun_body:body !(self.input.position):end
               token("}")
               -> self.i.ast(begin,['fun', name, ["params", p],
                    ['body', body + [self.i.sint_ast(end,['return-this'])]]])
           | spaces token("class_method") alpha_name:name token(":")
             !(self.input.position):begin
                token("fun") params:p token("{")
                  top_fun_body:body !(self.input.position):end
               token("}")
               -> self.i.ast(begin,['func', name, ["params", p],
                    ['body', body + [self.i.sint_ast(end,['return-this'])]]])

params = token("(") idlist:xs token(")") -> xs

idlist = id:x (token(",") id)*:xs -> [x]+xs
          | -> []

top_fun_body = primitive
             | stmts

primitive =  spaces !(self.input.position):begin token("<primitive") lit_string:s token(">") -> [self.i.ast(begin,["primitive", s])]

stmts  = stmt*

stmt = control_expr
     | non_control_expr:e token(";") -> e

non_control_expr = expr_debug
                 | expr_ret
                 | expr_attr
                 | expr
                 | expr_decl

expr_debug = spaces !(self.input.position):begin token("debug") -> self.i.ast(begin,['debug'])
expr_ret =  spaces !(self.input.position):begin token("return") expr:e -> self.i.ast(begin,['return', e])

expr_decl =  spaces !(self.input.position):begin token("var")
              id:name token("=") expr:e -> self.i.ast(begin,["var-def", name, e])

expr_attr =  spaces !(self.input.position):begin lhs:a token("=") expr:b -> self.i.ast(begin,["=", a, b])

lhs = alpha_name:x -> ["id", x]
    | field_name:x -> ["field", x]

control_expr = expr_if
             | expr_while
             | expr_try

expr_if =  spaces !(self.input.position):begin token("if") token("(") expr:e token(")") token("{")
           stmts:xs
           token("}") token("else") token("{") stmts:ys token("}")
          -> self.i.ast(begin,['if', e, xs, ys])
        |  spaces !(self.input.position):begin token("if") token("(") expr:e token(")") token("{")
           stmts:xs
           token("}")
          -> self.i.ast(begin,['if', e, xs])

expr_while = spaces !(self.input.position):begin token("while") token("(") expr:e token(")") token("{")
             stmts:xs
             token("}") -> self.i.ast(begin,['while', e, xs])

expr_try = spaces !(self.input.position):begin token("try") token("{")
             stmts:s_try
           token("}") token("catch") token("(") alpha_name:id token(")") token("{")
             stmts:s_catch
           token("}")
          -> self.i.ast(begin, ['try', s_try, id, s_catch])

expr = spaces expr_or

expr_or =  !(self.input.position):begin expr_or:a token("or") expr_and:b -> self.i.ast(begin,['or', a, b])
        | expr_and

expr_and =  !(self.input.position):begin expr_and:a token("and") expr_eq:b -> self.i.ast(begin,['and', a, b])
         | expr_eq

expr_eq =  !(self.input.position):begin expr_eq:a token("==") expr_rel:b -> self.i.ast(begin,['==', a, b])
        |  !(self.input.position):begin expr_eq:a token("!=") expr_rel:b -> self.i.ast(begin,['!=', a, b])
        | expr_rel

expr_rel =  !(self.input.position):begin expr_rel:a token(">=") expr_add:b -> self.i.ast(begin, ['>=', a, b])
         |  !(self.input.position):begin expr_rel:a token(">") expr_add:b -> self.i.ast(begin, ['>', a, b])
         |  !(self.input.position):begin expr_rel:a token("<=") expr_add:b -> self.i.ast(begin, ['<=', a, b])
         |  !(self.input.position):begin expr_rel:a token("<") expr_add:b -> self.i.ast(begin, ['<', a, b])
         | expr_add

expr_add =  !(self.input.position):begin expr_add:a token("++") expr_mul:b -> self.i.ast(begin, ['++', a, b])
         |  !(self.input.position):begin expr_add:a token("+") expr_mul:b -> self.i.ast(begin, ['+', a, b])
         |  !(self.input.position):begin expr_add:a token("-") expr_mul:b -> self.i.ast(begin, ['-', a, b])
         | expr_mul

expr_mul =  !(self.input.position):begin expr_mul:a token("*") expr_unary:b -> self.i.ast(begin, ['*', a, b])
         |  !(self.input.position):begin expr_mul:a token("/") expr_unary:b -> self.i.ast(begin, ['/', a, b])
         | expr_unary

expr_unary =   spaces !(self.input.position):begin token("+") prim_expr:a  -> self.i.ast(begin, ['positive', a])
            |  spaces !(self.input.position):begin token("-") prim_expr:a  -> self.i.ast(begin, ['negative', a])
            |  spaces !(self.input.position):begin token("!") expr_unary:a -> self.i.ast(begin, ['not', a])
            |  spaces !(self.input.position):begin token("~") expr_unary:a -> self.i.ast(begin, ['bit-neg', a])
            | spaces suffix_expr

suffix_expr = !(self.input.position):begin token("super") token(".") alpha_name:sel args:p
            -> self.i.ast(begin, ['super-ctor-send', sel, ['args', p]])
          |  !(self.input.position):begin suffix_expr:r token(".") alpha_name:sel args:p
            -> self.i.ast(begin, ['send', r, sel, ['args', p]])
          |  !(self.input.position):begin suffix_expr:r token(".") alpha_name:sel
            -> self.i.ast(begin, ['send', r, sel, ['args', []]])
          | !(self.input.position):begin suffix_expr:r token("[") expr:i token("]")
             -> self.i.ast(begin, ['index', r, i])
          | call_expr

call_expr =  !(self.input.position):begin call_expr:r args:p
            -> self.i.ast(begin, ['call', r, ['args', p]])
          |  spaces !(self.input.position):begin id:r args:p
            -> self.i.ast(begin, ['send-or-call', r, ['args',p]])
          | prim_expr

prim_expr = token("(") expr:e token(")") -> e
          | literal
          |  spaces !(self.input.position):begin field_name:x -> self.i.ast(begin, ["field", x])
          |  spaces !(self.input.position):begin alpha_name:x -> self.i.ast(begin, ["id", x])

pair_list = pair:x (token(",") pair)*:xs -> [x]+xs
          | -> []

pair = expr:key token(":") expr:val -> ['pair', key, val]

args = token("(") expr_list:p  token(")") -> p

expr_list = expr:x (token(",") expr)*:xs -> [x]+xs
          | -> []

literal = lit_number
        | lit_string
        | lit_symbol
        | spaces !(self.input.position):begin token("[") expr_list:e token("]")    -> self.i.ast(begin, ['literal-array']+e)
        | spaces !(self.input.position):begin token("{") pair_list:e token("}")    -> self.i.ast(begin, ['literal-dict']+e)
        | token("thisModule")-> ['literal', 'module']
        | token("thisContext")-> ['literal', 'context']
        | token("this")   -> ['literal', 'this']
        | token("null")   -> ['literal', 'null']
        | token("true")   -> ['literal', 'true']
        | token("false")  -> ['literal', 'false']
        | funliteral

funliteral = spaces !(self.input.position):begin token("fun") params:p token("{")
               funliteral_body:body
             token("}") -> self.i.ast(begin, ['fun-literal', ["params", p],
                            ['body', body]])

funliteral_body = stmt:x stmts:xs -> [x]+xs
                | expr_ret:e      -> [e]
                | expr:e          -> [e]
                |                 -> [['literal', 'null']]

lit_symbol = token(":") alpha_name:xs
           -> ["literal-symbol", xs]

lit_number = spaces digit+:ds -> ["literal-number", int(''.join(ds))]

lit_string  = spaces '"' ('\\' '"' | ~'"' :x)*:xs '"'
               -> ["literal-string", unicode(''.join(xs).decode("string_escape"))]
            | spaces '\'' (~'\'' :x)*:xs '\''
               -> ["literal-string", unicode(''.join(xs).decode("string_escape"))]

field_name = spaces '@' letter_or_digit_string:x -> x


single_top_level_fun :name = spaces !(self.input.position):begin token("fun")  params:p token("{")
                         top_fun_body:body !(self.input.position):end
                       token("}")
                       -> self.i.ast(begin,['fun', name, ["params", p],
                              ['body', body + [self.i.sint_ast(end,['return-this'])]]])
