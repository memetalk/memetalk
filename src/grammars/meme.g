alpha =  '+' | '*' | '-' | '/' | '=' | '<' '=' | '<' | '>' '=' | '>' | '?' | '!'

keyword = (token("fun") | token("var")) ~letterOrDigit

id = spaces ~keyword (letter | '_'):x (letterOrDigit|'_')*:xs !(xs.insert(0, x)) -> ''.join(xs)

alpha_name = spaces ~keyword (alpha | letter | '_'):x (letterOrDigit|'_'|alpha)*:xs !(xs.insert(0, x)) -> ''.join(xs)

letter_or_digit_string = (letter | '_'):x (letterOrDigit|'_')*:xs -> x+''.join(xs)

space = comment | anything:c ?(c.isspace())
comment = "/*"  (~"*/" anything)* "*/"
        | "//" (~"\n" anything)* "\n"

spaces = space*


start = token("module") id:name module_params:p token("{")
           module_decl*:d
         token("}") -> ["module", name, ["params", p], ["defs", d]]

module_params = params
              | -> []

module_decl = class_decl | top_level_fun

class_decl = token("class") id:name (token("<") id | token("<") token("null") | -> "Object"):parent token("{")
                fields:f
                constructors:c
                metho_decl*:m
              token("}") -> ['class', [name, parent], f, c, m]

fields = token("fields") token(":") idlist:xs token(";") -> ['fields', xs]
      | -> ["fields", []]

constructors = constructor*:c -> ["ctors", c]

constructor = spaces token("init") id:name params:p token("{")
                  top_fun_body:body
                token("}")
             -> ['ctor', name, ["params", p],
                  ['body', body + [['return-this']]]]

top_level_fun = spaces token("fun") id:name params:p token("{")
                  top_fun_body:body
                token("}")
                  -> ['fun', name, ["params", p],
                       ['body', body + [['return-null']]]]

metho_decl = spaces token("fun") alpha_name:name params:p token("{")
                  top_fun_body:body
               token("}")
               -> ['fun', name, ["params", p],
                    ['body', body + [['return-this']]]]

params = token("(") idlist:xs token(")") ->xs

idlist = id:x (token(",") id)*:xs -> [x]+xs
          | -> []

top_fun_body = primitive
             | stmts

primitive = token("<primitive") lit_string:s token(">") -> ["primitive", s]

stmts  = stmt*

stmt = control_expr
     | non_control_expr:e token(";") -> e

non_control_expr = expr_ret
                 | expr_decl
                 | expr_attr
                 | expr

expr_ret = token("return") expr:e -> ['return', e]

expr_decl = token("var")
              id:name token("=") expr:e -> ["var-def", name, e]

expr_attr = lhs:a token("=") expr:b -> ["=", a, b]

lhs = alpha_name:x -> ["id", x]
    | field_name:x -> ["field", x]

control_expr = expr_if
             | expr_while

expr_if = token("if") token("(") expr:e token(")") token("{")
           stmts:xs
           token("}") token("else") token("{") stmts:ys token("}")
          -> ['if', e, xs, ys]
        | token("if") token("(") expr:e token(")") token("{")
           stmts:xs
           token("}")
          -> ['if', e, xs]

expr_while = token("while") token("(") expr:e token(")") token("{")
             stmts:xs
             token("}") -> ['while', e, xs]

expr = expr_or

expr_or = expr_or:a token("or") expr_and:b -> ['or', a, b]
        | expr_and

expr_and = expr_and:a token("and") expr_eq:b -> ['and', a, b]
         | expr_eq

expr_eq = expr_eq:a token("==") expr_rel:b -> ['==', a, b]
        | expr_eq:a token("!=") expr_rel:b -> ['!=', a, b]
        | expr_rel

expr_rel = expr_rel:a token(">=") expr_add:b -> ['>=', a, b]
         | expr_rel:a token(">") expr_add:b -> ['>', a, b]
         | expr_rel:a token("<=") expr_add:b -> ['<=', a, b]
         | expr_rel:a token("<") expr_add:b -> ['<', a, b]
         | expr_add

expr_add = expr_add:a token("++") expr_mul:b -> ['++', a, b]
         | expr_add:a token("+") expr_mul:b -> ['+', a, b]
         | expr_add:a token("-") expr_mul:b -> ['-', a, b]
         | expr_mul

expr_mul = expr_mul:a token("*") expr_unary:b -> ['*', a, b]
         | expr_mul:a token("/") expr_unary:b -> ['/', a, b]
         | expr_unary

expr_unary =  token("+") prim_expr:a  -> ['positive', a]
            | token("-") prim_expr:a  -> ['negative', a]
            | token("!") expr_unary:a -> ['not', a]
            | token("~") expr_unary:a -> ['bit-neg', a]
            | send_expr

send_expr = token("super") token(".") alpha_name:sel token("(") expr_list:p  token(")")
            -> ['super-ctor-send', sel, ['args', p]]
          | send_expr:r token(".") alpha_name:sel token("(") expr_list:p  token(")")
            -> ['send', r, sel, ['args', p]]
          | send_expr:r token(".") alpha_name:sel
            -> ['send', r, sel, ['args', []]]
          | call_expr

call_expr = call_expr:r token("(") expr_list:p  token(")")
            -> ['call', r, ['args', p]]
          | id:r token("(") expr_list:p  token(")")
            -> ['send-or-call', r, ['args',p]]
          | prim_expr

prim_expr = token("(") expr:e token(")") -> e
          | token("[") expr_list:e token("]")    -> ['literal-array']+e
          | token("{") pair_list:e token("}")    -> ['literal-dict']+e
          | literal
          | field_name:x -> ["field", x]
          | alpha_name:x -> ["id", x]

pair_list = pair:x (token(",") pair)*:xs -> [x]+xs
          | -> []

pair = expr:key token(":") expr:val -> ['pair', key, val]

expr_list = expr:x (token(",") expr)*:xs -> [x]+xs
          | -> []

literal = lit_number
        | lit_string
        | lit_symbol
        | token("thisModule")-> ['literal', 'module']
        | token("this")   -> ['literal', 'this']
        | token("null")   -> ['literal', 'null']
        | token("true")   -> ['literal', 'true']
        | token("false")  -> ['literal', 'false']
        | funliteral

funliteral = token("fun") params:p token("{")
               funliteral_body:body
             token("}") -> ['fun-literal', ["params", p],
                            ['body', body]]

funliteral_body = stmt:x stmts:xs -> [x]+xs
                | expr_ret:e      -> [e]
                | expr:e          -> [e]
                |                 -> [['literal', 'null']]

as_eval = token("{") funliteral_body:body token("}") -> body

lit_symbol = token(":") alpha_name:xs
           -> ["literal-symbol", xs]

lit_number = spaces digit+:ds -> ["literal-number", int(''.join(ds))]

lit_string  = spaces '"' ('\\' '"' | ~'"' :x)*:xs '"'
               -> ["literal-string", ''.join(xs).decode("string_escape")]
            | spaces '\'' (~'\'' :x)*:xs '\''
               -> ["literal-string", ''.join(xs).decode("string_escape")]

field_name = spaces '@' letter_or_digit_string:x -> x
