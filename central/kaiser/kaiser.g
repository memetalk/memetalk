meme central:memescript/compiler
requires ometa_base
where
  ometa_base = central:ometa/ometa_base
  import OMetaBase from ometa_base
end

escaped: fun(chr) {
  if (chr == "b") { return  "\b"; }
  if (chr == "f") { return  "\f"; }
  if (chr == "n") { return  "\n"; }
  if (chr == "r") { return  "\r"; }
  if (chr == "t") { return  "\t"; }
  if (chr == "v") { return  "\v"; }
  return chr;
}


class Kaiser < OMetaBase
fields: current_production, local_vars;
init new: fun(input) {
  super.new(input);
  @local_vars = [];
}

<ometa>
  space = '/*' { ~'*/' _ }* '*/'
        | '//' { ~'\n' _}* '\n'
        | ^
        ;

  rule_name = identifier:id ?{id[0].isLower} => id;

  terminal = identifier:id ?{id[0].isUpper} => id;

  token_string = "\"" {~'"' '\\' char:c => escaped(c) | ~'"' char}+:cs '"'
                  => [:token_string, cs.join("")]
               ;

  start = prologue_code:p tokens:t rules:r epilogue_code:e
           => [:module, p, t, r, e]
        ;

  prologue_code = {~'<kaiser>' _}*:x '<kaiser>' spaces => x.join("");

  epilogue_code = "</kaiser>" _*:r => r.join("");

  tokens = "tokens" "{" token_rules+:r "}" => r;

  token_rules = ``key`` identifier:name "=" token_string:s  ";" => [:key_rule, name, s]
              | ``id``  identifier:name "=" token_string:s  ";" => [:id_rule, name, s]
              | identifier:name "=" { token_string | host_expr}:s  ";" => [:token_rule, name, s]
              ;

  rules = rule+:x => x;

  rule = identifier:name !{@current_production = name} rule_rest(name):r ";" => r;

  rule_rest :name = "=" choices:c               => [:rule, name, [:args], c]
                  |  prod_param+:params "=" choices:c => [:rule, name, [:args] + params, c]
                  ;

  prod_param = spaces binding:b => b;

  choices  = choice:x { "|" choice }+:xs => [:or, x] + xs
           | choice
           ;

  choice   = top_expression*:x action:ac => x + [ac]
           | top_expression*:x => x
           ;


  top_expression =   bind_expression
                 |   repeated_expression
                 |   "#!" => [:ns-position]
                 |   "#" => [:position]
                 ;


  bind_expression = repeated_expression:e binding:b => [:bind, b, e]
                  ;


  repeated_expression = term:x => x;

  term  = "?" element:e => [:lookahead, e]
        | element
        ;

  binding = ':' first_and_rest(:identifier_first, :identifier_rest):s => s.join("")
          ;


  element =  prod_app
          |  data_element
          |  "{" host_expr:s '}' => [:sem_action, s + ";"]
          ;


  data_element =  token_string:x => x
               ;

  host_str = "\"" {~'"' '\\' char:c => "\\" + c | ~'"' char}*:cs '"'
           => "\"" + cs.join("") + "\""
           ;

  host_paren  = '#(' { ~')' host_element }*:e ')' => ["(", e.join(""), ").at(this, _pos)"].join("")
              | '(' { ~')' host_element }*:e ')' => ["(", e.join(""), ")"].join("");

  host_sq_brk = '#[' { ~']' host_element }*:e ']' => ["[", e.join(""), "].at(this, _pos)"].join("")
              | '[' { ~']' host_element }*:e ']' => ["[", e.join(""), "]"].join("");

  host_c_brk  = '{' { ~'}' host_element }*:e '}' => ["{", e.join(""), "}"].join("");

  host_element = host_str
               | host_paren
               | host_sq_brk
               | host_c_brk
               | _
               ;

  host_expr = {~{';'|'}'|'|'} host_element}+:x => x.join("").trim();

  action  = "=>" host_expr:s => [:action, s];

  prod_app =  rule_name:p "(" prod_args:args ")" => [:apply-with-args, args, p]
           |  rule_name:p => [:apply, p]
           |  terminal:t => [:terminal, t]
           ;

  prod_args = prod_arg:x {"," prod_arg}*:xs => [x] + xs;

  prod_arg = data_element | identifier:i => [:id, i] | host_element:e => [:host-expr, e];

</ometa>

end
