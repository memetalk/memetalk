meme central:memescript/0.0.1/compiler
requires ometa_base
where
  ometa_base = central:ometa/0.0.1/ometa_base
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


class OMeta < OMetaBase
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

  char_sequence = "'" {~'\'' '\\' char:c => escaped(c) | ~'\'' char}+:cs '\''
                  => [:seq, cs.join("")]
                ;

  token_string = "\"" {~'"' '\\' char:c => escaped(c) | ~'"' char}+:cs '"'
                  => [:token_string, cs.join("")]
               ;

  keyword_string = "``" {~'`' '\\' char:c => escaped(c) | ~'`' char}+:cs '``'
                  => [:keyword_string, cs.join("")]
                ;

  string_object = "`" {~'`' '\\' char:c => escaped(c) | ~'`' char}+:cs '`'
                  => [:string_object, cs.join("")]
                ;

  alpha =  '+' | '*' | '-' | '/' | '=' | '<' | '>' | '?' | '!' | '&' | '|';

  asymbol = ":" {letter|alpha}:l {letter|digit|'_'|alpha}*:s => [:symbol, l + s.join("")];

  s_expr = "[" choice:s "]" => [:form, s];

  mm_module = prologue_code:p rules:r epilogue_code:e
            => [:module, p, r, e]
            ;

  prologue_code = {~'<ometa>' _}*:x '<ometa>' spaces => x.join("");

  epilogue_code = "</ometa>" _*:r => r.join("");


  rules = rule+:x => x;


  rule = identifier:name !{@current_production = name} rule_rest(name):r ";" => r;

  rule_rest :name =  action:ac                   => [:rule, name, ac]
                  |  "=" choices:c               => [:rule, name, c]
                  |  prod_param+:params  action:ac    => [:rule, name, [:args] + params, ac]
                  |  prod_param+:params "=" choices:c => [:rule, name, [:args] + params, c]
                  ;
  prod_param = spaces binding:b => b;

  choices  = choice:x { "|" choice }*:xs => [:or, x] + xs;

  choice   = top_expression*:x action:ac => [:and] + x + [ac]
           | top_expression*:x => [:and] + x
           ;


  top_expression =   bind_expression
                 |   repeated_expression
                 |   "#!" => [:ometa-ns-position]
                 |   "#" => [:ometa-position]
                 ;


  bind_expression = repeated_expression:e binding:b => [:bind, b, e]
                  ;


  repeated_expression = term:e '*' => [:many, e]
                      | term:e '+' => [:many1, e]
                      | term:e '?' => [:optional, e]
                      | term
                      ;

  term  = "~"  "~" element:e => [:not, [:not, e]]
        | "~"      element:e => [:not, e]
        | "&"      element:e => [:lookahead, e]
        | element
        ;

  binding = ':' first_and_rest(:identifier_first, :identifier_rest):s => s.join("")
          ;


  element =  prod_app
          |  data_element
          |  "?{" host_expr:s '}' => [:sem_pred, s]
          |  "!{" host_expr:s '}' => [:sem_action, s]
          |  "{" choices:c "}" => c
          ;


  data_element =  char_sequence
               |  token_string
               |  keyword_string
               |  string_object
               |  asymbol
               |  s_expr
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

  prod_app =  ``_`` => [:apply, :anything]
            | ``$`` => [:apply, :end]
            | identifier:p "(" prod_args:args ")" => [:apply_with_args, args, p.toSymbol]
            | identifier:p => [:apply, p.toSymbol]
            | "^" => [:apply_super, @current_production]
            ;

  prod_args = prod_arg:x {"," prod_arg}*:xs => [x] + xs;

  prod_arg = data_element | identifier:i => [:id, i];

</ometa>

end //OMeta
