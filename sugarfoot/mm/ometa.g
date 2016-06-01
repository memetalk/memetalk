.preamble(io, ometa_base)
  io: meme:io;
  ometa_base: meme:ometa_base;
  [OMetaBase, OMetaException] <= ometa_base;
.code

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

  mm_module = prologue_code:p rules:r epilogue_code:e
            => [:module, p, r, e]
            ;

  prologue_code = {~'<ometa>' _}*:x '<ometa>' spaces => x.join("");

  epilogue_code = "</ometa>" _*:r => r.join("");

  ometa = keyword("ometa") identifier:name inheritance:i  "{"
              rules:r
          "}"
        => [:grammar, name, i, r];

  keyword :xs = token(xs) ~identifier_rest => xs;

  inheritance = "<:" identifier:i => [:parent, i]
              | => [:parent, "OMetaBase"]
              ;

  rules = rule+;


  rule = identifier:name !{@current_production = name} rule_rest(name):r ";" => r;

  rule_rest :name =  action:ac                   => [:rule, name, ac]
                  |  "=" choices:c               => [:rule, name, c]
                  |  binding+:args  action:ac    => [:rule, name, [:args] + args, ac]
                  |  binding+:args "=" choices:c => [:rule, name, [:args] + args, c]
                  ;


  choices  = choice:x { "|" choice }*:xs => [:or, x] + xs;

  choice   = top_expression*:x action:ac => [:and] + x + [ac]
           | top_expression*:x => [:and] + x
           ;


  top_expression =   bind_expression
                 |   repeated_expression
                 ;


  bind_expression = repeated_expression:e binding:b => [:bind, b, e]
                  ;


  repeated_expression = term:e '*' => [:many, e]
                      | term:e '+' => [:many1, e]
                      | term:e '?' => [:optional, e]
                      | term
                      ;

  term  = "~"  element:e => [:not, e]
        |  "&"  element:e => [:lookahead, e]
        |  element
        ;

  binding = ":" identifier
          ;


  element =  prod_app
          |  data_element
          |  "?{" {~'}' _}*:s '}' => [:sem_pred, s.join("")]
          |  "!{" {~'}' _}*:s '}' => [:sem_action, s.join("")]
          |  "{" choices:c "}" => c
          ;


  data_element =  char_sequence
               |  token_string
               |  string_object
               |  asymbol
               |  s_expr
               ;


  action  = "=>" until(";|\n"):s => [:action, s.join("")];


  prod_app =  keyword("_") => [:apply, :anything]
            | keyword("$") => [:apply, :end]
            | identifier:p "(" prod_args:args ")" => [:apply_with_args, args, p.toSymbol]
            | identifier:p => [:apply, p.toSymbol]
            | "^" => [:apply_super, @current_production]
            ;

  prod_args = prod_arg:x {"," prod_arg}*:xs => [x] + xs;

  prod_arg = data_element | identifier:i => [:id, i];

  char_sequence = "'" {~'\'' '\\' char:c => escaped(c) | ~'\'' char}+:cs '\''
                  => [:seq, cs.join("")]
                ;

  token_string = "\"" {~'"' '\\' char:c => escaped(c) | ~'"' char}+:cs '"'
                  => [:token_string, cs.join("")]
               ;

  string_object = "`" {~'`' '\\' char:c => escaped(c) | ~'`' char}+:cs '`'
                  => [:token_string, cs.join("")]
                ;

  asymbol = ":" identifier:s => [:symbol, s];

  s_expr = "[" choice:s "]" => [:form, s];

</ometa>

end //OMeta

.endcode
