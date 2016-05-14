ometa OMeta {

  ometa = keyword("ometa") identifier:name inheritance:i  "{"
              inline:code
              rules:r
          "}" => [:grammar, name, i, r];


  inline = "{{" { ~"}}" _ }*:c "}}" => c.join("");

  keyword :xs = token(xs):i ~identifier_rest => xs;

  inheritance = "<:" identifier:i => [:parent, i]
              | => [:parent, "OMetaBase"]
              ;

  rules = rule+;


  rule = identifier:name !{@current_production = name} rule_rest(name):r ";" => r;

  rule_rest :name =  action:ac                    => [:rule, name, ac]
                  |  "=" choices:c                => [:rule, name, c]
                  |  argument+:args  action:ac    => [:rule, [:args] + args] + ac
                  |  argument+:args "=" choices:c => [:rule, [:args] + args, c]
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

  term  = '~'  element:e => [:not, e]
        |  '&'  element:e => [:lookahead, e]
        |  element
        ;

  binding = ':' identifier:i => this.add_local_var(i)
          ;


  element =  prod_app
          |  data_element
          |  "?{" until("}"):s "}" => [:sem_pred, s.join("")]
          |  "!{" until("}"):s "}" => [:sem_action, s.join("")]
          |  "{" choices:c "}" => c
          ;


  data_element =  char_sequence
               |  token_string
               |  string_object
               |  asymbol
               |  s_expr
               ;


  action  = "=>" until("}",";","\n"):s => [:action, s];


  prod_app =  keyword("_") => [:apply, :anything]
            | keyword("$") => [:apply, :end]
            | identifier:p "(" prod_args:args ")" => [:apply_with_args, args, p.toSymbol]
            | identifier:p => [:apply, p.toSymbol]
            | "^" => [:apply_super, this.current_production]
            ;

  prod_args = prod_arg:x {"," prod_arg}*:xs => [x] + xs;

  prod_arg = data_element | identifier:i => [:id, i];

  char_sequence = spaces '\'' {~'\'' '\\' char:c => escaped(c) | ~'\'' char}*:cs '\''
                  => [:seq, cs.join("")]
                ;

  token_string = spaces '"' {~'"' '\\' char:c => escape(c) | ~'"' char}*:cs '"'
                  => [:token_string, cs.join("")]
               ;

  string_object = spaces '`' {~'`' '\\' char:c => escape(c) | ~'`' char}*:cs '`'
                  => [:string_object, cs.join("")]
                ;

  asymbol = ":" identifier:s => [:symbol, s];

  s_expr = "[" choice:s "]" => [:form, s];

  space = '/*' { ~'*/' _ }* '*/'
          | '//' { ~'\n' _}* '\n'
          | ^
          ;
}
