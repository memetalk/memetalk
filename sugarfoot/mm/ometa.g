ometa OMeta {

  spacing = '/*' { ~'*/' _ }* '*/'
          | ^
          ;



  ometa = spaces "ometa" identifier:name inheritance:i  "{"
              rules:r
          "}" $;


  inheritance = "<:" identifier:i
              |
              ;


  rules = rule+;


  rule = &{rule_name:rname} <rule_part rname>+:p;


  rule_part :rn = rule_name:rname %(eq rname rn) rule_rest:r ";";

  rule_rest =  action
            |  "="  choices
            |  argument+:args "=" choices:c
            |  argument+:args  action:ac
            ;

  rule_name =  identifier:rname
            ;


  argument = bind_expression
           | binding
           ;

  choices  = choice:x { "|" choice}*:xs;

  choice   = top_expression*:x action:ac
           | top_expression*:x
           ;

  top_expression =   bind_expression
                 |   repeated_expression
                 ;

  bind_expression = repeated_expression:e binding:b
                  ;

  repeated_expression = term:e "*"
                      | term:e "+"
                      | term:e "?"
                      | term:e '[' str_number:rep ']'
                      | term
                      ;

  term  = '~'  element:e
        |  '&'  element:e
        |  element
        ;

  binding = ':' identifier:i
          ;

  element =  prod_app
          |  data_element
          |  '%' host_lang_expr:s
          |  "{" choices:c "}"
          ;

  data_element =  char_sequence
               |  char_sequence_s
               |  string_object
               |  asymbol
               |  s_expr
               |  any_symb
               |  end_symb
               |  s_number
               ;


  action  = "=>" host_lang_expr:s;


  host_lang_expr  = todo   ;

  prod_app = "<" identifier:p ">"
            |  identifier:p
            | "<" identifier:p prod_arg_list:args ">"
            | "^"
            | "<^" prod_arg_list:args ">"
            ;

  prod_arg_list = prod_arg:x {"," prod_arg}*:xs;

  prod_arg = data_element | identifier;

  char_sequence  = '\'' todo;

  string_object = '``' todo;

  asymbol     =  '#' identifier;

  s_expr    =  "(" choice:s ")";

  la_prefix    = "&";

  not_prefix     = "~";
  sem_prefix     = "%";
  end_symb       = "$";
  any_symb       = "_";
}
