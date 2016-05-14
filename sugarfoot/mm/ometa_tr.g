ometa OMetaTranslator {

  ometa = [:grammar string:name inheritance:base rules:r] => (["class ", name, " < ", base] + r).join("");

  inheritance = [:parent string:base] => base;

  rules = [rule+];

  rule = [:rule _:name rule_args:args body:p];

  rule_args = [:args _+:args] => args.join(",")
            | => ""

  body = [:and body+:p]
       | [:and]
       | [:or body+:p]
       | pattern
       ;

  pattern = [:bind _:name body:e]
          | [:action _:ac]
          | expression
          ;

  expression = [:apply _:s]
             | [:apply_with_args [prod_arg+:args] _:r]
             | [:apply_super _]
             | [:seq str:s]
             | [:token_string string:s]
             | [:many body:x]
             | [:many1 body:x]
             | [:not body:x]
             | [:optional body:x]
             | [:form body:x]
             | [:symbol an_atom:x]
             | [:string_object str:s]
             | [:sem_pred str:s]
             | [:sem_action str:s]
             | [:lookahead body:x]
             ;

  prod_arg = [:seq str:s]
           | [:token_string string:s]
           | [:string_object string:s]
           | [:symbol an_atom:x]
           | [:form body:x]
           | [:id string:s]
           ;

  string = _:str &(Mirror.vt(str)==String) => str;
  symbol = _:sym &(Mirror.vt(sym)==Symbol) => sym;
}
