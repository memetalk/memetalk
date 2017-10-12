meme central:memescript/compiler

requires kaiser_base, io
where
  kaiser_base = central:kaiser/kaiser_base
  io         = central:stdlib/io
  import KaiserBase from kaiser_base
  import ParseError from kaiser_base
end

class List < List
fields: start_line, end_line, start_col, end_col, text;
instance_method start_line: fun() {
  return @start_line;
}
instance_method end_line: fun() {
  return @end_line;
}
instance_method start_col: fun() {
  return @start_col;
}
instance_method end_col: fun() {
  return @end_col;
}
instance_method text: fun() {
  return @text;
}
instance_method toSource: fun() {
  var ret = super();
  var pos = "@<" + @start_line.toString + ":" + @start_col.toString + ":" + @end_line.toString + ":" + @end_col.toString + ">";
  return ret + pos;
}
instance_method at: fun(parser, begin_pos_pair) {
  var end_pos = parser.current_position[0];
  var full_input = parser.input; //assert type == String?
  // io.print(begin_pos_pair);
  var begin_pos = begin_pos_pair[0];
  @text = full_input.substr(begin_pos, end_pos - begin_pos);

  @start_line = (begin_pos_pair[1] - 1);
  @start_col = begin_pos - full_input.rindex_from("\n", begin_pos) - 1;
  @end_line = @start_line + @text.count("\n");
  var inside_nl = @text.rindex("\n");
  if (inside_nl == -1) {
    @end_col = @start_col + @text.size;
  } else {
    @end_col = @text.substr(inside_nl, @text.size - inside_nl).size;
  }
}
instance_method +: fun(other) { //#[] + [] yields a Core.List. This is a workaround
  var ret = [];
  this.each(fun(_, x) { ret.append(x) });
  other.each(fun(_, x) { ret.append(x) });
  return ret;
}
instance_method extends: fun(other) {
  other.each(fun(_, x) { this.append(x) });
}
end

class MemeScriptParser < KaiserBase
fields: has_fun_literal, line_offset;
<kaiser>
tokens {
  key T_FUN = "fun";
  key T_VAR = "var";
  key T_CLASS = "class";
  key T_REQUIRES = "requires";
  key T_WHERE = "where";
  key T_IMPORT = "import";
  key T_END = "end";
  key T_INIT = "init";
  key T_INSTANCE_METHOD = "instance_method";
  key T_CLASS_METHOD = "class_method";
  key T_OBJECT = "object";
  key T_THIS_MODULE = "thisModule";
  key T_THIS_CONTEXT = "thisContext";
  key T_THIS = "this";
  key T_NULL = "null";
  key T_TRUE = "true";
  key T_FALSE = "false";
  key T_PRIMITIVE = "primitive";
  key T_IF = "if";
  key T_ELIF = "elif";
  key T_ELSE = "else";
  key T_WHILE = "while";
  key T_TRY = "try";
  key T_CATCH = "catch";
  key T_FOR = "for";
  key T_RETURN = "return";
  key T_OR = "or";
  key T_AND = "and";
  key T_SUPER = "super";

  //"id" qualifier prevents ambiguity when checking for
  //lookaheads that might be keyword as well.
  //E.g. in "import X from Y", the word "import" would
  //also match T_ID, which would lead the parsing to
  // try a default parameter failing at "X", expecting a "="
  id T_ID = "[a-zA-Z_][a-zA-Z0-9_]*";

  //always match against possible keywords
  T_PRIV_ID = "[a-zA-Z_][a-zA-Z0-9_]*";

  //this token is fine without the "id" qualifier as long as
  //we try to parse class declaration before top level functions
  //(i.e. so it matches "class" as a keyword, before it matches
  //"class" as a T_FUN_ID)
  //-or perhaps we should only allow T_FUN_ID for class/instance methods only
  T_FUN_ID = "[a-zA-Z_a-zA-Z_!&\\*\\-\\+\\|<=>/][a-zA-Z_a-zA-Z0-9_!&\\*\\-\\+\\|<=>/]*";

  T_MEME = "meme"; //this shouldn't be a reserved keyword.
  T_FROM = "from"; //this shouldn't be a reserved keyword.
  T_FUNCTIONS = "functions"; //this shouldn't be a reserved keyword.
  T_FIELDS = "fields"; //core.me uses this as an id
  T_NUMBER = "[[:digit:]]+";
  T_HEXNUM = "0x[[:xdigit:]]+";
  T_OPEN_CURLY = "{";
  T_CLOSE_CURLY = "}";
  T_SYMBOL = ":[a-zA-Z_a-zA-Z_!&\\*\\-\\+\\|<=>/][a-zA-Z_a-zA-Z0-9_!&\\*\\-\\+\\|<=>/]*";
  T_AT_ID = "@[a-zA-Z_][a-zA-Z0-9_]*";
  T_COMMA = ",";
  T_SLASH = "/";
  T_COLON = ":";
  T_SEMICOLON = ";";
  T_AT = "@";
  T_ASSIGN = "=";
  T_EQ = "==";
  T_NEQ = "!=";
  T_EOF = "$";
  T_LT = "<";
  T_LTE = "<=";
  T_GT = ">";
  T_GTE = ">=";
  T_SL = "<<";
  T_SR = ">>";
  T_AMP = "&";
  T_PIPE = "\\|";
  T_DOUBLE_QUOTE = "\"";
  T_OPEN_PAREN = "\\(";
  T_CLOSE_PAREN = "\\)";
  T_OPEN_BRACKET = "\\{";
  T_CLOSE_BRACKET = "\\}";
  T_OPEN_SBRACKET = "\\[";
  T_CLOSE_SBRACKET = "\\]";
  T_STAR = "\\*";
  T_CARET = "\\^";
  T_PLUS = "\\+";
  T_MINUS = "\\-";
  T_INCR = "\\+=";
  T_DECR = "\\-=";
  T_NOT = "\\!";
  T_BIT_NOT = "\\~";
  T_DOT = "\\.";
}

start = compiler_line:c
        meta_section:meta
        requirements_section:req
        code_section:code
        T_EOF =>  [:module, c, meta, req, code]
      ;

compiler_line = T_MEME module_path:c => c
               ;

module_path = T_ID:x T_COLON:c path:p => x + c + p;

path = T_PRIV_ID:x path_rest:xs => x + xs
     ;

path_rest = T_SLASH:x path:y => x + y
          | => ""
          ;

meta_section = meta_variables:x => [:meta, x];

meta_variables = meta_variable:x meta_variables:xs => [x] + xs
             | => []
             ;

meta_variable = T_AT_ID:name T_COLON {this.scanner.match_regexp("[^;]+")}:val T_SEMICOLON => [name.from(1), val]
              ;


requirements_section = T_REQUIRES module_params:params requirements_rest(params):x => x
                     | => [:requirements, [], [:default-locations, []], [:imports, []]]
                     ;

requirements_rest :params = T_WHERE module_defaults:specs module_imports:imp T_END
                            => [:requirements, params, [:default-locations, specs], [:imports, imp]]
                          | => [:requirements, params, [:default-locations, []], [:imports, []]]
                          ;


module_params = T_ID:x module_params_rest:xs => [x] + xs
              ;

module_params_rest = T_COMMA T_ID:x module_params_rest:xs => [x] + xs
                  | => []
                  ;

module_defaults = T_ID:name T_ASSIGN module_path:p module_defaults:xs => [[name, p]] + xs
                | => []
                ;

module_imports = module_import:x module_imports:xs => [x] + xs
               | => []
               ;

module_import = T_IMPORT T_ID:name T_FROM T_ID:lib => [name, lib];


code_section = top_level_decls:x => [:code, x]
             ;

top_level_decls = top_level_decl:x top_level_decls:xs => [x] + xs
                | => []
                ;

top_level_decl = obj_decl:x => x
               | class_decl:x => x
               | top_level_function:x => x
               ;

top_level_function = T_FUN_ID:x T_COLON top_level_fun_rest(x):y => y;

top_level_fun_rest :name = top_level_fun(name):x => x
                         | top_level_fn(name):x => x
                         ;

obj_decl = T_OBJECT T_ID:name object_slots:s obj_fun:f T_END => [:object, name, s, f];


obj_fun = T_FUNCTIONS T_OPEN_CURLY obj_fun_block:f T_CLOSE_CURLY => f
        | => []
        ;

obj_fun_block = constructor:x obj_fun_block_more:xs => [x] + xs
              | top_level_function:x obj_fun_block_more:xs => [x] + xs
              ;

obj_fun_block_more = obj_fun_block:xs => xs
                   | => []
                   ;

object_slots = ?T_FUNCTIONS => []
             | object_slot:x object_slots_more:xs => [x] + xs
             ;

object_slots_more = object_slots:x => x
                  | => []
                  ;

object_slot = T_ID:name T_COLON obj_slot_value:value T_SEMICOLON => [:slot, name, value];

obj_slot_value = literal:x => x
               | T_ID:x => x
               ;

class_decl = T_CLASS T_ID:name super_class:parent
                fields_:f
                constructors:c
                instance_method_decls:im
                class_method_decls:cm
              T_END => [:class, [name, parent], f, c, im, cm];

super_class = T_LT T_ID:x => x
            | => "Object"
            ;

fields_ = T_FIELDS T_COLON idlist:xs T_SEMICOLON => [:fields, xs]
      | => [:fields, []];

idlist = T_ID:x idlist_more:xs => [x] + xs;

idlist_more = T_COMMA idlist:xs => xs
            | => []
            ;

constructors = constructor:x constructors_more:xs => [:ctors, [x] + xs]
             | => [:ctors, []]
             ;

constructors_more = constructor:x constructors_more:xs => [x] + xs
                  | => []
                  ;

constructor =  T_INIT T_FUN_ID:name T_COLON
               # T_FUN fparams:p T_OPEN_CURLY
               {@has_fun_literal = false}
               top_fun_body:body
               end_function:e
             => #[:ctor, name, [:params, p],
                  @has_fun_literal, [:body,  body + [e]]];

instance_method_decls = instance_method_decl:x instance_method_decls:xs => [x] + xs
                      | => []
                      ;

instance_method_decl = T_INSTANCE_METHOD T_FUN_ID:name T_COLON
                       # T_FUN fparams:p T_OPEN_CURLY
                       {@has_fun_literal = false}
                       top_fun_body:body
                       end_function:e
                     => #[:fun, name, [:params, p], @has_fun_literal,
                       [:body, body + [e]]]
                     ;

class_method_decls = class_method_decl:x class_method_decls:xs => [x] + xs
                   | => []
                   ;

class_method_decl = T_CLASS_METHOD T_FUN_ID:name T_COLON
                       # T_FUN fparams:p T_OPEN_CURLY
                       {@has_fun_literal = false}
                       top_fun_body:body
                       end_function:e
                     => #[:fun, name, [:params, p], @has_fun_literal,
                       [:body, body + [e]]]
                     ;


top_level_fn :name = # data_literal:e T_SEMICOLON
                   => #[:fun, name, [:params, []], false, #[:body, #[[:return, e]]]];

top_level_fun :name =  # T_FUN
                       fparams:p T_OPEN_CURLY
                       {@has_fun_literal = false}
                       top_fun_body:body
                       end_function:e
                      => #[:fun, name, [:params, p], @has_fun_literal,
                           [:body, body + [e]]]
                      ;

end_function = # T_CLOSE_CURLY => #[:end-body];

fparams = T_OPEN_PAREN fparam_list:x T_CLOSE_PAREN => x;

fparam_list = T_ID:x fparam_list_rest:xs => [x] + xs
            |  T_STAR T_ID:x => [:var-arg, x]
            | => []
            ;

fparam_list_rest = T_COMMA fparam_list:x => x
                 | => []
                 ;

top_fun_body = primitive_body:p => p
             | stmts:s => s
             ;

primitive_body =  T_LT T_PRIMITIVE lit_string:s T_GT => [[:primitive, s]];

stmts = stmt:x stmts:xs => [x] + xs
    | => []
    ;

stmt = control_expr:e => e
     | non_control_expr:e T_SEMICOLON => e
    ;

control_expr = expr_if:x => x
             | expr_while:x => x
             | expr_try:x => x
             | expr_for:x => x
             ;
expr_if = # T_IF T_OPEN_PAREN expr:e T_CLOSE_PAREN T_OPEN_BRACKET
           stmts:body
          T_CLOSE_BRACKET
          expr_elifs:elif_
          expr_else:else_
         => #[:if, e, body, elif_, else_ or []]
         ;

expr_elifs =  expr_elif:x expr_elifs:xs => [x] + xs
          | => []
          ;

expr_elif = # T_ELIF T_OPEN_PAREN  expr:e T_CLOSE_PAREN T_OPEN_BRACKET
             stmts:body
             T_CLOSE_BRACKET
           => #[:elif, e, body];

expr_else = T_ELSE T_OPEN_BRACKET stmts:body T_CLOSE_BRACKET => body
          | => null
          ;

expr_while = # T_WHILE T_OPEN_PAREN expr:e T_CLOSE_PAREN T_OPEN_BRACKET
             stmts:xs
             T_CLOSE_BRACKET
           => #[:while, e, xs]
           ;

expr_try = # T_TRY T_OPEN_BRACKET
             stmts:s_try
           T_CLOSE_BRACKET catch_part:c T_OPEN_BRACKET
             stmts:s_catch
           T_CLOSE_BRACKET
          => #[:try, s_try, c, s_catch]
          ;

catch_part =  # T_CATCH T_OPEN_PAREN catch_expr(_pos):x T_CLOSE_PAREN => x;

catch_expr :_pos = T_ID:a catch_expr_rest(a, _pos):b => b;

catch_expr_rest :a :_pos = T_ID:b => #[:catch, [:id, a], b]
                         | => #[:catch, a]
                         ;

expr_for =  T_FOR T_OPEN_PAREN for_init:v T_SEMICOLON expr:c T_SEMICOLON
               expr_or_assignment:n T_CLOSE_PAREN T_OPEN_BRACKET
               stmts:st
             T_CLOSE_BRACKET
           => [:for, v, c, st, n]
         ;

for_init = expr_decl:x => x
         | for_expr_assign:x => x
         ;

//TODO: prevent "foo() = a" code
expr_or_assignment = # expr:a maybe_assignment(a, _pos):b => b;
maybe_assignment :a :_pos = T_ASSIGN  expr:b => #[:=, a, b]
                          | T_INCR expr:b => #[:=, a, [:+, a, b]]
                          | T_DECR expr:b => #[:=, a, [:-, a, b]]
                          | => [:expression, a]
                          ;

for_expr_assign = # expr:a for_expr_assign_rest(a, _pos):b => b;
for_expr_assign_rest :a :_pos = T_ASSIGN  expr:b => #[:=, a, b]
                               ;


expr_decl = # T_VAR T_ID:name T_ASSIGN expr:e => #[:var-def, name, e];

non_control_expr = expr_ret:x => x
                 | expr_non_local_ret:x => x
                 | expr_decl:x => x
                 | expr_or_assignment:x => x
                 ;


expr_ret = # T_RETURN expr:e => #[:return, e]
         ;

expr_non_local_ret =  # T_CARET  expr:e => #[:non-local-return, e]
                   ;

expr = expr_or:e => e;

expr_or = # expr_and:a expr_or_(a, _pos):b => b;
expr_or_ :a :_pos = T_OR expr_and:b expr_or_(#[:or, a, b], _pos):c => c
                  | => a
                  ;

expr_and = # expr_eq:a expr_and_(a, _pos):b => b;
expr_and_ :a :_pos = T_AND expr_eq:b expr_and_(#[:and, a, b], _pos):c => c
                   | => a
                   ;

expr_eq = # expr_rel:a expr_eq_(a, _pos):b => b;
expr_eq_ :a :_pos = T_EQ expr_rel:b expr_eq_(#[:==, a, b], _pos):c => c
                  | T_NEQ expr_rel:b expr_eq_(#[:!=, a, b], _pos):c => c
                  | => a
                  ;

expr_rel = # expr_add:a expr_rel_(a, _pos):b => b;
expr_rel_ :a :_pos = T_LTE expr_add:b expr_rel_(#[:<=, a, b], _pos):c => c
                   | T_GTE expr_add:b expr_rel_(#[:>=, a, b], _pos):c => c
                   | T_LT  expr_add:b expr_rel_(#[:<, a, b], _pos):c => c
                   | T_GT  expr_add:b expr_rel_(#[:>, a, b], _pos):c => c
                   | => a
                   ;

expr_add = # expr_mul:a expr_add_(a, _pos):b => b;
expr_add_ :a :_pos = ?T_INCR => a
                  | ?T_DECR => a
                  | T_PLUS expr_mul:b expr_add_(#[:+, a, b], _pos):c => c
                  | T_MINUS expr_mul:b expr_add_(#[:-, a, b], _pos):c => c
                  | T_AMP expr_mul:b expr_add_(#[:&, a, b], _pos):c => c
                  | T_PIPE expr_mul:b expr_add_(#[:|, a, b], _pos):c => c
                  | T_SL expr_mul:b expr_add_(#[:<<, a, b], _pos):c => c
                  | T_SR expr_mul:b expr_add_(#[:>>, a, b], _pos):c => c
                  | => a
                  ;

expr_mul = # expr_unary:a expr_mul_(a, _pos):b => b;
expr_mul_ :a :_pos = T_STAR expr_unary:b expr_mul_(#[:*, a, b], _pos):c => c
                   | T_SLASH expr_unary:b expr_mul_(#[:/, a, b], _pos):c => c
                   | => a
                   ;

expr_unary = # T_PLUS suffix_expr:a => #[:positive, a]
           | # T_MINUS suffix_expr:a => #[:negative, a]
           | # T_NOT suffix_expr:a => #[:not, a]
           | # T_BIT_NOT suffix_expr:a => #[:bit-neg, a]
           | super_expr:a => a
           ;


super_expr = # T_SUPER super_call_rest(_pos):x => x
            | suffix_expr:x => x
            ;

super_call_rest :_pos = T_DOT T_ID:sel args:p => #[:super-ctor-send, sel, [:args, p]]
                      | args:p => #[:super-send, [:args, p]]
                      ;


suffix_expr = # prim_expr:r suffix_expr_(r, _pos):e => e;

suffix_expr_ :r :_pos = send_expr(r, _pos):x suffix_expr_(x, _pos):y => y
                      | index_expr(r, _pos):x suffix_expr_(x, _pos):y => y
                      | call_expr(r, _pos):x suffix_expr_(x, _pos):y => y
                      | => r
                      ;


index_expr :r :_pos = T_OPEN_SBRACKET expr:i T_CLOSE_SBRACKET => #[:index, r, i];

send_expr :r :_pos = T_DOT T_ID:sel maybe_args:p  => #[:send, r, sel, [:args, p]];

call_expr :r :_pos = args:p  => #[:call, r, [:args, p]];

maybe_args = args:x => x
          | => []
          ;

args = T_OPEN_PAREN expr_list:p T_CLOSE_PAREN => p;


prim_expr = T_OPEN_PAREN expr:e T_CLOSE_PAREN => e
          | literal:x => x
          | # field_name:x => #[:field, x.from(1)]
          | # T_ID:x maybe_call(x, _pos):y => y
          ;

maybe_call :r :_pos = args:p => #[:send-or-local-call, r, [:args, p]]
                    | => #[:id, r]
                    ;


field_name = T_AT_ID:i => i;

expr_list = expr:x expr_list_rest:xs => [x] + xs
          | => []
          ;

expr_list_rest = T_COMMA expr_list:x => x
               | => []
               ;

pair_list = pair:x pair_list_rest:xs => [x] + xs
          | => []
          ;

pair_list_rest = T_COMMA pair_list:x => x
               | => []
               ;

pair = # expr:key T_COLON expr:val => #[:pair, key, val];



data_literal = lit_number:x => x
             | # T_THIS_MODULE => #[:literal, :module]
             | # T_THIS_CONTEXT => #[:literal, :context]
             | # T_SYMBOL:x => #[:literal-symbol, x.from(1)]
             | # T_THIS => #[:literal, :this]
             | # T_NULL => #[:literal, :null]
             | # T_TRUE => #[:literal, :true]
             | # T_FALSE => #[:literal, :false]
             | lit_string:x => x
             | # T_OPEN_SBRACKET expr_list:e T_CLOSE_SBRACKET => #([:literal-array].extends([e]))
             | # T_OPEN_CURLY pair_list:e T_CLOSE_CURLY    => #([:literal-dict]).extends(e);


literal = data_literal:x => x
        | funliteral:x {@has_fun_literal = true} => x;

funliteral = # T_FUN params:p T_OPEN_CURLY
               funliteral_body:body
             end_function:e
           => #[:fun-literal, [:params, p], [:body, body + [e]]];

maybe_expr = expr:x => x
           | => null
           ;

params = T_OPEN_PAREN idlist_opt:xs T_CLOSE_PAREN => xs;

idlist_opt = T_ID:x idlist_more:xs => [x] + xs
           | => []
           ;

idlist_more_opt = T_COMMA T_ID:x idlist_more_opt:xs => [x] + xs
                | => []
                ;


funliteral_body = funliteral_stmts:body
                   {this.last_or_empty(body)}:last
                   {this.rewrite_last_stmt(last)}
                => body;

funliteral_stmts = control_expr:x funliteral_stmts:xs => [x] + xs
                 | non_control_expr:x funliteral_stmt_sep_more:xs => [x] + xs
                 | => []
                 ;

funliteral_stmt_sep_more = T_SEMICOLON funliteral_stmts:x => x
                         | => []
                         ;


lit_string = # T_DOUBLE_QUOTE {this.consume_rest_lit_string()}:x
           => #[:literal-string, x]
           ;

lit_number = # T_HEXNUM:x => #[:literal-number, x.asHex]
           | # T_NUMBER:x => #[:literal-number, x.toInteger]
           ;

</kaiser>
instance_method consume_rest_lit_string: fun() {
  var res = "";
  while (true) {
    var x = this.scanner.match_regexp("[[:graph:]]|[ \t]");
    if (x == "\"") {
      return res.unescape;
    } elif (x == "\\") {
      var y = this.scanner.match_regexp("[[:graph:]]|[\t ]");
      res = res + x + y;
    } elif (x == null) {
      ParseError.throw(this.current_line, "string literals can't contain newline character");
    } elif (x == "\n") {
      ParseError.throw(this.current_line, "string literals can't contain newline character");
    } else {
      res = res + x;
    }
  }
}
instance_method last_or_empty: fun(lst) {
  if (lst.size > 0) {
    return lst.last;
  } else {
    return [];
  }
}
instance_method rewrite_last_stmt: fun(last) {
  if (last) {
    if(last.size > 0) {
      if (last[0] == :expression) {
        return last.set(0, :return);
      }
    }
  }
  return last;
}
instance_method initialize: fun() {
  @has_fun_literal = false;
  @line_offset = 0;
}
instance_method set_line_offset: fun(line_offset) {
  @line_offset = line_offset;
}
instance_method line_offset: fun() {
  return @line_offset;
}
end
