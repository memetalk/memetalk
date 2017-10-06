meme central:memescript/compiler
requires io, ometa_base
where
  io = central:stdlib/io
  ometa_base = central:memescript/ometa_base
  import OMetaBase from ometa_base
end

flatten: fun(lst) {
  var res = [];
  for (var i = 0; i < lst.size; i = i + 1) {
    var x = lst[i];
    if (Mirror.vtFor(x) == List) {
      res = res + flatten(x);
    } else {
      res.append(x);
    }
  }
  return res;
}

class KaiserTranslator < OMetaBase
fields: rules, terminals, keyword_terminals, id_terminal, pred_table, rec_table, local_vars, indent;
init new: fun(input) {
  super.new(input);
  @rules = {};
  @terminals = {};
  @keyword_terminals = [];
  @id_terminal = null;
  @pred_table = {};
  @rec_table = {};
  @local_vars = [];
  @indent = "  ";
}

instance_method set_rule: fun(name, ast) {
  @rules[name] = ast;
}
instance_method check_pred_ambiguity: fun(name, lst) {
  if (lst.size != lst.unique.size) {
    Exception.throw("rule '" + name + "' is ambiguous: " + lst.toSource);
  }
  return lst;
}
instance_method maybe_proc_more_terms: fun(name, x, asts) {
  //iterate over each term in a choice checking for :empty
  //to accumulate the preditive table for the rule `name`
  // io.print("maybe_proc_more_terms: " + name + " -- " + x.toSource);
  var res = [];
  var i = 0;
  if (x == [:skip]) {
    x = this._apply_with_args(:pred_expr, [asts[i]]);
    i += 1;
  }
  if (x.has(:empty)) {
    while (i < asts.size) {
      var xs = this._apply_with_args(:pred_expr, [asts[i]]);
      res += flatten(xs);
      if (!xs.has(:empty)) {
        // io.print("RETURN LOOPEND: " + name + " = " + res.toSource);
        return res;
      }
      i += 1;
    }
    // io.print("RETURN END:" + name + " = " + res.toSource);
    return res;
  } else {
    // io.print("RETURN LAST: " + name + " = " + x.toSource);
    return x;
  }
}

instance_method proc_pred_rule_body: fun(name) {
  if (@rec_table.has(name)) {
    if (@rec_table[name]) {
      Exception.throw("Recursive rules are not supported: " + n);
    }
  }
  @rec_table[name] = true;
  try {
    var res = this._apply_with_args(:pred_rule_body, [name, @rules[name]]);
  } catch(e) {
    @rec_table[name] = true;
    e.throw();
  }
  @rec_table[name] = false;
  return this.check_pred_ambiguity(name, res);
}
instance_method add_pred_entry: fun(name, x) {
  @pred_table[name] = x;
}
instance_method lookaheads_for: fun(rule, include_empty) {
  var filtered = null;
  if (include_empty) {
    filtered = @pred_table[rule];
  } else {
    filtered = @pred_table[rule].filter(fun(x) { x != :empty });
  }
  return filtered.map(fun(t) {
    if (t == :empty) {
      return t;
    } else {
      return this.get_terminal_info(t.toSymbol)[:id];
    }
  });
}
instance_method get_terminal_info: fun(k) {
  if (@terminals.has(k)) {
    return @terminals[k];
  } else {
    Exception.throw("Undeclared terminal: " + k.toString);
  }
}
instance_method add_terminal: fun(name, rx) {
  var id = @terminals.size;
  if (@terminals.has(name)) {
    Exception.throw("Duplicated terminal: " + name);
  }
  @terminals[name] = {:regexp: rx, :name: name, :id: id};
  return id;
}
instance_method add_keyword_terminal: fun(name, rx) {
  var id = this.add_terminal(name, rx + "\\b");
  @keyword_terminals.append(id);
}
instance_method set_id_terminal: fun(name ,rx) {
  var id = this.add_terminal(name, rx);
  if (@id_terminal != null) {
    Exception.throw("Duplicated id token definition");
  } else {
    @id_terminal = id;
  }
}
instance_method token_id: fun(name) {
  return this.get_terminal_info(name.toSymbol)[:id];
}
instance_method _add_local_var: fun(name) {
  if (!@local_vars.has(name)) {
    @local_vars.append(name);
  }
}
instance_method _local_vars: fun() {
  var locals = @local_vars.map(fun(name) { ["  var ", name, " = null;\n"].join("") });
  @local_vars = [];
  return locals;
}

instance_method code_for_choice: fun(first, rest) {
  if (first[:lookahead] == :skip) {
    return first[:text] + "\n" + this.code_for_choice(rest[0], rest.from(1));
  }
  if (first[:lookahead].size > 0) {
    return [@indent, "if(this.lookahead_any(",first[:lookahead].toSource,")) {\n",@indent.times(2), first[:text], "\n",
              rest.map(fun(x) { @indent.times(2) + x[:text]}).join("\n"), "\n", @indent.times(2), "return null;\n", @indent, "}\n"].join("");
  } else {
    return [@indent, first[:text], "\n",rest.map(fun(x) { @indent.times(2) + x[:text]}).join("\n")].join("");
  }
}
instance_method code_for_body: fun(name, first, rest) {
  var epilogue = [@indent, "this.unexpected(" + this.lookaheads_for(name, false).toSource + ");"].join("");
  return ([first] + rest + [epilogue]).join("\n");
}
instance_method basic_methods: fun() {
  var ctor = ["init new: fun(text) {",
              [@indent, "super.new(text, ", @terminals.toSource, ", ", (@keyword_terminals.last + 1).toSource, ",", @id_terminal.toSource,");"].join(""),
              @indent, "this.initialize();",
             "}\n"].join("\n");
  return ctor;
}
<ometa>

   //build terminal table and predictive table

   start = [:module _:b [token+] _:r _:e]:x store_rules(r) compute_pred_tables(r) translate(r):txt => b + txt + e;

   token = [:token_rule _:name [:token_string _:rx]] => this.add_terminal(name.toSymbol, rx)
         | [:key_rule _:name [:token_string _:rx]] => this.add_keyword_terminal(name.toSymbol, rx)
         | [:id_rule _:name [:token_string _:rx]]  => this.set_id_terminal(name.toSymbol, rx)
         ;

   store_rules = [[:rule _:name _ _:v !{this.set_rule(name, v)}]+];
   compute_pred_tables = [[:rule _:name _ _ !{this.proc_pred_rule_body(name)}:x !{this.add_pred_entry(name, x)} ]+];

   pred_rule_body :name = [:or pred_choice(name)+:xs] => flatten(xs)
                        | pred_choice(name):x => x
                        ;

   pred_choice :name = [pred_expr:x _*:asts !{this.maybe_proc_more_terms(name, x, asts)}:xs] => flatten(xs).unique
                     | [] => [:empty]
                     ;

   pred_expr = [:lookahead pred_expr:x]    => x
             | [:sem_action _] => []
             | [:bind _ pred_expr:x] => x
             | [:apply-with-args _ _:n] => this.proc_pred_rule_body(n)
             | [:apply _:n] => this.proc_pred_rule_body(n)
             | [:terminal _:n] => [n]
             | :action _ // rule => action
                 => [:empty]
             | [:action _] // rule = foo | => action
                 => []
             | [:position] => [:skip]
             | [:ns-position] => [:skip]
             | _:x => Exception.throw("pred_expr: UNEXPECTED " + x.toSource)
             ;



  translate = [rule+:xs] => ([this.basic_methods] + xs).join("\n");

  rule = [:rule _:name rule_params:p rule_body(name):b]
       => (["instance_method ", name, ": fun(", p.join(", "), ") {\n"] + this._local_vars() + [b, "\n}"]).join("");

  rule_params = [:args _*:args] => args
              ;

  rule_body :name = [:or choice:x choice*:xs] => this.code_for_body(name, x, xs)
                  | terms
                  ;

  choice = [expr:x expr*:xs] => this.code_for_choice(x, xs)
          | [] => [@indent, "return null;"].join("")
          ;

  terms = [expr+:p] => p.map(fun(x) { @indent + x[:text] }).join("\n");

  expr     = [:bind _:name expr:e] !{this._add_local_var(name)}
              => {:lookahead: e[:lookahead], :text: [name, " = ", e[:text].trim()].join("")}
            | [:sem_action _:x] => {:lookahead: [], :text: x}
            | [:terminal _:t] => {:lookahead: [this.get_terminal_info(t.toSymbol)[:id]], :text: ["this.match(",this.token_id(t).toSource,");"].join("")}
            | [:action _:t] => {:lookahead: [], :text: ["return ", t, ";"].join("")}
            | [:apply _:t] => {:lookahead: this.lookaheads_for(t, true), :text: ["this.", t, "();"].join("")}
            | [:apply-with-args [rule_arg+:a] _:t] => {:lookahead: this.lookaheads_for(t, true), :text: ["this.", t, "(",a.join(", "), ");"].join("")}
            | [:sem_action _:x] => {:lookahead: [], :text: x + ";"}
            | [:lookahead [:terminal _:t]] => {:lookahead: [this.get_terminal_info(t.toSymbol)[:id]], :text: ""}
            | [:position] !{this._add_local_var("_pos")} => {:lookahead: :skip, :text: "_pos = this.position_after_spaces();"}
            | [:ns-position] !{this._add_local_var("_pos")} => {:lookahead: :skip, :text: "_pos = this.current_position();"}
            | _:x => Exception.throw("expr:UNEXPECTED " + x.toSource)
            ;

  rule_arg = [:id _:x] => x | [:host-expr _:x] => x;
</ometa>

end
