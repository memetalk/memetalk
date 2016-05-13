.preamble(io)
  io: meme:io;
.code

class OMetaException < Exception

end


class OMetaStreamEnd
instance_method head: fun() {
  OMetaException.throw("ometa error");
}
instance_method tail: fun() {
  OMetaException.throw("ometa error");
}
end

class OMetaStream
fields: memo, idx, hd, tl, data;
init with_data: fun(data) {
  @memo = {};
  @idx = 0;
  @data = data;
  @hd = data[@idx];
  @tl = null;
}
init with_head_and_tail: fun(hd, tl) {
  @memo = {};
  @idx = tl.idx;
  @data = tl.data;
  @hd = hd;
  @tl = tl;
}
init from_idx_and_data: fun(idx, data) {
  @memo = {};
  @idx = idx;
  @data = data;
  @hd = data[@idx];
  @tl = null;
}
instance_method remaining: fun() {
  return "[" + @data.from(@idx) + "] pos: " + @idx.toString;
}
instance_method data: fun() {
  return @data;
}

instance_method idx: fun() {
  return @idx;
}

instance_method head: fun() {
  return @hd;
}

instance_method tail: fun() {
 if (!@tl) {
   if (@idx < @data.size) {
     @tl = OMetaStream.from_idx_and_data(@idx + 1, @data);
   } else {
     @tl = OMetaStreamEnd.new();
   }
  }
 return @tl;
}

instance_method memo: fun() {
  return @memo;
}

instance_method toString: fun() {
  return  "{" + @data.from(@idx).toString + " pos: " + @idx.toString + "}";
}
end


class OMetaBase
fields: indent, input, error_pos, error_rules;
init new: fun(input) {
  @indent = "";
  @input = input;
  @error_pos = 0;
  @error_rules = [];
}

instance_method set_error: fun(rule, input) {
  if (input.idx > @error_pos) {
    @error_pos = input.idx;
    @error_rules.append(rule);
    // io.print("+set error: " + rule.toString + " " + input.idx.toString);
  } else {
    if (input.idx == @error_pos) {
       @error_rules.append(rule);
       // io.print("+add error: " + rule.toString + " " + input.idx.toString);
     } else {
       // io.print("-skip error: " + rule.toString + " " + input.idx.toString);
     }
  }
}

instance_method input: fun() {
  return @input;
}

instance_method get_error: fun() {
  return [@error_pos, @error_rules];
}
instance_method _apply: fun(rule) {
  // @indent = @indent + " ";
  var memo = @input.memo;
  if (!memo.has(rule)) {
    var input = @input;
    try {
      // io.print(@indent + "%try " + rule.toString + " on [" + input.toString + "]");
      var res = this.send(rule, []);
      // io.print(@indent + "!matched " + rule.toString + " on [" + input.toString + "]");
      input.memo[rule] = {:input : @input, :ans : res};
    } catch(OMetaException e) {
      // io.print(@indent + "FAIL " + rule.toString + " on [" + input.toString + "]");
      // @indent = @indent.from(1);
      @input = input;
      input.memo[rule] = {:error : e};
      this.set_error(rule.toString, input);
      e.throw();
    }
  } else {
    if (memo[rule].has(:error)) {
      // @indent = @indent.from(1);
      memo[rule][:error].throw();
    }
  }
  // @indent = @indent.from(1);
  @input = memo[rule][:input];
  return memo[rule][:ans];
}
instance_method _apply_with_args: fun(rule, args) {
  args.reverse.each(fun(arg) {
    this.prepend_input(arg);
  });
  try {
    return this._apply(rule);
  } catch(OMetaException e) {
      this.set_error(rule.toString + "(" + args.toString + ")", @input);
      e.throw();
  }
}

instance_method prepend_input: fun(value) {
  @input = OMetaStream.with_head_and_tail(value, @input);
}



//*************************** basic functions

instance_method _pred: fun(x) {
// fails if not x
  if (x) {
     return true;
  } else {
    OMetaException.throw("ometa error");
  }
}

instance_method _not: fun(fn) {
// returns true if fn() fails. Fails otherwise
  var input = @input;
  try {
    fn();
  } catch(OMetaException e) {
    @input = input;
    return true;
  }
  OMetaException.throw("ometa error");
}

instance_method _lookahead: fun(fn) {
// runs fn(), restores input when success, returns fn's result
  var input = @input;
  var res = fn();
  @input = input;
  return res;
}

instance_method _or: fun(fns) {
// return result of the first fn() success or fails if no success.
  var input = @input;
  fns.each(fun (fn) {
    try {
      ^ fn();
    } catch(OMetaException e) {
       @input = input;
    }
  });
  OMetaException.throw("ometa error");
}

instance_method _opt: fun(fn) {
//if fn() fails, restore state, else return fn() result
  var input = @input;
  try {
    return fn();
  } catch(OMetaException e) {
    @input = input;
    return null;
  }
}

instance_method _many: fun(fn, first) {
  var res = null;
  if (first) {
    res = [first];
  } else {
    res = [];
  }
  while (true) {
    var input = @input;
    try {
      res.append(fn());
    } catch(OMetaException e) {
      @input = input;
      return res;
    }
  }
}

instance_method _many1: fun(fn) {
  return this._many(fn, fn());
}

instance_method _repeat: fun(n, fn) {
  var res = [];
  n.times(fun(i) {
    var input = @input;
    try {
      res.append(fn());
    } catch(OMetaException e) {
      @input = input;
      e.throw();
    }
  });
  return res;
}

instance_method _form: fun(fn) {
//very confused about this one
  var r = this._apply(:anything);
  this.pred([String, List].contains(Mirror.vtFor(r)));
  var input = @input;
  @input = @input.branch();
  var res = fn();
  this._apply(:end);
  @input = input;
  return r;
}


//*************************** basic rules

instance_method anything: fun() {
  var head = @input.head();
  @input = @input.tail();
  return head;
}

instance_method end: fun() {
  return this._not(fun() { this._apply(:anything) });
}

instance_method apply: fun() {
  var r = this._apply(:anything);
  return this._apply(r);
}

instance_method exactly: fun() {
  var wanted = this._apply(:anything);
  if (wanted == this._apply(:anything)) {
     return wanted;
  } else {
    OMetaException.throw("ometa error");
  }
}

instance_method until: fun() {
  var brk = this._apply(:anything);
  return this._many1(fun() { this._not(fun() { this._apply_with_args(:exactly, [brk]) });
                     return this._apply(:anything); });
}

// instance_method true: fun() {
//   var r = this.apply(:anything);
//   this._pred(r == true);
//   return r;
// }

// instance_method false: fun() {
//   var r = this.apply(:anything);
//   this._pred(r == false);
//   return r;
// }

instance_method number: fun() {
  var r = this._apply(:anything);
  this._pred(Mirror.vtFor(r) == Number);
  return r;
}

instance_method string: fun() {
  var r = this._apply(:anything);
  this._pred(Mirror.vtFor(r) == String);
  return r;
}

instance_method char: fun() {
  var r = this._apply(:anything);
  this._pred(Mirror.vtFor(r) == String and r.size() == 1);
  return r;
}

instance_method space: fun() {
  var r = this._apply(:char);
  this._pred(r.onlySpaces());
  return r;
}

instance_method spaces: fun() {
  return this._many(fun() { this._apply(:space) }, null);
}

instance_method digit: fun() {
  var r = this._apply(:char);
  this._pred(r.onlyDigits());
  return r;
}

instance_method str_digits: fun() {
  var ds = this._many1(fun() { this._apply(:digit) });
  return ds.join("");
}

instance_method lower: fun() {
  var r = this._apply(:char);
  this._pred(r.isLower());
  return r;
}

instance_method upper: fun() {
  var r = this._apply(:char);
  this._pred(r.isUpper());
  return r;
}

instance_method letter: fun() {
  return this._or([fun() { this._apply(:lower) },
                   fun() { this._apply(:upper) }]);
}

instance_method letter_or_digit: fun() {
  return this._or([fun() { this._apply(:letter) },
                   fun() { this._apply(:digit) }]);
}

instance_method first_and_rest: fun() {
  var first = this._apply(:anything);
  var rest = this._apply(:anything);
  return this._many(fun() { this._apply(rest) }, this._apply(first));
}

instance_method seq: fun() {
  var xs = this._apply(:anything);
  xs.each(fun(x) {
    this._apply_with_args(:exactly, [x]);
  });
  return xs;
}

// instance_method seq_s: fun() {
//   var res = this._apply(:seq);
//   this._apply(:spaces);
//   return res;
// }

instance_method token: fun() {
  var tk = this._apply(:anything);
  this._apply(:spaces);
  return this._apply_with_args(:seq, [tk.split("")]);
}

instance_method identifier_first: fun() {
  return this._or([fun() { this._apply_with_args(:exactly, ["_"]) },
                   fun() { this._apply(:letter) }]);
}

instance_method identifier_rest: fun() {
  return this._or([fun() { this._apply_with_args(:exactly, ["_"]) },
                   fun() { this._apply(:letter_or_digit) }]);
}

instance_method identifier: fun() {
  this._apply(:spaces);
  var ident = this._apply_with_args(:first_and_rest, [:identifier_first, :identifier_rest]);
  return ident.join("");
}


// (defmethod str-eq ((o ometa-base))
//   (let* ((wanted (core-apply o 'anything))
//          (found  (core-apply o 'anything)))
//     (core-pred o (and (stringp wanted) (stringp found)))
//     (core-pred o (string= wanted found))
//     found))

end


/// OMeta


class OMeta < OMetaBase
fields: current_production, local_vars;
init new: fun(input) {
  super.new(input);
  @local_vars = [];
}
instance_method add_local_var: fun(name) {
  @local_vars.append(name);
}

// rules...
instance_method ometa: fun() {
  this._apply_with_args(:keyword, ["ometa"]);

  var name = this._apply(:identifier);
  var i = this._apply(:inheritance);

  this._apply_with_args(:token, ["{"]);

  var r = this._apply(:rules);

  this._apply_with_args(:token, ["}"]);
  return [:grammar, name, i, r];
}

instance_method keyword: fun() {
  var xs = this._apply(:anything);
  var ident = this._apply_with_args(:token, [xs]);
  this._not(fun() { this._apply(:identifier_rest) });
  return xs;
}

instance_method inheritance: fun() {
  return this._or([fun() {
                     this._apply_with_args(:token, ["<"]);
                     var ident = this._apply(:identifier);
                     return [:parent, ident]; },
                   fun() { return [:parent, "OMetaBase"]; }]);
}

instance_method rules: fun() {
  return this._many1(fun() { this._apply(:rule) });
}

instance_method rule: fun() {
  var rname = this._lookahead(fun() { this._apply(:rule_name) });
  var p = this._many1(fun() { return this._apply_with_args(:rule_part, [rname]); });
  return [:rule, rname, [:or] + p];
}

instance_method rule_name: fun() {
  return this._apply(:identifier);
}

instance_method rule_part: fun() {
  var rn = this._apply(:anything);
  var rname = this._apply(:rule_name);
  this._pred(rname == rn);
  @current_production = rname;
  var r = this._apply(:rule_rest);
  this._apply_with_args(:token, [";"]);
  return r;
}

instance_method rule_rest: fun() {
  var args = null;
  var c = null;
  var ac = null;
  return this._or([
    fun() { this._apply(:action) },
    fun() { this._apply_with_args(:token, ["="]);
            return this._apply(:choices);},
    fun() { args = this._many1(fun() { this._apply(:argument); });
            this._apply_with_args(:token, ["="]);
            c = this._apply(:choices);
            return [:and] + args + [c];},
    fun() { args = this._many1(fun() { this._apply(:argument); });
            ac = this._apply(:action);
            return [:and] + args + [ac];}]);
}

instance_method argument: fun() {
  return this._or([fun() { return this._apply(:bind_expression); },
                   fun() { var b = this._apply(:binding);
                           return [:bind, b] + [:apply, :anything];}]);
}

instance_method choices: fun() {
  var x = this._apply(:choice);
  var xs = this._many(fun() { this._apply_with_args(:token, ["|"]);
                              return this._apply(:choice);}, null);
  return [:or, x] + xs;
}


instance_method choice: fun() {
  var x = null;
  return this._or([fun() { x = this._many(fun() { this._apply(:top_expression) }, null);
                           var ac = this._apply(:action);
                           return [:and] + x + [ac];},
                   fun() { x = this._many(fun() { this._apply(:top_expression) }, null);
                           return [:and] + [x];}]);
}

instance_method top_expression: fun() {
  return this._or([fun() { this._apply(:bind_expression) },
                  fun() { this._apply(:repeated_expression) }]);
}

instance_method bind_expression: fun() {
  var e = this._apply(:repeated_expression);
  var b = this._apply(:binding);
  return [:bind, b, e];
}

instance_method repeated_expression: fun() {
  var e = null;
  return this._or([
    fun() { e = this._apply(:term);
            this._apply_with_args(:token, ["*"]);
            return [:many, e];},
    fun() { e = this._apply(:term);
            this._apply_with_args(:token, ["+"]);
            return [:many1, e];},
    fun() { e = this._apply(:term);
            this._apply_with_args(:token, ["?"]);
            return [:optional, e];},
    fun() { return this._apply(:term); }]);
}

instance_method term: fun() {
  var e = null;
  return this._or([fun() { this._apply_with_args(:token, ["~"]);
                           e = this._apply(:element);
                           return  [:not, e];},
                   fun() { this._apply_with_args(:token, ["&"]);
                           e = this._apply(:element);
                           return [:lookahead, e];},
                   fun() { return this._apply(:element); }]);
}

instance_method binding: fun() {
  this._apply_with_args(:token, [":"]);
  var i = this._apply(:identifier);
  this.add_local_var(i);
  return i;
}

instance_method element: fun() {
  var s = null;
  var c = null;
  return this._or([fun() { this._apply(:prod_app) },
                   fun() { this._apply(:data_element) },
                   fun() { this._apply_with_args(:token, ["%("]);
                           s = this._apply_with_args(:until, [")"]).join("");
                           this._apply_with_args(:exactly, [")"]);
                           return [:sem_pred, s];},
                   fun() { this._apply_with_args(:token, ["{"]);
                           c = this._apply(:choices);
                           this._apply_with_args(:token, ["}"]);
                           return c; }]);
}

instance_method data_element: fun() {
  return this._or([fun() { this._apply(:char_sequence) },
                   fun() { this._apply(:token_string) },
                   fun() { this._apply(:string_object) },
                   fun() { this._apply(:asymbol) },
                   fun() { this._apply(:s_expr) },
                   fun() { this._apply_with_args(:token, ["_"]) },
                   fun() { this._apply_with_args(:token, ["$"]) }]);
}

instance_method action: fun() {
  this._apply_with_args(:token, ["=>"]);
  var s = this._apply_with_args(:until, [";"]).join("");
  return [:action, s];
}

instance_method prod_app: fun() {
  var p = null;
  var args = null;
  return this._or([fun() { this._apply_with_args(:token, ["<"]);
                           p = this._apply(:identifier);
                           this._apply_with_args(:token, [">"]);
                           return [:apply, p];},
                   fun() { p = this._apply(:identifier);
                           return [:apply, p];},
                   fun() { this._apply_with_args(:token, ["<"]);
                           p = this._apply(:identifier);
                           args = this._apply(:prod_arg_list);
                           this._apply_with_args(:token, [">"]);
                           return [:apply_with_args, [:arguments] + args, p];},
                   fun() { this._apply_with_args(:token, ["^"]);
                           return [:apply_super, @current_production];}]);
}

instance_method prod_arg_list: fun() {
  var x = this._apply(:prod_arg);
  var xs = this._many(fun() { this._apply_with_args(:token, [","]);
                              return this._apply(:prod_arg);}, null);
  return [x] + xs;
}

instance_method prod_arg: fun() {
  return this._or([fun() { this._apply(:data_element); },
                   fun() { this._apply(:identifier); }]);
}

instance_method char_sequence: fun() {
  this._apply(:spaces);
  this._apply_with_args(:exactly, ["'"]);
  var cs = this._many1(fun() {
        return this._or([fun() {
                            this._not(fun() { this._apply_with_args(:exactly, ["'"]) });
                            this._apply_with_args(:exactly, ["\\"]);
                            var c = this._apply(:char);
                            return "\\" + c; },
                         fun() {
                            this._not(fun() { this._apply_with_args(:exactly, ["'"]) });
                            return this._apply(:char); }]);
  });
  this._apply_with_args(:exactly, ["'"]);
  return [:seq, cs];
}

instance_method token_string: fun() {
  this._apply(:spaces);
  this._apply_with_args(:exactly, ["\""]);
  var cs = this._many1(fun() {
        return this._or([fun() {
                            this._not(fun() { this._apply_with_args(:exactly, ["\""]) });
                            this._apply_with_args(:exactly, ["\\"]);
                            var c = this._apply(:char);
                            return "\\" + c; },
                         fun() {
                            this._not(fun() { this._apply_with_args(:exactly, ["\""]) });
                            return this._apply(:char); }]);
  });
  this._apply_with_args(:exactly, ["\""]);
  return [:token_string, cs];
}

instance_method string_object: fun() {
  this._apply(:spaces);
  this._apply_with_args(:exactly, ["`"]);
  var cs = this._many1(fun() {
        return this._or([fun() {
                            this._apply_with_args(:exactly, ["\\"]);
                            this._apply(:char) },
                         fun() {
                            this._not(fun() { this._apply_with_args(:exactly, ["\\"]) });
                            return this._apply(:char); }]);
  }, null);
  this._apply_with_args(:exactly, ["`"]);
  return [:token_string, cs];
}

instance_method asymbol: fun() {
  this._apply_with_args(:token, [":"]);
  var s = this._apply(:identifier);
  return [:symbol, s];
}

instance_method s_expr: fun() {
  this._apply_with_args(:token, ["["]);
  var s = this._apply(:choice);
  this._apply_with_args(:token, ["]"]);
  return [:form, s];
}



end //end OMeta

parse: fun(data, cls, rule) {
  var parser = cls.new(OMetaStream.with_data(data));
  parser.prepend_input(rule);
  try {
    return [null, parser.apply()];
  } catch(OMetaException e) {
    return [parser.get_error, null];
  }
}

.endcode
