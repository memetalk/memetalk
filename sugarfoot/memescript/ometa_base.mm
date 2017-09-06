meme foo

requires io
where
  io = central:stdlib/io

class OMetaException
instance_method throw: fun() {
  <primitive "exception_throw">
}
class_method throw: fun() {
  this.new.throw;
}
end

class OMetaLeftRecursion
fields: used;
init new: fun() {
  @used = false;
}
instance_method set_used: fun() {
  @used = true;
}
instance_method used: fun() {
  return @used;
}
instance_method throw: fun() {
  <primitive "exception_throw">
}
class_method throw: fun() {
  this.new.throw;
}
end


class OMetaStream
fields: memo, idx, hd, tl, data;
init new: fun(memo, idx, data, hd, tl) {
  @memo = memo;
  @idx = idx;
  @data = data;
  @hd = hd;
  @tl = tl;
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
  @hd = null;
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

instance_method equals: fun(other) {
  return @idx == other.idx and @data == other.data;
}

instance_method head: fun() {
  if (!@hd) {
    @hd = @data[@idx];
  }
  return @hd;
}
instance_method last_parsed_token: fun() {
  var st = this;
  var prev = this;
  var line = 0;
  var col = 0;
  var idx = 0;
  while (idx < @data.size) {
    if (@data[idx] == "\n") {
      line = line + 1;
      col = 0;
    } else {
      col = col + 1;
    }
    idx = idx + 1;
    if (st.memo.size > 0) {
      prev = st;

      st = st.tail;

      if (Mirror.vtFor(st) == OMetaStreamEnd) {
        return {:line : line, :col : col, :token: prev};
      }
    } else {
      return {:line : line, :col : col, :token: prev};
    }
  }
}
instance_method format_error: fun() {
  var last = this.last_parsed_token();
  var line = last[:line];
  var col = last[:col];
  if (Mirror.vtFor(@data) == String) {
//    var lines = @data.split("\n");
    return ["parse error: ", (line + 1).toString, ":", col.toString, "\n"].join("");
            // lines[line],"\n",
            // " ".times(col),"^\n",
            // "expected: ",
            // last[:token].memo.keys().map(fun(x) { x.toString }).join(", ")].join("");
  } else {
    return ["parse error: ", "\n"].join("");
    // return ["error: '", last.head.toString, "'\n",
    //         "expected: ",
    //         last.memo.keys().map(fun(x) { x.toString }).join(", ")].join("");
  }
}

instance_method tail: fun() {
 if (!@tl) {
   var nidx = @idx + 1;
   if (nidx < @data.size) {
     @tl = OMetaStream.from_idx_and_data(nidx, @data);
   } else {
     @tl = OMetaStreamEnd.from_idx_and_data(nidx, @data);
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
class_method with_data: fun(data) {
  if (data.size == 0) {
    return OMetaStreamEnd.from_idx_and_data(0, data);
  } else {
    return OMetaStream.new({}, 0, data, null, null);
  }
}
end


class OMetaStreamEnd < OMetaStream
fields: memo;
init from_idx_and_data: fun(idx, data) {
  super.from_idx_and_data(idx, data);
}
instance_method head: fun() {
  OMetaException.throw();
}
instance_method tail: fun() {
  OMetaException.throw();
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
  } else {
    if (input.idx == @error_pos) {
       @error_rules.append(rule);
     }
  }
}

instance_method input: fun() {
  return @input;
}
instance_method set_input: fun(input) {
  @input = input;
}

instance_method get_error: fun() {
  return [@error_pos, @error_rules];
}
instance_method _apply: fun(rule) {
  var memo = @input.memo;
  if (!memo.has(rule)) {
    var input = @input;
    var lr = OMetaLeftRecursion.new;
    memo[rule] = {:fail : lr};
    var res = this.send(rule, []);
    var zz = @input;
    //debug(); //io.print(@input)
    memo[rule] = {:input : @input, :ans : res, :fail : null};
    if (lr.used) {
      var sentinel = @input;
      var loop = true;
      while (loop) {
        try {
          @input = input;
          res = this.send(rule, []);
          var xx = @input;
          if (@input.equals(sentinel)) {
             //io.print(@input == sentinel);
             OMetaException.throw;
          }
          memo[rule] = {:input : @input, :ans : res, :fail : null};
        } catch(OMetaException e) {
          loop = false; // break is not implemented yet :/
        }
      }
    }
  } elif (memo[rule][:fail]) {
    lr = memo[rule][:fail]   ;
    lr.set_used();
    OMetaException.throw();
  }
  @input = memo[rule][:input];
  return memo[rule][:ans];
}
instance_method _apply_with_args: fun(rule, args) {
  args.reverse.each(fun(_, arg) {
    this.prepend_input(arg);
  });
  try {
    return this._apply(rule);
  } catch(OMetaException e) {
      this.set_error(rule.toString + "(" + args.toString + ")", @input);
      e.throw();
  }
}

instance_method _apply_super: fun(rule) {
  //non-memoized
  return this.super_send(rule, []);
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
    OMetaException.throw();
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
  OMetaException.throw();
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
  fns.each(fun (_, fn) {
    try {
      ^ fn();
    } catch(OMetaException e) {
       @input = input;
    }
  });
  OMetaException.throw();
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
  var klass = Mirror.vtFor(r);
  this._pred(Mirror.isSubclass(klass, List) or Mirror.isSubclass(klass, List)); //enumerable?sequenceable?
  var input = @input;
  @input = OMetaStream.with_data(r);
  var res = fn(); //ignore the last element in the list.
  this._apply(:end);
  @input = input;
  return r;
}

// *************************** basic rules

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
    OMetaException.throw();
  }
}


instance_method number: fun() {
  var r = this._apply(:anything);
  var vt = Mirror.vtFor(r);
  this._pred(vt == Integer or vt == LongNum);
  return r;
}

instance_method string: fun() {
  var r = this._apply(:anything);
  this._pred(Mirror.vtFor(r) == String);
  return r;
}

instance_method symbol: fun() {
  var r = this._apply(:anything);
  this._pred(Mirror.vtFor(r) == Symbol);
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
  return this._many(fun() { this._apply(:space) }, null).join("");
}

instance_method digit: fun() {
  var r = this._apply(:char);
  this._pred(r.onlyDigits());
  return r;
}

instance_method digits: fun() {
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
  xs.each(fun(_, x) {
    this._apply_with_args(:exactly, [x]);
  });
  return xs;
}

instance_method token: fun() {
  var tk = this._apply(:anything);
  this._apply(:spaces);
  return this._apply_with_args(:seq, [tk]);
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

instance_method keyword: fun() {
  var xs = this._apply(:anything);
  this._apply_with_args(:token, [xs]);
  this._not(fun() {
    this._apply(:identifier_rest);});
  return xs;
}



instance_method until: fun() {
  var brks = this._apply(:anything);
  var r = null;
  var acc = [];
  while (true) {
    r = this._lookahead(fun() { this._apply(:anything) });
    if (brks.find(r) >= 0) {
      return acc;
    }
    acc.append(r);
    this._apply(:anything);
  }
}

end

parse: fun(data, cls, rule) {
  var input = OMetaStream.with_data(data);
  var parser = cls.new(input);
  parser.prepend_input(rule);
  try {
    return [null, parser.apply()];
  } catch(OMetaException e) {
    return [input.format_error, null];
  }
}

parse_with_args: fun(data, cls, rule, args) {
  var input = OMetaStream.with_data(data);
  var parser = cls.new(input);
  try {
    return [null, parser._apply_with_args(rule, args)];
  } catch(OMetaException e) {
    return [input.format_error, null];
  }
}
