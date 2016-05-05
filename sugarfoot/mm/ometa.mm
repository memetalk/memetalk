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

class OMetaStream < List
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
  return "{" + @hd.toString + " " + @data.from(@idx).toString + " -- " + @idx.toString + "}";
}
end


class OMetaBase
fields: input;
init new: fun(input) {
  @input = input;
}

instance_method _apply: fun(rule) {
  var memo = @input.memo;
  if (!memo.has(rule)) {
    var input = @input;
    try {
      var res = this.send(rule, []);
      input.memo[rule] = {"input": @input, "ans": res};
    } catch(OMetaException e) {
      @input = input;
      e.throw();
    }
  }
  @input = memo[rule]["input"];
  return memo[rule]["ans"];
}
instance_method _apply_with_args: fun(rule, args) {
  args.reverse.each(fun(arg) {
    this.prepend_input(arg);
  });
  return this._apply(rule);
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
    OMetaException.throw("ometa error");
  } catch(OMetaException e) {
    @input = input;
    return true;
  }
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
  return this.many(fn, fn());
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

instance_method letter_or_digit_or_: fun() {
  return this._or([fun() { this._apply(:letter_or_digit) },
                   fun() { this._apply_with_args(:exactly, ['_'])}]);
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
  this._apply(:spaces);
  return this._apply_with_args(:first_and_rest, [:letter, :letter_or_digit_or_]);
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

// rules...
instance_method ometa: fun() {
  this._apply(:spaces);

  this._apply_with_args(:seq, [["o", "m", "e", "t", "a"]]);

  var name = this._apply(:token);
  var i = this._apply(:inheritance);

  this._apply_with_args(:exactly, ["{"]);
  var r = this._apply(:rules);

  this._apply_with_args(:exactly, ["}"]);

  this._apply(:end);
  return ["grammar", name, i, r];
}

instance_method inheritance: fun() {
  this._apply(:spaces);
  return this._or([fun() {
                     this._apply_with_args(:exactly, ["<"]);
                     return this._apply(:token);},
                   fun() { return ["parent", "OMetaBase"]; }]);
}

instance_method rules: fun() {
  this._apply(:spaces);
  return ["rules"];
}

end //end OMeta

parse: fun(data, cls, rule) {
  var parser = cls.new(OMetaStream.with_data(data));
  parser.prepend_input(rule);
  return parser.apply();
}

main: fun() {
  io.print(parse("  ometa test {  }", OMeta, :ometa));
}

.endcode
