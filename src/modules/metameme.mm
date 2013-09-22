.license
Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
.endlicense

.preamble(io)
  io : memetalk/io/1.0();

.code

// -- module functions --

main: fun() {
  //ometa { rulename = 10; }
  var ometa = Base.new("  ometa { haha = 10; }");
  ometa.apply_with_args("token", "ometa");
  ometa.apply_with_args("token", "{");
  //id
  var idseq = ometa.many1(fun() {
    ometa.apply("letter_or_digit");
  });
  ometa.apply("spaces");
  ometa.apply("token", "=");
  var num = ometa.many(fun() {
    ometa.apply("digit");
  });
}

// -- module classes --

class Base
fields: input;
init new: fun(data) {
  @input = Input.new(data);
}

instance_method apply: fun(rule_name) {
  //TODO: memo it
  return this.send(("rule_" + rule_name).toSymbol, []);
}

instance_method apply_with_args: fun(rule, *rest) {
  @input = @input.prepend(rest.reversed);
  this.apply(rule);
}

instance_method end: fun() {
  return this.not(fun() { this.apply("anything") });
}

instance_method form: fun(fn) {
  var seq = this.apply("anything");
  if (!is_sequence(seq)) {
    Fail.throw();
  }
  var input = @input;
  @input = Input.new(seq);
  var ret = fn();
  this.apply("end");
  @input = input;
  return ret;
}

instance_method lookahead: fun(fn) {
  var input = @input;
  var r = fn();
  @input = input;
  return r;
}

instance_method many: fun(fn, *opt) {
  var ret = opt;

  while (true) {
    var input = @input;
    try {
      ret = ret + [fn()];
    } catch(ex) {
      if (ex.type != Fail) {
        ex.throw();
      } else {
        @input = input;
        return ret;
      }
    }
  }
}

instance_method many1: fun(fn) {
  return this.many(fn, fn());
}

instance_method not: fun(fn) {
  var input = @input;
  try {
    fn();
  } catch(e) {
    @input = input;
    return true;
  }
  Fail.throw();
}

instance_method opt: fun(fn) {
  var input = @input;
  var ret = null;
  try {
    ret = fn();
  } catch(ex) {
    if (ex.type == Fail) {
      @input = input;
    }
  }
  return ret;
}

instance_method or: fun(fns) {
  var input = @input;
  fns.each(fun(fn) {
    try { //cl-ometa checks if fn == null, don't remember why
      @input = input;
      ^ fn();
    } catch(ex) {
      if (ex.type == Exception) { //not an ometa match-fail
        ex.throw();
      }
    }
  });
}

instance_method pred: fun(x) {
  if (x) {
    return true;
  }
  Fail.throw();
}

instance_method rule_anything: fun() {
  var r = @input.first;
  @input = @input.rest;
  return r;
}

instance_method rule_apply: fun() {
  var rule = this.apply("anything");
  return this.apply(rule);
}

instance_method rule_char: fun() {
  var r = this.apply("anything");
  this.pred(Mirror.vtFor(r) == String and r.size() == 1);
  return r;
}

instance_method rule_digit: fun() {
  var r = this.apply("char");
  this.pred(r >= "0" and r <= "9");
  return r;
}

instance_method rule_exactly: fun() {
  var wanted = this.apply("anything");
  if (wanted == this.apply("anything")) {
    return wanted;
  } else {
    Fail.throw;
  }
}

instance_method rule_false: fun() {
  var r = this.apply("anything");
  this.pred(r == false);
  return r;
}

instance_method rule_first_and_rest: fun() {
  var first = this.apply("anything");
  var rest = this.apply("anything");
  return this.many(fun() { this.apply(rest) }, this.apply(first));
}

instance_method rule_letter: fun() {
  return this.or([fun() {
    this.apply("lower");
  }, fun() {
    this.apply("upper");
  }]);
}

instance_method rule_letter_or_digit: fun() {
  return this.or([fun() {
    this.apply("letter");
  }, fun() {
    this.apply("digit");
  }]);
}

instance_method rule_lower: fun() {
  var r = this.apply("char");
  this.pred(r.charCode >= "a".charCode and r.charCode <= "z".charCode);
  return r;
}

instance_method rule_number: fun() {
  var r = this.apply("anything");
  this.pred(Mirror.vtFor(r) == Number);
  return r;
}

instance_method rule_seq: fun() {
  var xs = this.apply("anything");
  xs.each(fun(x) {
    this.apply_with_args("exactly", x);
  });
  return xs;
}

instance_method rule_space: fun() {
  var r = this.apply("char");
  this.pred(r.charCode <= 32);
  return r;
}

instance_method rule_spaces: fun() {
  return this.many(fun() { this.apply("space") });
}

instance_method rule_string: fun() {
  var r = this.apply("anything");
  this.pred(Mirror.vtFor(r) == String);
  return r;
}

instance_method rule_token: fun() { // dummy/stand-in code. I'm hungry, be back latter
  var tk = this.apply("anything");
  this.apply("spaces");
  var found = this.apply_with_args("first_and_rest", "letter_or_digit", "letter");
  this.pred(tk == found);
  return tk;
}

instance_method rule_true: fun() {
  var r = this.apply("anything");
  this.pred(r == true);
  return r;
}

instance_method rule_upper: fun() {
  var r = this.apply("char");
  this.pred(r >= "A" and r <= "Z");
  return r;
}

end //metameme:Base

class Fail < Exception
fields: ;
init new: fun(msg) { super.new(msg); }

class_method throw: fun() {
  this.new("Meta-Meme error").throw;
}

end //metameme:Fail

class Input < List
fields: ;
init new: fun(data) {
  super.new(data);
}

instance_method prepend: fun(other) {
  return Input.new(super(other));
}

end //metameme:Input


.end
