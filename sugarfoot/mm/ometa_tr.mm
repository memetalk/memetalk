.preamble(io, ometa_base)
  io: meme:io;
  ometa_base: meme:ometa_base;
  [OMetaBase] <= ometa_base;
.code

class OMetaTranslator < OMetaBase
instance_method ometa: fun() {
//  ometa = [:grammar _:name inheritance rules];
  return this._form(fun() {
     this._apply_with_args(:exactly, [:grammar]);
     var name = this._apply(:anything);
     var base = this._apply(:inheritance);
     var r = this._apply(:rules);
     return ["class ", name, " < ", base, "\n", r.join("\n"), "\nend"].join("");
  });
}

instance_method inheritance: fun() {
  return this._form(fun() {
    this._apply_with_args(:exactly, [:parent]);
    var base = this._apply(:anything);
    return base;
  });
}

instance_method rules: fun() {
  return this._form(fun() {
    return this._many1(fun() { return this._apply(:rule); });
  });
}

instance_method rule: fun() {
  return this._form(fun() {
    this._apply_with_args(:exactly, [:rule]);
    var name = this._apply(:anything);
    var args = this._apply(:rule_args);
    var cs = this._apply(:body);
    return ["instance_method ", name, ": fun() {\n", args, cs, "\n}"].join("");
  });
}

instance_method rule_args: fun() {
  return this._or([fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:args]);
      var args = this._many1(fun() {
        return this._apply(:anything);
      });
      return args.map(fun(name) { ["  var ", name, " = this._apply(:anything);"].join("") }).join("\n");
    });
  }, fun() {
    return "";
  }]);
}

instance_method body: fun() {
  var p = null;
  return this._or([fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:and]);
      p = this._many1(fun() {
        return this._apply(:body);
      });
      return p.join(";\n") + ";";
    });
  },
  fun() {
    return this._form(fun() {
      return this._apply_with_args(:exactly, [:and]);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:or]);
      p = this._many1(fun() {
        return this._apply(:body);
      });
      return ["  this._or([", p.map(fun(x) { ["fun() { ", x, "}"].join("") }).join(",\n"), "])"].join("") + ";";
    });
  },
  fun() {
    return this._apply(:pattern);
  }]);
}

instance_method pattern: fun() {
  return this._or([
    fun() {
      return this._form(fun() {
        this._apply_with_args(:exactly, [:bind]);
        var name = this._apply(:string);
        var e = this._apply(:body);
        return ["  var ", name, " = ", e].join("");
      });
    },
    fun() {
      return this._form(fun() {
        this._apply_with_args(:exactly, [:action]);
        return ["  return ", this._apply(:string)].join("");
      });
    },
    fun() {
      return this._apply(:expression);
    }
  ]);
}

instance_method prod_arg: fun() {
  return this._or([fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:seq]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:token_string]);
      return this._apply(:string).toSource();
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:string_object]);
      return this._apply(:string).toSource();
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:symbol]);
      return this._apply(:anything).toSource();
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:id]);
      return this._apply(:string);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:form]);
      return this._apply(:anything);
    });
  }]);
}

instance_method expression: fun() {
  return this._or([fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:apply]);
      var s = this._apply(:symbol);
      return ["  this._apply(", s.toSource, ")"].join("");
    });
  },
  fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:apply_with_args]);
      var args = this._form(fun() {
        return this._many1(fun() {
          var res = this._apply(:prod_arg);
          return res;
        });
      });
      var r = this._apply(:anything);
      return ["  this._apply_with_args(", r.toSource, ", [", args.join(","),"])"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:apply_super]);
      var sup = this._apply(:string);
      return [" this._apply_super(:", sup, ")"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:seq]);
      var str = this._apply(:string);
      return ["  this._apply_with_args(:seq, [", str.split("").map(fun(x) { x.toSource }).join(",") , "])"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:token_string]);
      var str = this._apply(:string);
      return ["  this._apply_with_args(:token, [", str.toSource, "])"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:many]);
      var b = this._apply(:body);
      return ["  this._many(fun() { ", b, "}, null)"].join(""); //TODO: many 2nd param
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:many1]);
      var b = this._apply(:body);
      return ["  this._many1(fun() { ", b, "})"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:not]);
      var e = this._apply(:expression);
      return ["  this._not(fun() { ", e, "})"].join("");
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:optional]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:form]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:symbol]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:string_object]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:sem_pred]);
      return this._apply(:anything);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:sem_action]);
      return this._apply(:string);
    });
  }, fun() {
    return this._form(fun() {
      this._apply_with_args(:exactly, [:lookahead]);
      return this._apply(:anything);
    });
  }]);
}

instance_method string: fun() {
  var str = this._apply(:anything);
  this._pred(Mirror.vtFor(str) == String);
  return str;
}

instance_method symbol: fun() {
  var sym = this._apply(:anything);
  this._pred(Mirror.vtFor(sym) == Symbol);
  return sym;
}

end //OMetaTranslator

.endcode
