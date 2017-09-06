.license
.endlicense
.preamble(io)
         io: meme:io;
.code

class NoFields
end

class NoFieldsInherit < NoFields
end

class JustFields
  fields: a, b;
end

class JustFieldsInherit < NoFields
  fields: a, b;
end

class NoFieldsJustConstructorInherited < NoFields
  init new: fun(a, b, c) {
  }
end

class NoFieldsJustConstructor
  init new: fun(a, b, c) {
  }
end

class FieldsAndConstructor
  fields: a, b;

  init new: fun() {
    @a = 1;
    var b = [
      1,
      2,
      3,
      4
    ];
    var c = {
      :foo: "bar",
      :bar: "baz"
    };
    var d = {};
  }
end

class FieldsAndConstructorAndInstanceMethod
  fields: a, b;

  init new: fun() {
    @a = 1;
  }

  instance_method foo: fun() {
    io.print(@a);
  }
end

class JustOneInstanceMethodWithAnIf
  instance_method toString: fun() {
    if (true) {
      return this.message.toString();
    } elif (false) {
      return this.message.toString() + "bar";
    } else {
      return this.message.toString() + "foo";
    }
    return 1;
  }
end

class JustOneInstanceMethodWithAnIfAndElif
  instance_method toString: fun() {
    if (true) {
      return this.message.toString();
    } elif (false) {
      return this.message.toString() + "foo";
    }
  }
end

class JustOneInstanceMethodWithAnIfAndElifAndElse
  instance_method toString: fun() {
    if (true) {
      return this.message.toString();
    } elif (false) {
      return this.message.toString() + "bar";
    } else {
      return this.message.toString() + "foo";
    }
  }
end

class VariousInstanceMethodsAndAnIfElseElifs
  instance_method method1: fun() {
    if (false) {
      FieldsAndConstructor.new();
    }
  }

  instance_method method2: fun() {
    return "foo";
  }

  instance_method method3: fun() {
    if (false) {
      FieldsAndConstructor.new();
    } else {
      return 1;
    }
  }

  instance_method method4: fun() {
    if (false) {
      FieldsAndConstructor.new();
    } elif (true) {
      return 1;
    }
  }

  instance_method method5: fun() {
    if (false) {
      FieldsAndConstructor.new();
    } elif (true) {
      return 1;
    } else {
      var a = 2;
      return 2;
    }
  }

  instance_method method6: fun() {
    return :blah;
  }
end

class InstanceMethodsWithWhile
  instance_method m1: fun() {
    while (this.m2()) {
      io.print("foo");
    }
  }

  instance_method m2: fun () {
    return false;
  }
end

class InstanceMethodWithTryCatch
  instance_method m1: fun() {
    try {
      foo();
    } catch (Exception e) {
      io.print(e);
    }
  }

  instance_method m2: fun() {
    this.m1();
  }
end

class InstanceMethodWithClosures
  instance_method m1: fun() {
    // style1
    this.m2(fun(r) {
      io.print(r);
    });

    // style2
    var f = fun(r) {
      io.print(r);
    };

    // style3
    this.m2(fun(r) {
      io.print(r);
    });

    // style4
    this.m3([fun(r) { io.print(r); }, fun(r) { io.print(r); }]);

    // style5
    this.m3([fun(r) { io.print(r); },
             fun(r) { io.print(r); }]);

    // style6
    this.m3([
      fun(r) { io.print(r); },
      fun(r) { io.print(r); }
    ]);

    // style7
    var as = this.m3([
      fun(r) { io.print(r); },
      fun(r) { io.print(r); }
    ]);
  }

  instance_method m2: fun(f) {
    f("hi");
  }

  instance_method m3: fun(l) {
    l.each(fun(f) { f("hi"); });
  }

  instance_method m4: fun() {
    var style1 = [fun() {}, fun () {}];

    var style2 = [fun() {},
                  fun() {}];

    var style3 = [
      fun() {},
      fun() {}
    ];
  }
end

class InstanceMethodWithListEach
  instance_method m1: fun() {
    var blah = [1, 2, 3, 4, 5, 6];
    blah.each(fun(i) {
      if (false) {
        io.print("fu");
      }
      ^ i;
    });
  }
end

object Foo
  a: 1;
  b: 2;
end

.end
