module foo() {

  bar: fun() {
    return 50;
  }

  main: fun() {
    var cmod = get_compiled_module(thisModule);

    var cfn = bar.compiledFunction();

    var x = bar();

    cfn.setCode("fun(a,b) { return a + b; }");

    assert(x + bar(2,3) == 55, "Changing module function");
  }
}
