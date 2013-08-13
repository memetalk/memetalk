module foo() {

  main: fun() {
    var cmod = get_compiled_module(thisModule);
    var env = {"a": 1};
    var cfun = CompiledFunction.new("bar","fun(x) { a = a + x; }", [], cmod);
    var fn = cfun.asContext(thisModule, env);
    var f = fn.apply([]);
    var ret = f(20);
    var env_new = fn.getEnv();
    assert(env_new['a'] == 40, "Context.getEnv");
  }
}
