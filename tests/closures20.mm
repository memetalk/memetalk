module foo(io) 
 io: memetalk/io/1.0();
{
  foo: fun() {
    var cfn = CompiledFunction.newClosure("fun(a) { a }", thisContext.compiledFunction());
    var fn = cfn.asContextWithVars(thisModule, {});
  }

  main: fun() {
    foo(); //screwing with foo's env
    foo(); //check if it's all ok
  }
}
