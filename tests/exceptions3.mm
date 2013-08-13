module foo()
{
  main: fun() {
    var a = 0;
    try {
      Exception.throw(10);
    } catch(e) {
      a = a + 1;
    }
    a = a + 1;
    assert(a == 2, "Core Exception class");
  }
}
