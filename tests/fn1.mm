module foo()
{
  a: 1;

  main: fun() {
    assert(a() == 1, "testing fn");
  }
}
