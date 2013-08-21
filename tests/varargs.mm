.preamble(io)
  io: memetalk/io/1.0();
.code

main: fun() {
  foo(1);
  bar(1,2);
  baz(1,2,3);
}

foo: fun(x, *rest) {
  assert(x == 1 and rest == [], "foo(x = 1, rest = [])");
}

bar: fun(x, *rest) {
  assert(x == 1 and rest == [2], "foo(x = 1, rest = [2])");
}

baz: fun(x, *rest) {
  assert(x == 1 and rest == [2,3], "foo(x = 1, rest = [2,3])");
}

.endcode
