module foo() {

  fun x(f) {
    return f(9);
  }

  fun main() {
    var z = fun(k) { k + 1 };
    assert(x(z) == 10, "testing closure parameters");
  }
}
