/* accessing enclosed data from closures */
module foo() {

  fun x(f) {
    return f(9);
  }

  fun main() {
    var f = fun() { 99 };
    var z = fun(k) { f() + 1 + k };
    return x(z);
  }
}
