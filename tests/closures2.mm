/* passing closure as parameter, passing parameter to closure
  and getting result */
module foo() {

  fun x(f) {
    return f(9);
  }

  fun main() {
    var z = fun(k) { k + 1 };
    return x(z);
  }
}
