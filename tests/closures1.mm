/* passing parameters to closure and getting result */
module foo() {

  fun main() {
    var z = fun(k) { k + 1 };
    return z(10);
  }
}
