module foo() {

  fun main() {
    var x = 10;
    var y = 11;
    if (1 == 10) {
      x = 50;
      return x;
    } else {
      x = 2;
    }
    return 99;
  }
}
