module foo() {

  fun print(arg) {
    <primitive "print">
  }

  fun foo() {
    var z = 88;
    debug;
    print(z);
    z = z + 1;
    print(z);
    return 10;
  }

  fun main() {
    var x = 10;
    var y = 11;
    var k = foo();
    print(k);
    return k + 10;
  }
}
