module foo() {

  class X {
    fields: bla;
    init new() {
      var f = fun() {
        @bla = 10;
      };
      f();
    }
    fun bla() {
      return @bla;
    }
  }

  fun main() {
    assert(X.new().bla() == 10, "Closure accessing instance field");
  }
}
