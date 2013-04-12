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
    return X.new().bla();
  }
}
