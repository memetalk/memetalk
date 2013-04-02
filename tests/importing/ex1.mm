module foo() {

  class Foo {
    fields: a;
    init new(a) {
      @a = a;
    }
    fun a() {
      return @a;
    }
  }

  fun import(mname, margs) {
    <primitive "import">
  }

  fun main() {
    var bar = import("ex2.mm",
                     [Foo]);
    return bar.Bar.new(1,2).all() + bar.other();
  }
}
