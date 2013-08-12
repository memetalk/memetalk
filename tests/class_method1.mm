module foo()
{
  class X {
    fields: x;
    init new(x) {
      @x = x;
    }
    func throw() {
      return 10;
    }
    fun x() {
      return @x;
    }
  }

  fun main() {
    assert(X.throw + X.new(2).x == 12, "Testing class method");
  }
}
