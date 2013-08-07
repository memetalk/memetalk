module ex()
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
    return X.throw + X.new(2).x;
  }
}
