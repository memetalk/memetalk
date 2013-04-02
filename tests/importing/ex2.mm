module bar(x) {
  class Bar < x {
    fields: b;
    init new(a,b) {
      super.new(a);
      @b = b;
    }
    fun all() {
      return this.a + @b;
    }
  }

  fun other() {
    return x.new(9).a;
  }
}
