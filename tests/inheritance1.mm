module foo() {

  class Point2D {
    fields: x,y;
    init new(x,y) {
      @x = x;
      @y = y;
    }
    fun xy() {
      return @x + @y;
    }
  }
  class Point3D < Point2D {
    fields: z;
    init new(x,y,z) {
      super.new(x,y);
      @z = z;
    }
    fun all() {
      return @z + this.xy();
    }
  }

  fun main() {
    var x = Point3D.new(1,5,9);
    assert(x.all() == 15, "Simple inheritance");
  }
}
