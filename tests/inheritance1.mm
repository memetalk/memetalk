module foo() {

  class Point2D {
    fields: x,y;
    init new: fun(x,y) {
      @x = x;
      @y = y;
    }
    instance_method xy: fun() {
      return @x + @y;
    }
  }
  class Point3D < Point2D {
    fields: z;
    init new: fun(x,y,z) {
      super.new(x,y);
      @z = z;
    }
    instance_method all: fun() {
      return @z + this.xy();
    }
  }

  main: fun() {
    var x = Point3D.new(1,5,9);
    assert(x.all() == 15, "Simple inheritance");
  }
}
