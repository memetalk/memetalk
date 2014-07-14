.preamble()
.code

class Point2D
  fields: x,y;
  init other_new: fun() {
    @x = 300;
    @y = 400;
  }
  init new: fun(x,y) {
    @x = x;
    @y = y;
  }
  instance_method x: fun() {
    return @x;
  }
  instance_method y: fun() {
    return @y;
  }
end

class Point3D < Point2D
  fields: z;
  init new: fun(x,y,z) {
    super.new(x,y);
    //super.other_new();
    @z = z;
  }
  instance_method z: fun() {
    return @z;
  }
end

main: fun() {
  var p =  Point3D.new(99,98,97);
  return p.z();
}

.endcode
