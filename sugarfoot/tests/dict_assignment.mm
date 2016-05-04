.license
.endlicense

.preamble(test)

.code

// -- module functions --

class X
fields: d;
init new: fun() {
  @d = {};
}
instance_method d: fun() {
  return @d;
}
end

main: fun() {
  var x = X.new;
  x.d["a"] = 1;
  test.assert(x.d.has("a"), "dictionary should have key 'a'");
  test.assert(x.d["a"] == 1, "dictionary at 'a' should be 1");
}

// -- module classes --




.end
