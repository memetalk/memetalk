.preamble(io, memetest)
 io : meme:io;
 memetest : meme:memetest;
 [Test] <= memetest;
.code

WSIZE: 8;

class FormatException < Exception
class_method check: fun(pred, message) {
  if (!pred) {
    this.throw(message);
  }
}
end

unpack: fun(str) {
   FormatException.check(str.size == 8, "unpack requires string of length 8");
   var res = 0;
   str.each(fun(idx, chr) {
     res = res + (chr.toByte << (idx * 8));
   });
   return res;
}

pack: fun(num) {
  var res = "";
  [0,1,2,3,4,5,6,7].each(fun(_, i) {
    var n = (num >> (i * 8)) & 0xFF;
    res = res + n.asChar;
  });
  return res;
}

pack_byte: fun(num) {
    return num.asChar;
}

untag: fun(num) {
  var flag = (4611686018427387903 * 2) + 1; //0x7FFFFFFFFFFFFFFF
  return num & flag;
}

tag: fun(num) {
  var flag = (-4611686018427387903 - 1) * 2; //0x8000000000000000
  return num | flag;
}

bytelist: fun(num) {
  return pack(num).map(fun(x) { x.toByte });
}


bytelist_tag: fun(num) {
  return pack(tag(num)).map(fun(x) { x.toByte });
}

string_block_size: fun(string) {
  //number of bytes required for string, aligned to word size
  return (string.size / WSIZE).ceil * WSIZE;
}


// tests

main: fun() {
  var t = Test.new;
  t.assertEqual(unpack("\x08\x07\x06\x05\x04\x03\x02\x01"), 0x102030405060708, "unpack");

  var s = pack(0x102030405060708);
  t.assertEqual(s, "\x08\x07\x06\x05\x04\x03\x02\x01", "pack");

  t.assertEqual(untag(tag(10)), 10, "tagging");

  t.assertEqual(bytelist(1010), [242, 3, 0, 0, 0, 0, 0, 0], "bytelist");
  t.assertEqual(bytelist_tag(1010), [242, 3, 0, 0, 0, 0, 0, 128], "bytelist_tag");
  t.assertEqual(string_block_size   ("1"), WSIZE, "string_block_size 1");
  t.assertEqual(string_block_size   ("123456789"), WSIZE * 2, "string_block_size 1");
}

.endcode
