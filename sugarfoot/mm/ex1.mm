.preamble(ometa,io)
 ometa : meme:ometa;
 io : meme:io;
.code


main: fun() {
  var text = io.read_file("mm/meme.g");
  io.print(ometa.parse(text, ometa.OMeta, :ometa));
}
.endcode
