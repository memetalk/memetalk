.preamble(ometa,io)
 ometa : meme:ometa;
 io : meme:io;
.code


main: fun() {
  var text = io.read_file("mm/ometa.g");
  io.print(ometa.parse(text, ometa.OMeta, :ometa));
}
.endcode
