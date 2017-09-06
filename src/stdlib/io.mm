meme foo

read_file: fun(filepath) {
  <primitive "io_read_file">
}

write_file: fun(filepath, text) {
  <primitive "io_write_file">
}

print: fun(arg) {
  <primitive "io_print">
}


open_file: fun(filepath, direction) {
  <primitive "io_open_file">
}

write: fun(fp, data) {
  <primitive "io_write">
}

close: fun(fp) {
  <primitive "io_close">
}

with_file: fun(filepath, fn) {
  var fp = open_file(filepath, :write);
  var writer = fun(data) { write(fp, data) };
  try {
    fn(writer);
  } catch(e) {
    close(fp);
    e.throw();
  }
  close(fp);
}
