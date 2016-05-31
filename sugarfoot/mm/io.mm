.preamble()
.code

read_file: fun(path) {
  <primitive "io_read_file">
}

write_file: fun(path, text) {
  <primitive "io_write_file">
}

print: fun(arg) {
  <primitive "io_print">
}

.endcode
