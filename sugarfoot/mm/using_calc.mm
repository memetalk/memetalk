.preamble(io, calc, ometa_base)
  io : meme:io;
  ometa_base : meme: ometa_base;
  calc : meme: calc;
.code

main: fun() {
  var res = ometa_base.parse("1+3*3", calc.Calculator, :expr);
  io.print(res.toSource); //*[null, 10]
}

.endcode
