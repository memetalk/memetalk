.preamble(ometa,ometa_tr,ometa_base, io)
 ometa : meme:ometa;
 ometa_tr : meme:ometa_tr;
 ometa_base : meme:ometa_base;
 io : meme:io;
.code

gen: fun(grammar_in_file_path, grammar_out_file_path, OMeta, OMetaTranslator) {
  io.print("=== processing " + grammar_in_file_path);
  var text = io.read_file(grammar_in_file_path);
  var maybe_ast = ometa_base.parse(text, OMeta, :mm_module);
  //io.print(maybe_ast.toSource);
  if (maybe_ast[0]) {
    io.print("parse error: " + maybe_ast[0].toString);
  } else {
    io.print("\n\ntranslating...\n\n");
    maybe_ast = ometa_base.parse([maybe_ast[1]], OMetaTranslator, :mm_module);
    if (maybe_ast[0]) {
      io.print("translation error: " + maybe_ast[0].toString);
    } else {
      io.write_file(grammar_out_file_path, maybe_ast[1]);
      io.print("Wrote to " + grammar_out_file_path);
    }
  }
}

main: fun() {
  gen("mm/ometa.g", "mm/ometa1.mm", ometa.OMeta, ometa_tr.OMetaTranslator);
  gen("mm/ometa_tr.g", "mm/ometa_tr1.mm", ometa.OMeta, ometa_tr.OMetaTranslator);

  var om1 = null;
  var omtr1 = null;
  if (compile_module("ometa1")) {
    om1 = instantiate_module("ometa1");
  }
  if (compile_module("ometa_tr1")) {
    omtr1 = instantiate_module("ometa_tr1");
  }

  gen("mm/ometa.g", "mm/ometa2.mm", om1.OMeta, omtr1.OMetaTranslator);
  gen("mm/ometa_tr.g", "mm/ometa_tr2.mm", om1.OMeta, omtr1.OMetaTranslator);
}

.endcode
