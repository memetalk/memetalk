.preamble(ometa,ometa_tr,ometa_base, io)
 ometa : meme:ometa;
 ometa_tr : meme:ometa_tr;
 ometa_base : meme:ometa_base;
 io : meme:io;
.code


ometa_generate: fun() {
  var text = io.read_file("mm/ometa.g");
  var maybe_ast = ometa_base.parse(text, ometa.OMeta, :ometa);
  io.print(maybe_ast.toSource);
  if (maybe_ast[0]) {
    io.print("parse error: " + maybe_ast[0].toString);
  } else {
    io.print("\n\ntranslating...\n\n");
    maybe_ast = ometa_base.parse([maybe_ast[1]], ometa_tr.OMetaTranslator, :ometa);
    if (maybe_ast[0]) {
      io.print("translation error: " + maybe_ast[0].toString);
    } else {
      io.print(maybe_ast[1]);
    }
  }
}

ometatr_generate: fun() {
  var text = io.read_file("mm/ometa_tr.g");
  var maybe_ast = ometa_base.parse(text, ometa.OMeta, :ometa);
  io.print(maybe_ast.toSource);
  if (maybe_ast[0]) {
    io.print("parse error: " + maybe_ast[0].toString);
  } else {
    io.print("\n\ntranslating...\n\n");
    maybe_ast = ometa_base.parse([maybe_ast[1]], ometa_tr.OMetaTranslator, :ometa);
    if (maybe_ast[0]) {
      io.print("translation error: " + maybe_ast[0].toString);
    } else {
      io.print(maybe_ast[1]);
    }
  }
}


main: fun() {
  ometa_generate();
  ometatr_generate();
}

xmain: fun() {
  var ast = [:rule, "space", [:or, [:and, [:apply_super, "space"]]]];

  // var ast = [:grammar, "OMeta", [:parent, "OMetaBase"],
  //  [[:rule, "ometa", [:or,
  //     [:and, [:apply_with_args, [[:token_string, "ometa"]], :keyword],
  //            [:bind, "name", [:apply, :identifier]],
  //            [:bind, "i", [:apply, :inheritance]],
  //            [:token_string, "{"],
  //            [:bind, "code", [:apply, :inline]],
  //            [:bind, "r", [:apply, :rules]],
  //            [:token_string, "}"],
  //       [:action,  "[:grammar, name, i, r]"]]]]]];
  var maybe_res = ometa_base.parse([ast], ometa_tr.OMetaTranslator, :rule);
  if (maybe_res[0]) {
    io.print("ometa.g error: " + maybe_res[0].toString);
  } else {
    io.print(maybe_res[1]);
  }
}

.endcode
