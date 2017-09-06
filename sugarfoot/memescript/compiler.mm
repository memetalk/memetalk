meme foo

requires bits, comp_vmemory, io, entries, ometa_base, meme, meme_tr
where
  bits         = central:memescript/bits
  comp_vmemory = central:memescript/comp_vmemory
  io           = central:stdlib/io
  entries      = central:memescript/entries
  ometa_base   = central:memescript/ometa_base
  meme         = central:memescript/meme
  meme_tr      = central:memescript/meme_tr
import OMetaException from ometa_base


MAGIC_NUMBER: 0x420;

class MMC
fields: cmodule, HEADER_SIZE;
init new: fun(cmodule) {
  @cmodule = cmodule;
}
instance_method name_ptr_for: fun(name, mmc) {
  var acc = 0;
  mmc[:names].each(fun(_, pair) {
    var entry_name_t = pair[0];
    var bsize = pair[1];
    if (entry_name_t == name + "\0") {
      ^ @HEADER_SIZE + acc;
    }
    acc = acc + bsize;
  });
  Exception.throw("entry " + name.toString + " not found in NAMES");
}
instance_method create_mmc_struct: fun(vmem) {
  var mmc = {:header: {:magic_number: null,
                      :ot_size: null,
                      :er_size: null,
                      :st_size: null,
                      :names_size: null},
             :names: [],
             :object_table: [],        //the loaded objects of this module
             :external_references: [], //references to core objects
             :symbol_table: [],        //Symbol objects needed
             :reloc_table: []          //addresses within the OT that need relocation on load
    };

  @HEADER_SIZE = mmc[:header].size * bits.WSIZE;
  @cmodule.fill(vmem);

  mmc[:header][:magic_number] = MAGIC_NUMBER;

  var names_list = vmem.external_names.map(fun(n) { n + "\0" }).unique;

  mmc[:names] = names_list.map(fun(name_t) { [name_t, bits.string_block_size(name_t)] });

  mmc[:header][:names_size] = mmc[:names].map(fun(x) { x[1] }).sum;

  var base = @HEADER_SIZE + mmc[:header][:names_size];
  vmem.set_base(base);

  mmc[:object_table] = vmem.object_table();

  mmc[:header][:ot_size] = mmc[:object_table].size;

  vmem.external_references().each(fun(_,pair) {
    mmc[:external_references].append(this.name_ptr_for(pair[0], mmc));
    mmc[:external_references].append(pair[1]);
  });

  mmc[:header][:er_size] = mmc[:external_references].size * bits.WSIZE;

  vmem.symbols_references.each(fun(_, p2) {
    mmc[:symbol_table].append(this.name_ptr_for(p2[0], mmc));
    mmc[:symbol_table].append(p2[1]);
  });

  mmc[:header][:st_size] = mmc[:symbol_table].size * bits.WSIZE;

  mmc[:reloc_table] = vmem.reloc_table();

  return mmc;
}
instance_method dump: fun(filepath) {
  var vmem = comp_vmemory.CompVirtualMemory.new();
  var mmc = this.create_mmc_struct(vmem);

  io.with_file(filepath.substr(0, filepath.size - 2) + "mmc", fun(write) {
    //header
    write(bits.pack(mmc[:header][:magic_number]));
    write(bits.pack(mmc[:header][:ot_size]));
    write(bits.pack(mmc[:header][:er_size]));
    write(bits.pack(mmc[:header][:st_size]));
    write(bits.pack(mmc[:header][:names_size]));

    //names
    mmc[:names].each(fun(_, entry) {
      var name = entry[0];
      var chunk_size = entry[1];
      var text = name + "\0".times(chunk_size - name.size);
      write(text);
    });

    //object table
    mmc[:object_table].each(fun(_, v) {
      write(bits.pack_byte(v));
    });

    //external references
    mmc[:external_references].each(fun(_, v) {
      write(bits.pack(v));
    });

    //symbols
    mmc[:symbol_table].each(fun(_, v) {
      write(bits.pack(v));
    });

    //reloc table
    mmc[:reloc_table].each(fun(_, word) {
      write(bits.pack(word));
    });
  });
}
end


class Compiler
fields: cmodule, filepath;
instance_method new_module: fun() {
  var module_name = basename(@filepath); //provisory function to get the basename
  @cmodule = entries.CompiledModule.new(module_name);
  return @cmodule;
}
instance_method parse: fun() {
  var ast = ometa_base.parse(io.read_file(@filepath), meme.MemeScriptParser, :start);
  if (ast[0]) {
    io.print(ast[0]);
    exit(1);
  } else {
    return ast[1];
  }
}
instance_method translate: fun(ast) {
  var input = ometa_base.OMetaStream.with_data([ast]);
  var parser = meme_tr.MemeScriptTranslator.new(this, input);
  parser.prepend_input(:start);
  try {
    parser.apply();
  } catch(e) {
    io.print(input.format_error);
    exit(1);
  }
}
instance_method compile: fun(filepath) {
  @filepath = filepath;
  var ast = this.parse();

  io.print(ast.toSource);
  io.print("translating...");

  this.translate(ast);

  var mmc = MMC.new(@cmodule);
  mmc.dump(filepath);
}
end

main: fun() {
  var args = argv();
  if (args.size < 2) {
    io.print("usage: compiler <path to file.mm>");
  } else {
    argv().from(1).each(fun(_, path) {
      io.print(path);
      Compiler.new.compile(path);
   });
  }
}
