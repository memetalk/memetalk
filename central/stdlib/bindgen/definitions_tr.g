meme central:memescript/compiler

requires ometa_base, types
where
  types      = central:stdlib/bindgen/types
  ometa_base = central:memescript/ometa_base
import OMetaBase from ometa_base
import Fun from types
import Param from types
import Struct from types

class SyscallDefinitionsTranslator < OMetaBase
  fields: funs;

  init new: fun(input) {
    super.new(input);
    @funs = [];
  }
  instance_method newFun: fun(name) {
     var f = Fun.new(name);
     @funs.append(f);
     return f;
  }
  instance_method genMainFunction: fun(moduleName) {
    var output = ["void init_primitives(VM *vm) {"];
    @funs.each(fun(_, d) {
      var prim_name = moduleName + "_" + d.getName();
      output.append("  vm->register_primitive(\"" + prim_name + "\", prim_" + d.getName() + ");");
    });
    output.append("}");
    return output.join("\n");
  }
  instance_method gen: fun(moduleName, definitions) {
    var output = [definitions.map(fun(i) { i.toString }).join("\n\n"), ""];
    output.append(this.genMainFunction(moduleName));
    return output.join("\n");
  }

<ometa>

start = definitions;

module :m = definitions:xs => this.gen(m, xs);

definitions = [definition+:xs] => xs;

definition = include | struct_def | func;

include
    = [:include string:name] => "#include " + name
    ;
struct_def
    = [:struct string:n !{Struct.new}:s !{s.setName(n)} params(s)] => s
    | [:struct string:n !{Struct.new}:s !{s.setName(n)}] => s
    ;
func
    = [:func string:n !{this.newFun(n)}:f
             params(f) !{f.newReturnType()}:r
             type(r):rt] => f
    ;
params :obj
    = [typed(obj)*:x]
    ;
typed :obj
    = [!{obj.newChild()}:p type(p) string:n !{p.setName(n)}] => p
    ;
type :p
    = [:list type(p) !{p.setIsArray(true)}]
    | [:annotations type(p) _:a !{p.setAnnotations(a)}]
    | [:pointer type(p) _:c !{p.setIsPointer(c)}]
    | [:const type(p) !{p.setIsConst(true)}]
    | [:unsigned type(p) !{p.setIsUnsigned(true)}]
    | type0(p)
    ;
type0 :p
    = [:builtin string:t !{p.setTypeName(t)}]
    | [:struct string:t !{p.setTypeName("struct " + t)}]
    ;

</ometa>

end
