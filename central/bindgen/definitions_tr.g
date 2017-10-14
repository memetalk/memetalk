meme central:memescript/0.1/compiler

requires ometa_base, types
where
  types      = central:bindgen/0.1/types
  ometa_base = central:memescript/0.1/ometa_base
  import OMetaBase from ometa_base
  import Fun from types
  import Param from types
  import Struct from types
  import Typedef from types
  import FuncPointer from types
  import Module from types
end

class SyscallDefinitionsTranslator < OMetaBase

<ometa>

start = definitions;

module :n = !{Module.new(n)}:m definitions(m) => m.toString;

definitions :m = [definition(m)+:xs] => xs;

definition :m = include(m) | struct_def(m) | func(m) | typedef(m);

include :m = [:include string:name] => m.appendInclude(name);

typedef :m = [:typedef string:n
              !{FuncPointer.new(n)}:fp
              [:func-pointer params(fp) !{fp.getRType()}:t type(t)]]
                => m.appendFuncPointer(fp)
           | [:typedef string:n !{Typedef.new(n)}:td !{td.getType()}:t type(t)]
                => m.appendTypedef(td)
           ;

struct_def :m = [:struct string:n !{Struct.new(n)}:s params(s)] => m.appendStruct(s)
              | [:struct string:n !{Struct.new(n)}:s] => m.appendStruct(s)
              ;
func :m = [:func string:n !{Fun.new(n)}:f
                 params(f) !{f.newReturnType()}:r
                 type(r):rt] => m.appendFun(f);
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
    | [:unsigned !{p.setIsUnsigned(true)} !{p.setTypeName("")}] // just to keep it implicit how it was found in the API
    | type0(p)
    ;
type0 :p
    = [:builtin string:t !{p.setTypeName(t)}]
    | [:struct string:t !{p.setTypeName("struct " + t)}]
    | [:unknown string:t !{p.setIsUnknown(true)} !{p.setTypeName(t)}]
    ;

</ometa>

end
