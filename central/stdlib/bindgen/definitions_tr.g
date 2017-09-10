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

<ometa>

start = definitions;

definitions
    = [definition+:x] => x.map(fun(i) { i.toString }).join("\n\n")
    ;
definition = include | struct_def | func;

include
    = [:include string:name] => "#include " + name
    ;
struct_def
    = [:struct string:name !{Struct.new}:sobj !{sobj.setName(name)}
               params(sobj)] => sobj
    | [:struct string:name !{Struct.new}:sobj !{sobj.setName(name)}] => sobj
    ;
func
    = [:func string:name !{Fun.new(name)}:fobj
             params(fobj) !{fobj.newReturnType()}:rtobj
             type(rtobj):rtype] => fobj
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
