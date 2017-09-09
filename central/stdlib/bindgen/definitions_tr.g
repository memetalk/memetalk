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
    = [definition+:x] => x.map(fun(i) { i.toString }).join("\n\n");

definition = include | struct_def | func;

include
    = [:include string:name] => "#include " + name;

struct_def
    = [:struct string:name !{Struct.new}:sobj !{sobj.setName(name)}
               params(sobj)] => sobj
    | [:struct string:name !{Struct.new}:sobj !{sobj.setName(name)}] => sobj;

func
    = [:func string:name !{Fun.new(name)}:fobj
             params(fobj):p !{fobj.newReturnType()}:rtobj
             type(rtobj):rtype]
      => fobj;

params :obj = [param(obj)*:x];

param :obj
    = [:list !{obj.newChild()}:pobj !{pobj.setIsArray(true)}
       [type(pobj):type string:name !{pobj.setName(name)}]] => pobj
    | [!{obj.newChild()}:pobj
       type(pobj):type string:name !{pobj.setName(name)}] => pobj
    ;

type :p = type_pointer(p) | type_const(p) | type_unsigned(p) | type0(p);

type_pointer :pobj
    = [:pointer type(pobj):t _:c !{pobj.setIsPointer(c)}];
type_const :pobj
    = [:const type(pobj):t !{pobj.setIsConst(true)}];
type_unsigned :pobj
    = [:unsigned type(pobj):t !{pobj.setIsUnsigned(true)}];

type0 :pobj
    = [:builtin string:t !{pobj.setTypeName(t)}]
    | [:struct string:t !{pobj.setTypeName("struct " + t)}]
    ;

</ometa>

end
