meme central:memescript/compiler

requires io, ometa_base
where
  io         = central:stdlib/io
  ometa_base = central:memescript/ometa_base
  import OMetaBase from ometa_base
end

class SyscallDefinitionsParser < OMetaBase

<ometa>

start = definition*:xs => xs;

definition
    = prefix typedef:t => t
    | prefix func:f => f
    | prefix include:i => i
    | prefix struct_def:s => s
    ;

prefix = spaces '//' 'sys';

include
    = "#" spaces "include" spaces "<" {~">" _}+:fname ">"
      => [:include, "<" + fname.join("") + ">"]
    | "#" spaces "include" spaces '"' {~'"' _}+:fname '"'
      => [:include, "\"" + fname.join("") + "\""]
    ;

typedef = ``typedef`` spaces type_only:t spaces id:id => [:typedef, id, t];

struct_def
    = struct:s spaces struct_body:b !{s.append(b)} => s
    | struct:s => s
    ;

struct_body = "{" spaces struct_field*:fields "}" => fields;

struct_field = typed:t ``;`` => t;

func = rtype:rp spaces id:funcname spaces params:p
    => [:func, funcname, p, rp];

rtype = type_pointer | type;

params
    = "(void)" => []
    | "(" paramlist:pl ")" => pl
    ;
paramlist
    = typed:x {"," spaces typed}*:xs => [x] + xs
    | => []
    ;

typed = type_with_annotations | type_list_or_id | varargs;
type_with_annotations = type_list_or_id:x spaces type_annotation+:as
    => [[:annotations, x[0], as], x[1]];
type_annotation = "+" {``out`` | ``null``}:x =>  x;
varargs = ``...`` => [:varargs];

type_list_or_id = type_list | type_id | type_only;
type_list = type_id:x '[]' => [[:list, x[0]], x[1]];
type_id = {type_pointer | type}:t spaces id:n => [t, n];
type_only = {type_pointer | type}:t => t;
type_pointer = type:x spaces "*"+:star => [:pointer, x, star.size()];
type = type0 | unknown;
type0 = signed | unsigned | const | struct | builtins;

struct = ``struct`` spaces id:x => [:struct, x];
const = ``const`` spaces type0:x => [:const, x]
    | ``const`` spaces unknown:x => [:const, x]
    ;
unknown = spaces id:x => [:unknown, x];
signed = ``signed`` spaces type0:x => [:signed, x];
unsigned
    = ``unsigned`` spaces type0:x => [:unsigned, x]
    | ``unsigned`` => [:unsigned]
    ;
builtins = {``void`` | ``char`` | numeric}:x => [:builtin, x];
numeric
    = ``complex long double``
    | ``complex long``
    | ``complex double``
    | ``complex float``
    | ``short int``
    | ``short``
    | ``long long int``
    | ``long long``
    | ``long double``
    | ``long int``
    | ``long``
    | ``double``
    | ``float``
    | ``int``
    ;

id = {letter | '_'}:x identifier_rest*:xs => ([x] + xs).join("");

</ometa>

end
