meme central:memescript/compiler

requires io, ometa_base
where
  io             = central:stdlib/io
  ometa_base     = central:memescript/ometa_base
import OMetaBase from ometa_base

class SyscallDefinitionsParser < OMetaBase

<ometa>

space =  _:c ?{c.onlySpaces} => c
      | comment;

comment = '//' 'sys';

start = definition*;

definition
    = func:f "\n"* => f
    | include:i "\n"* => i
    ;

include
    = "#" spaces "include" spaces "<" {~">" _}+:fname ">"
      => [:include, "<" + fname.join("") + ">"]
    | "#" spaces "include" spaces '"' {~'"' _}+:fname '"'
      => [:include, "\"" + fname.join("") + "\""]
    ;

func = rtype:rp spaces id:funcname spaces params:p
    => [:func, funcname, p, rp];

rtype = type_pointer | type;

params = "(" paramlist:pl ")" => pl;

paramlist = typed:x {"," spaces typed}*:xs => [x] + xs | => [];

typed = type_list | type_id;
type_list = type_id:x '[]' => [:list, x];
type_id = {type_pointer | type}:t spaces id:n => [t, n];
type_pointer = type:x spaces "*"+:star => [:pointer, x, star.size()];

type = unsigned | const | struct | builtins;

struct = ``struct`` spaces id:x => [:struct, x];
const = ``const`` spaces type:x => [:const, x];
unsigned = ``unsigned`` spaces type:x => [:unsigned, x];
builtins = {``void`` | ``int`` | ``float`` | ``short`` | ``long`` | ``char``}:x
    => [:builtin, x];

id = {letter | '_'}:x identifier_rest*:xs => ([x] + xs).join("");


</ometa>

end
