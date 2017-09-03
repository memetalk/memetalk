meme central:memescript/compiler

requires io, ometa_base
where
  io             = central:stdlib/io
  ometa_base     = central:memescript/ometa_base
import OMetaBase from ometa_base

class Param
  fields: name, typeName, isPointer, isArray, isConst, isUnsigned;

  init new: fun() {
    @isPointer = false;
    @isArray = false;
    @isConst = false;
    @isUnsigned = false;
  }
  instance_method setName: fun(name) { @name = name; }
  instance_method setTypeName: fun(typeName) { @typeName = typeName; }
  instance_method setIsArray: fun(isArray) { @isArray = isArray; }
  instance_method setIsPointer: fun(isPointer) { @isPointer = isPointer; }
  instance_method setIsConst: fun(isConst) { @isConst = isConst; }
  instance_method setIsUnsigned: fun(isUnsigned) { @isUnsigned = isUnsigned; }

  instance_method getName: fun() { return @name; }
  instance_method getTypeName: fun() { return @typeName; }
  instance_method getIsArray: fun() { return @isArray; }
  instance_method getIsPointer: fun() { return @isPointer; }
  instance_method getIsConst: fun() { return @isConst; }
  instance_method getIsUnsigned: fun() { return @isUnsigned; }

  instance_method toString: fun() {
    return [
      "       Name: " + @name.toString,
      "   TypeName: " + @typeName.toString,
      "    IsArray: " + @isArray.toString,
      "    IsConst: " + @isConst.toString,
      "  IsPointer: " + @isPointer.toString,
      " IsUnsigned: " + @isUnsigned.toString
    ].join("\n");
  }
end

class Fun
  fields: name, params, rtype;

  init new: fun(name) {
    @name = name;
    @params = [];
  }
  instance_method newParam: fun() {
    var param = Param.new();
    @params.append(param);
    return param;
  }
  instance_method newReturnType: fun() {
    var rtype = Param.new();
    @rtype = rtype;
    return rtype;
  }
  instance_method getName: fun() {
    return @name;
  }
  instance_method getParams: fun() {
    return @params;
  }
  instance_method getReturnType: fun() {
    return @rtype;
  }
  instance_method genFunSignature: fun(fobj) {
    return ["static", "int", "prim_" + fobj.getName(),
            "(Process* proc)", "{"].join(" ");
  }
  instance_method genGetArg: fun(i, arg) {
    return ["  oop", "oop_" + arg.getName(), "=",
            "proc->get_arg(" + i.toString() + ");"].join(" ");
  }
  instance_method genChkArg: fun(arg) {
    var out = [];
    var primeType = {
        "int": "Integer",
        "char": "String"
    }[arg.getTypeName()];
    out.append("  if (!(proc->mmobj()->mm_object_vt(oop_" + arg.getName() +
               ") == proc->vm()->get_prime(\"" + primeType + "\"))) {");
    out.append("    proc->raise(\"TypeError\", e);");
    out.append("  }");
    return out.join("\n");
  }
  instance_method genType: fun(arg) {
    var out = [];
    if (arg.getIsConst()) { out.append("const"); }
    if (arg.getIsUnsigned()) { out.append("unsigned"); }
    out.append(arg.getTypeName());
    if (arg.getIsPointer()) { out.append("*"); }
    return out.join(" ");
  }
  instance_method genUnwrapArg: fun(arg) {
    var typeName = arg.getTypeName();
    var argName = "oop_" + arg.getName();
    var unwrap = {
      "int": "untag_small_int(" + argName + ")",
      "char": "proc->mmobj()->mm_string_cstr(proc, " + argName + "))"
    }[typeName];
    if (!unwrap) {
      Exception.throw("Unknown type `" + typeName + "'");
    }
    var out = [" "];            // indentation
    out.append(this.genType(arg));
    return (out + [argName, "=", unwrap + ";"]).join(" ");
  }
  instance_method genFunCall: fun(fobj) {
    var paramNames = fobj.getParams().map(fun(p) { p.getName });
    var params = "(" + paramNames.join(", ") + ");";
    var rtype = fobj.getReturnType();
    var out = [];
    if (rtype) {
      var typeName = rtype.getTypeName();
      var wrap = {
        "int": "tag_small_int(oop_output)",
        "char": "proc->mmobj()->mm_string_new(proc, oop_output)"
      }[typeName];
      if (!wrap) {
        Exception.throw("Unknown type `" + typeName + "'");
      }

      out.append(" ");          // indentation
      out.append(this.genType(rtype));
      out = out + ["output", "=", fobj.getName(), params, "\n",
              " oop", "oop_output", "=", wrap + ";\n",
              " proc->stack_push(oop_output);"];
    }

    return out.join(" ");
  }
  instance_method genFun: fun(fobj) {
    var output = [];
    output.append(this.genFunSignature(fobj));
    fobj.getParams().each(fun(i, p) {
      output.append(this.genGetArg(i, p));
      output.append(this.genChkArg(p));
      output.append(this.genUnwrapArg(p));
    });
    output.append("\n  /* Call to the underlying function */");
    output.append(this.genFunCall(fobj));
    output.append("  return 0;");
    output.append("}");
    return output.join("\n");
  }
end

class SyscallDefinitionsTranslator < OMetaBase

<ometa>

start = funcs;

funcs = [func+:x] => x.join("\n\n");

func
    = [:func string:name !{Fun.new(name)}:fobj
             params(fobj):p !{fobj.newReturnType()}:rtobj
             type(rtobj):rtype]
      => fobj.genFun(fobj);

params :fobj = [param(fobj)*:x];

param :fobj
    = [:list !{fobj.newParam()}:pobj !{pobj.setIsArray(true)}
       [type(pobj):type string:name !{pobj.setName(name)}]]
    | [!{fobj.newParam()}:pobj
       type(pobj):type string:name !{pobj.setName(name)}]
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
