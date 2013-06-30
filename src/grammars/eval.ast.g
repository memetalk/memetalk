exec_fun = exprlist:x -> x
         | ['primitive' ['literal-string' :name] (:ignore)*]:ast   -> self.i.eval_prim(name)

exprlist = [(expr:x)+] -> x

pair = ['pair' expr:key expr:val] -> (key,val)

expr = ['super-ctor-send' :s args:a]:ast     -> self.i.eval_do_super_ctor_send(s,a)
     | ['var-def' :id expr:e]:ast            -> self.i.eval_do_var_def(id,e)
     | ['debug']:ast                         -> self.i.eval_do_debug(ast)
     | ['return' expr:x]:ast                 -> self.i.eval_do_return(x,ast)
     | ['not' expr:x]                        -> self.i.eval_do_un_not(x)
     | ['++' expr:x expr:y]                  -> self.i.todo6()
     | ['+' expr:x expr:y]                   -> self.i.eval_do_bin_send('+',x,y)
     | ['-' expr:x expr:y]                   -> self.i.eval_do_bin_send('-',x,y)
     | ['*' expr:x expr:y]                   -> self.i.todo8()
     | ['<' expr:x expr:y]                   -> self.i.eval_do_bin_send('<',x,y)
     | ['<=' expr:x expr:y]                  -> self.i.eval_do_bin_send('<=',x,y)
     | ['==' expr:x expr:y]                  -> self.i.eval_do_bin_send('==',x,y)
     | ['!=' expr:x expr:y]                  -> self.i.eval_do_bin_send('!=',x,y)
     | ['call' expr:e args:a]:ast            -> self.i.eval_do_call(e,a,ast)
     | ['setter' expr:r :s args:a]           -> self.i.todo14()
     | ['getter' expr:r :s]                  -> self.i.todo15()
     | ['send-or-call' :e args:a]:ast        -> self.i.eval_do_send_or_call(e,a,ast)
     | ['send' expr:r :s args:a]:ast         -> self.i.eval_do_send(r,s,a,ast)
     | ['if' expr:cond :yes]                 -> self.i.eval_do_if(cond,yes)
     | ['if' expr:cond :yes :no]             -> self.i.eval_do_if_else(cond,yes,no)
     | ['while' expr:c [expr*:yes]]          -> self.i.todo20()
     | ['literal-array'  expr*:r]            -> r
     | ['literal-dict'  pair*:r]             -> dict(r)
     | ['fun-literal'  ['params' :p]
          ['body' :b]]                       -> self.i.eval_do_fun_lit(p,b)
     | ['return-this']:ast                   -> self.i.eval_do_return(self.i.r_rp,ast)
     | ['return-null']:ast                   -> self.i.eval_do_return(None,ast)
     | ['return-top']                        -> self.i.todo26()
     | attribution
     | atom

attribution = ['=' ['id' :v] expr:rhs]:ast    -> self.i.eval_do_local_attr(v,rhs,ast)
            | ['=' ['field' :f] expr:rhs]:ast -> self.i.eval_do_field_attr(f,rhs,ast)

args =  ['args' arglist:x] -> x
     |  ['args' []] -> []

arglist = [expr+:lst] -> lst

atom = ['literal-string' :x]      -> x
    |  ['literal-number' :x]      -> x
    |  ['literal-symbol' :x]      -> self.i.todo32()
    |  ['literal' 'this']         -> self.i.eval_access_this()
    |  ['literal' 'null']         -> None
    |  ['literal' 'true']         -> True
    |  ['literal' 'false']        -> False
    |  ['literal' 'module']       -> self.i.eval_access_module()
    |  ['id' :x]                  -> self.i.eval_access_var(x)
    |  ['field' :x]               -> self.i.eval_access_field(x)
