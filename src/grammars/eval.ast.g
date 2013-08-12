exec_fun = exprlist:x -> x
         | [['primitive' ['literal-string' :name]]:ast (:ignore)*]   -> self.i.eval_prim(name,ast)

exprlist = [(expr:x)+] -> x

pair = ['pair' expr:key expr:val] -> (key,val)

expr = ['super-ctor-send' :s args:a]:ast     -> self.i.eval_do_super_ctor_send(s,a,ast)
     | ['var-def' :id expr:e]:ast            -> self.i.eval_do_var_def(id,e,ast)
     | ['debug']:ast                         -> self.i.eval_do_debug(ast)
     | ['return' expr:x]:ast                 -> self.i.eval_do_return(x,ast)
     | ['not' expr:x]:ast                    -> self.i.eval_do_un_not(x,ast)
     | ['negative' expr:x]:ast               -> self.i.eval_do_un_neg(x,ast)
     | ['++' expr:x expr:y]                  -> self.i.todo6()
     | ['+' expr:x expr:y]:ast               -> self.i.eval_do_bin_send('+',x,y,ast)
     | ['-' expr:x expr:y]:ast               -> self.i.eval_do_bin_send('-',x,y,ast)
     | ['*' expr:x expr:y]                   -> self.i.todo8()
     | ['<' expr:x expr:y]:ast               -> self.i.eval_do_bin_send('<',x,y,ast)
     | ['<=' expr:x expr:y]:ast              -> self.i.eval_do_bin_send('<=',x,y,ast)
     | ['==' expr:x expr:y]:ast              -> self.i.eval_do_bin_send('==',x,y,ast)
     | ['!=' expr:x expr:y]:ast              -> self.i.eval_do_bin_send('!=',x,y,ast)
     | ['call' expr:e args:a]:ast            -> self.i.eval_do_call(e,a,ast)
     | ['setter' expr:r :s args:a]           -> self.i.todo14()
     | ['getter' expr:r :s]                  -> self.i.todo15()
     | ['send-or-call' :e args:a]:ast        -> self.i.eval_do_send_or_call(e,a,ast)
     | ['send' expr:r :s args:a]:ast         -> self.i.eval_do_send(r,s,a,ast)
     | ['index' expr:r expr:i]:ast           -> self.i.eval_do_access_index(r,i,ast)
     | ['if' :c :yes]:ast
          apply('expr' c):cond               -> self.i.eval_do_if(cond,yes)
     | ['if' :c :yes :no]:ast
          apply('expr' c):cond               -> self.i.eval_do_if_else(cond,yes,no)
     | ['while' expr:c [expr*:yes]]          -> self.i.todo20()
     | ['try' :tr :id :ct]:ast               -> self.i.eval_do_try(ast,tr,id,ct)
     | ['literal-array'  expr*:r]            -> r
     | ['literal-dict'  pair*:r]             -> dict(r)
     | ['fun-literal'  ['params' :p]
          ['body' :b]]:ast                   -> self.i.eval_do_fun_lit(p,b,ast)
     | ['return-this']:ast                   -> self.i.eval_do_return(self.i.r_rp,ast)
     | ['return-null']:ast                   -> self.i.eval_do_return(None,ast)
     | ['return-top']                        -> self.i.todo26()
     | assignment
     | atom

assignment = ['=' ['id' :v] expr:rhs]:ast    -> self.i.eval_do_local_assign(v,rhs,ast)
           | ['=' ['field' :f] expr:rhs]:ast -> self.i.eval_do_field_assign(f,rhs,ast)

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
    |  ['literal' 'context']      -> self.i.eval_access_context()
    |  ['id' :x]:ast              -> self.i.eval_access_var(x,ast)
    |  ['field' :x]               -> self.i.eval_access_field(x)
