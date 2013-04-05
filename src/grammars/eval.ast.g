exec_fun = exprlist:x -> x
         | primitive:p   -> self.i.eval_prim(p)

primitive = ['primitive' ['literal-string' :x] (:ignore)*] -> x

exprlist = [(expr:x)+] -> x

pair = ['pair' expr:key expr:val] -> (key,val)

expr = ['super-ctor-send' :s args:a]         -> self.i.eval_do_super_ctor_send(s,a)
     | ['var-def' :id expr:e]                -> self.i.eval_do_var_def(id,e)
     | ['return' expr:x]                     -> self.i.eval_do_return(x)
     | ['not' expr:x]                        -> self.i.todo5()
     | ['++' expr:x expr:y]                  -> self.i.todo6()
     | ['+' expr:x expr:y]                   -> self.i.eval_do_bin_send('+',x,y)
     | ['*' expr:x expr:y]                   -> self.i.todo8()
     | ['<' expr:x expr:y]                   -> self.i.todo9()
     | ['==' expr:x expr:y]                  -> self.i.todo10()
     | ['!=' expr:x expr:y]                  -> self.i.todo11()
     | ['call' expr:e args:a]                -> self.i.eval_do_call(e,a)
     | ['setter' expr:r :s args:a]           -> self.i.todo14()
     | ['getter' expr:r :s]                  -> self.i.todo15()
     | ['send-or-call' :e args:a]            -> self.i.eval_do_send_or_call(e,a)
     | ['send' expr:r :s args:a]             -> self.i.eval_do_send(r,s,a)
     | ['if' expr:c [expr*:yes]]             -> self.i.todo18()
     | ['if' expr:c [expr*:yes] [expr+:no]]  -> self.i.todo19()
     | ['while' expr:c [expr*:yes]]          -> self.i.todo20()
     | ['literal-array'  expr*:r]            -> r
     | ['literal-dict'  pair*:r]             -> dict(r)
     | ['fun-literal'  ['params' :p]
          ['body' :b]]                       -> self.i.eval_do_fun_lit(p,b)
     | ['return-this']                       -> self.i.eval_do_return(self.i.r_rp)
     | ['return-null']                       -> self.i.eval_do_return(None)
     | ['return-top']                        -> self.i.todo26()
     | attribution
     | atom

attribution = ['=' ['id' :v] expr:rhs]     -> self.i.eval_do_local_attr(v,rhs)
            | ['=' ['field' :f] expr:rhs]  -> self.i.eval_do_field_attr(f,rhs)

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
