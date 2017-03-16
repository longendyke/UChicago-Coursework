structure Evaluate : sig

(* eval : Evaluate the given term. Make sure if and nand evaluation are
 *        both properly short-circuiting. Big-step is fine and
 *        appropriate.
 *)
  val eval : CoreNBT.term -> CoreNBT.term

end  = struct

  exception RuntimeError of string

  structure C = CoreNBT

  (*datatype term
    = True
    | False
    | If of term * term * term
    | Nand of term * term
    | Zero
    | Succ of term
    | Pred of term
    | IsZero of term*)

  fun eval (p : C.term) : C.term = 
  	case p
  		of C.True => C.True
  		| C.False => C.False
  		| C.Zero => C.Zero
  		| C.If(t_if) =>
  			let
  				val (t1, t2, t3) = t_if
  			in
  				(case t1
  					of C.True => eval(t2)
  					| C.False => eval(t3)
  					| _ => 
  						let
  							val eval_ret = eval(t1)
  						in
  							let 
  								val return = C.If(eval_ret, t2, t3)
  							in
  								eval(return)
							end
  						end
  				)
  				(*ENDCASE*)
  			end
  		| C.Succ(t_succ) => 
  			if C.isNumVal(t_succ) then C.Succ(t_succ) 
  			else eval(t_succ)
  		| C.Pred(t_pred) =>
  			(case t_pred
  				of C.Zero => C.Zero
  				| C.Succ(t_pred_succ) => 
  					if(C.isNumVal(t_pred_succ)) then eval(t_pred_succ)
  					else 
  						let
  							val eval_ret = eval(t_pred)
  						in
  							let 
  								val return = C.Pred(eval_ret)
  							in
  								eval(return)
  							end
  						end
  				| _ => 
  					let
  						val eval_ret = eval(t_pred)
  					in
  						let 
  							val return = C.Pred(eval_ret)
  						in
	  						eval(return)
						end
  					end)
  			(*ENDCASE*)
  		| C.IsZero(t_0) =>
  			(case t_0
  				of C.Zero => C.True
  				| C.Succ(nv) => C.False
  				| _ => 
  						let
  							val eval_ret = eval(t_0)
  						in
  							let 
  								val return = C.IsZero(eval_ret)
  							in
  								eval(return)
 							end
  						end
  			)
  			(*ENDCASE*)
  		| C.Nand(t1, t2) =>
  			(case (t1, t2)
  				of ( C.False, _ ) => C.True
  				| ( _ , C.False ) => C.True
  				| ( C.True, C.True ) => C.False
  				| ( _ , _ ) => 
  						let
  							val eval1 = eval(t1)
  							val eval2 = eval(t2)
  							val eval_ret = C.Nand(eval1, eval2)
  						in
  							eval(eval_ret)
  						end
  			)


end
