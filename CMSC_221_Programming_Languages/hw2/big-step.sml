structure BigStep : BIG_STEP = struct

(* This datatype declaration allows us to type True instead of NB.True,
 * False instead of NB.False, etc. *)
  datatype term = datatype NB.term

(* Implement the big-step evaluation relation given in exercise
 * 3.5.17. The big-step relation describes an "all-the-way" evaluation 
 * rather than an individual step of evaluation, like the small step 
 * relations in Figures 3-1 and 3-2. For stuck terms, i.e., normal forms
 * that are not values, return NONE.
 *)
(* ex: eval True        --> SOME True *)
(* ex: eval (Pred Zero) --> SOME Zero *)
(* ex: eval (Pred True) --> NONE *)
 fun eval (t : term) : term option = 
  	case t
  		of True => SOME True
  		| False => SOME False
  		| Zero => SOME Zero
  		| If(t_if) =>
  			let
  				val (t1, t2, t3) = t_if
  			in
  				(case t1
  					of True => eval(t2)
  					| False => eval(t3)
  					| _ => 
  						let
  							val eval_ret = eval(t1)
  						in
  							if isSome(eval_ret) then
  								if NB.isBoolVal(valOf(eval_ret)) then 
  									let 
  										val return = If(valOf(eval_ret), t2, t3)
  									in
  										eval(return)
									end
								else NONE
  							else NONE
  						end)
  				(*ENDCASE*)
  			end
  		| Succ(t_succ) => 	let
  								val eval_ret = eval(t_succ)
  							in
  								if isSome(eval_ret) then
  									if NB.isNumVal(valOf(eval_ret)) then 
  										let 
  											val return = Succ(valOf(eval_ret))
  										in
	  										SOME return
										end
									else NONE
  								else NONE
  							end
  		| Pred(t_pred) =>
  				(case t_pred
  					of Zero => SOME Zero
  					| Succ(t_pred_succ) => 
  						if(NB.isNumVal(t_pred_succ)) then SOME t_pred_succ
  						else 
  							let
  								val eval_ret = eval(t_pred)
  							in
  								if isSome(eval_ret) then 
  									let 
  										val return = Pred(valOf(eval_ret))
  									in
  										eval(return)
  									end
  								else NONE
  							end
  					| _ => 
  							let
  								val eval_ret = eval(t_pred)
  							in
  								if isSome(eval_ret) then
  									if NB.isNumVal(valOf(eval_ret)) then 
  										let 
  											val return = Pred(valOf(eval_ret))
  										in
	  										eval(return)
										end
									else NONE
  								else NONE
  							end)
  				(*ENDCASE*)
  		| IsZero(t_0) =>
  			(case t_0
  				of Zero => SOME True
  				| Succ(nv) => SOME False
  				| _ => 
  						let
  							val eval_ret = eval(t_0)
  						in
  							if isSome(eval_ret) then 
  								if(NB.isNumVal(valOf(eval_ret))) then
  									let 
  										val return = IsZero(valOf(eval_ret))
  									in
  										eval(return)
  									end
  								else NONE
  							else NONE
  						end)
  			(*ENDCASE*)
end