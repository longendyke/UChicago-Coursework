structure SmallStep1 : SMALL_STEP = struct

(* This datatype declaration allows us to type True instead of NB.True,
 * False instead of NB.False, etc. *)
  datatype term = datatype NB.term

(* Implement the one-step evaluation relation given in Figures 3-1 and
 * 3-2 of the text. Return NONE for normal forms.
 *)
  fun oneStep (t : term) : term option = 
  	case t
  		of True => NONE
  		| False => NONE
  		| Zero => NONE
  		| If(t_if) =>
  			let
  				val (t1, t2, t3) = t_if
  			in
  				(case t1
  					of True => SOME t2
  					| False => SOME t3
  					| _ => 
  						let
  							val eval_ret = oneStep(t1)
  						in
  							if isSome(eval_ret) then 
  									let 
  										val return = If(valOf(eval_ret), t2, t3)
  									in
  										SOME return
									end
  							else NONE
  						end)
  				(*ENDCASE*)
  			end
  		| Succ(t_succ) => 	let
  								val eval_ret = oneStep(t_succ)
  							in
  								if isSome(eval_ret) then 
  									let 
  										val return = Succ(valOf(eval_ret))
  									in
  										SOME return
									end
  							else NONE
  							end
  		| Pred(t_pred) =>
  				(case t_pred
  					of Zero => SOME Zero
  					| Succ(t_pred_succ) => 
  						if(NB.isNumVal(t_pred_succ)) then SOME t_pred_succ
  						else 
  							let
  								val eval_ret = oneStep(t_pred)
  							in
  								if isSome(eval_ret) then 
  									let 
  										val return = Pred(valOf(eval_ret))
  									in
  										SOME return
  									end
  								else NONE
  							end
  					| _ => 
  							let
  								val eval_ret = oneStep(t_pred)
  							in
  								if isSome(eval_ret) then 
  									let 
  										val return = Pred(valOf(eval_ret))
  									in
  										SOME return
  									end
  								else NONE
  							end)
  				(*ENDCASE*)
  		| IsZero(t_0) =>
  			(case t_0
  				of Zero => SOME True
  				| Succ(nv) => SOME False
  				| _ => 
  						let
  							val eval_ret = oneStep(t_0)
  						in
  							if isSome(eval_ret) then 
  									let 
  										val return = IsZero(valOf(eval_ret))
  									in
  										SOME return
  									end
  							else NONE
  						end)
  			(*ENDCASE*)

(* Return the list of all steps in the evaluation of a term, stopping
 * at a normal form. Use oneStep from this module in your implementation. *)
(* ex: steps True --> [True] *)
(* ex: steps (If (True, Pred Zero, Succ Zero)) --> 
 *     [If (True, Pred Zero, Succ Zero), Pred Zero, Zero] *)
(* ex: steps (Pred True) --> [Pred True] *)
  fun steps (t : term) : term list = 
  	let
  		val step_term = oneStep(t)
  	in
  		if(isSome(step_term))
  			then t::steps(valOf(step_term))
  		else
  			t::nil
  	end

(* Similar to the steps function above, but returns only the final
 * normal form in the evaluation steps. Use oneStep from this module
 * in your implementation. *)
  fun eval (t : term) : term = 
  	let 
  		val step_term = oneStep(t)
  	in
  		if(isSome(step_term))
  			then eval(valOf(step_term))
  		else
  			t
  	end

end
