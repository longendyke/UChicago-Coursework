structure SmallStep2 : SMALL_STEP = struct

(* This datatype declaration allows us to type True instead of NB.True,
 * False instead of NB.False, etc. *)
  datatype term = datatype NB.term

(* Implement a modifiction of the one-step evaluation relation given
 * in Figures 3-1 and 3-2, such that the subcomponents of if terms are
 * evaluated from right to left. That is to say, in the term
 * (if t1 then t2 else t3), if it possible to take a step from t3 to t3',
 * then the term as a whole should step to (if t1 then t2 else t3'). If it is
 * not possible to take a step on t3, then try t2; else try t1. Otherwise,
 * if it is not possible to take a step on any of the three, then you can
 * either take the natural True/False step on the if term as a whole, or 
 * you have a stuck term and can go no further.
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
  				(* Operate on t3 first *)
  				(case t3
  					of If(t3_if) => let
  										val eval_ret = oneStep(t3)
  									in
  										if isSome(eval_ret) then 
  												let 
  													val return = If(t1, t2, valOf(eval_ret))
  												in
  													SOME return
												end
										else (* Oh boy here we go *)
											(case t2
  											(* Now, since we couldn't operate on t3, operate on t2 *)
  												of If(t2_if) => let
  																	val eval_ret = oneStep(t2)
  																in
  																	if isSome(eval_ret) then 
  																			let 
  																				val return = If(t1, valOf(eval_ret), t3)
  																			in
  																				SOME return
																			end
																	(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
																	else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																end
  												| Succ(t2_succ) => let
  																		val eval_ret = oneStep(t2_succ)
  																	in
  																		if isSome(eval_ret) then 
  																			let 
  																				val return = Succ(valOf(eval_ret))
						  													in
  																				SOME return
																			end
																	(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  																	else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																	end
  												| Pred(t2_pred) => (case t2_pred
  																		of Zero => SOME Zero
  																		| Succ(t2_pred_succ) => 
				  															if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  																			else 				
  																				let
  																					val eval_ret = oneStep(t2_pred)
  																				in
  																					if isSome(eval_ret) then 
  																						let 
  																							val return = Pred(valOf(eval_ret))
  																						in
  																							SOME return
  																						end
  																					(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																				end
  																		| _ => 
  																			let
  																				val eval_ret = oneStep(t2_pred)
  																			in
  																				if isSome(eval_ret) then 
  																					let 
  																						val return = Pred(valOf(eval_ret))
  																					in
  																						SOME return
  																					end
  																					(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																		end)
  																	(*ENDCASE*)
  												| IsZero(t2_iszero) => (case t2_iszero
  																		of Zero => SOME True
				  														| Succ(nv) => SOME False
  																		| _ => 				
  																			let
  																				val eval_ret = oneStep(t2_iszero)
  																			in
  																				if isSome(eval_ret) then 
  																						let 
  																							val return = IsZero(valOf(eval_ret))
  																						in
  																							SOME return
  																						end
  																				else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																			end)
  																		(*ENDCASE*)
  												| _ => if(not (NB.isBoolVal(t1))) then 
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
  															end
  														else
  															case t1
  															of True => SOME t2
  															| False => SOME t3
  															| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  												)
  											(*ENDCASE*)
  									end
  					| Succ(t3_succ) => let
  											val eval_ret = oneStep(t3_succ)
  										in
  											if isSome(eval_ret) then 
  												let 
  													val return = Succ(valOf(eval_ret))
  												in
  													SOME return
												end
  										else (* Oh god please no not again... *)
  											(case t2
  												(* Now, since we couldn't operate on t3, operate on t2 *)
  												of If(t2_if) => let
  																	val eval_ret = oneStep(t2)
  																in
  																	if isSome(eval_ret) then 
  																			let 
  																				val return = If(t1, valOf(eval_ret), t3)
  																			in
  																				SOME return
																			end
																	(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
																	else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																end
  												| Succ(t2_succ) => let
  																		val eval_ret = oneStep(t2_succ)
  																	in
  																		if isSome(eval_ret) then 
  																			let 
  																				val return = Succ(valOf(eval_ret))
						  													in
  																				SOME return
																			end
																	(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  																	else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																	end
  												| Pred(t2_pred) => (case t2_pred
  																		of Zero => SOME Zero
  																		| Succ(t2_pred_succ) => 
  																			if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  																			else 
  																				let
  																					val eval_ret = oneStep(t2_pred)
  																				in
  																					if isSome(eval_ret) then 
  																						let 
  																							val return = Pred(valOf(eval_ret))
  																						in
  																							SOME return
  																						end
  																					(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																				end
  																		| _ => 
  																			let
  																				val eval_ret = oneStep(t2_pred)
  																			in
  																				if isSome(eval_ret) then 
  																					let 
  																						val return = Pred(valOf(eval_ret))
  																					in
  																						SOME return
  																					end
  																					(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																		end)
  																	(*ENDCASE*)
  												| IsZero(t2_iszero) => (case t2_iszero
  																		of Zero => SOME True
  																		| Succ(nv) => SOME False
  																		| _ => 
  																			let
  																				val eval_ret = oneStep(t2_iszero)
  																			in
  																				if isSome(eval_ret) then 
  																						let 
  																							val return = IsZero(valOf(eval_ret))
  																						in
  																							SOME return
  																						end
  																				else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																			end)
  																		(*ENDCASE*)
  												| _ => if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																		else
  																			case t1
  																				of True => SOME t2
  																				| False => SOME t3
  																				| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  												)
  											(*ENDCASE*)
  										end
  					| Pred(t3_pred) => (case t3_pred
  											of Zero => SOME Zero
  											| Succ(t_pred_succ) => 
  												if(NB.isNumVal(t_pred_succ)) then SOME t_pred_succ
  												else 
  													let
  														val eval_ret = oneStep(t3_pred)
  													in
  														if isSome(eval_ret) then 
  															let 
  																val return = Pred(valOf(eval_ret))
  															in
  																SOME return
  															end
  														else 
  															(case t2
  																(* Now, since we couldn't operate on t3, operate on t2 *)
  																of If(t2_if) => let
  																					val eval_ret = oneStep(t2)
  																				in
  																					if isSome(eval_ret) then 
  																							let 
  																								val return = If(t1, valOf(eval_ret), t3)
  																							in
  																								SOME return
																							end
																					(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																				end
  																| Succ(t2_succ) => let
  																						val eval_ret = oneStep(t2_succ)
  																					in
  																						if isSome(eval_ret) then 
  																							let 
  																								val return = Succ(valOf(eval_ret))
						  																	in
  																								SOME return
																							end
																					(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																					end
  																| Pred(t2_pred) => (case t2_pred
  																						of Zero => SOME Zero
  																						| Succ(t2_pred_succ) => 
  																							if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  																							else 
  																								let
  																									val eval_ret = oneStep(t2_pred)
  																								in
  																									if isSome(eval_ret) then 
  																										let 
  																											val return = Pred(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																									(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																								end
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_pred)
  																							in
  																								if isSome(eval_ret) then 
  																									let 
  																										val return = Pred(valOf(eval_ret))
  																									in
  																										SOME return
  																									end
  																									(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																						end)
  																					(*ENDCASE*)
  																| IsZero(t2_iszero) => (case t2_iszero
  																						of Zero => SOME True
  																						| Succ(nv) => SOME False
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_iszero)
  																							in
  																								if isSome(eval_ret) then 
  																										let 
  																											val return = IsZero(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																								else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																							end)
  																						(*ENDCASE*)
  																| _ => if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																)
  															(*ENDCASE*)
  													end
  											| _ => 
  													let
  														val eval_ret = oneStep(t3_pred)
  													in
  														if isSome(eval_ret) then 
  															let 
  																val return = Pred(valOf(eval_ret))
  															in
  																SOME return
  															end
  														else 	
  															(case t2
  																(* Now, since we couldn't operate on t3, operate on t2 *)
  																of If(t2_if) => let
  																					val eval_ret = oneStep(t2)
  																				in
  																					if isSome(eval_ret) then 
  																							let 
  																								val return = If(t1, valOf(eval_ret), t3)
  																							in
  																								SOME return
																							end
																					(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																				end
  																| Succ(t2_succ) => let
  																						val eval_ret = oneStep(t2_succ)
  																					in
  																						if isSome(eval_ret) then 
  																							let 
  																								val return = Succ(valOf(eval_ret))
						  																	in
  																								SOME return
																							end
																					(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																					end
  																| Pred(t2_pred) => (case t2_pred
  																						of Zero => SOME Zero
  																						| Succ(t2_pred_succ) => 
  																							if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  																							else 
  																								let
  																									val eval_ret = oneStep(t2_pred)
  																								in
  																									if isSome(eval_ret) then 
  																										let 
  																											val return = Pred(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																									(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																									else
  																										case t1
  																											of True => SOME t2
  																											| False => SOME t3
  																											| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																								end
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_pred)
  																							in
  																								if isSome(eval_ret) then 
  																									let 
  																										val return = Pred(valOf(eval_ret))
  																									in
  																										SOME return
  																									end
  																									(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																						end)
  																					(*ENDCASE*)
  																| IsZero(t2_iszero) => (case t2_iszero
  																						of Zero => SOME True
  																						| Succ(nv) => SOME False
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_iszero)
  																							in
  																								if isSome(eval_ret) then 
  																										let 
  																											val return = IsZero(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																								else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																							end)
  																						(*ENDCASE*)
  																| _ => if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																)
  															(*ENDCASE*)
  													end)
  												(*ENDCASE*)
  					| IsZero(t3_iszero) => (case t3_iszero
  												of Zero => SOME True
  												| Succ(nv) => SOME False
  												| _ => 
  														let
  															val eval_ret = oneStep(t3_iszero)
  														in
  															if isSome(eval_ret) then 
  																	let 
  																		val return = IsZero(valOf(eval_ret))
  																	in
  																		SOME return
  																	end
  															else 
			  													(case t2
  																(* Now, since we couldn't operate on t3, operate on t2 *)
  																of If(t2_if) => let
  																					val eval_ret = oneStep(t2)
  																				in
  																					if isSome(eval_ret) then 
  																							let 
  																								val return = If(t1, valOf(eval_ret), t3)
  																							in
  																								SOME return
																							end
																					(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																				end
  																| Succ(t2_succ) => let
  																						val eval_ret = oneStep(t2_succ)
  																					in
  																						if isSome(eval_ret) then 
  																							let 
  																								val return = Succ(valOf(eval_ret))
						  																	in
  																								SOME return
																							end
																					(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  																					else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																					end
  																| Pred(t2_pred) => (case t2_pred
  																						of Zero => SOME Zero
  																						| Succ(t2_pred_succ) => 
  																							if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  																							else 
  																								let
  																									val eval_ret = oneStep(t2_pred)
  																								in
  																									if isSome(eval_ret) then 
  																										let 
  																											val return = Pred(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																									(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																								end
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_pred)
  																							in
  																								if isSome(eval_ret) then 
  																									let 
  																										val return = Pred(valOf(eval_ret))
  																									in
  																										SOME return
  																									end
  																									(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																									else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																						end)
  																					(*ENDCASE*)
  																| IsZero(t2_iszero) => (case t2_iszero
  																						of Zero => SOME True
  																						| Succ(nv) => SOME False
  																						| _ => 
  																							let
  																								val eval_ret = oneStep(t2_iszero)
  																							in
  																								if isSome(eval_ret) then 
  																										let 
  																											val return = IsZero(valOf(eval_ret))
  																										in
  																											SOME return
  																										end
  																								else if(not (NB.isBoolVal(t1))) then 
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
  																										end
  																								else
  																									case t1
  																										of True => SOME t2
  																										| False => SOME t3
  																										| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																							end)
  																						(*ENDCASE*)
  																| _ => if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																)
  															(*ENDCASE*)			
  														end)
  													(*ENDCASE*)
  					| _ => (case t2
  							(* Now, since we couldn't operate on t3, operate on t2 *)
  								of If(t2_if) => let
  													val eval_ret = oneStep(t2)
  												in
  													if isSome(eval_ret) then 
  															let 
  																val return = If(t1, valOf(eval_ret), t3)
  															in
  																SOME return
															end
													(* We couldn't operate on the if clause in t2! Now proceed to t1 *)
													else if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  												end
  								| Succ(t2_succ) => let
  														val eval_ret = oneStep(t2_succ)
  													in
  														if isSome(eval_ret) then 
  															let 
  																val return = Succ(valOf(eval_ret))
						  									in
  																SOME return
															end
													(* We couldn't operate on the Succ clause in t2! Now proceed to t1 *)
  													else if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  													end
  								| Pred(t2_pred) => (case t2_pred
  														of Zero => SOME Zero
  														| Succ(t2_pred_succ) => 
  															if(NB.isNumVal(t2_pred_succ)) then SOME t2_pred_succ
  															else 
  																let
  																	val eval_ret = oneStep(t2_pred)
  																in
  																	if isSome(eval_ret) then 
  																		let 
  																			val return = Pred(valOf(eval_ret))
  																		in
  																			SOME return
  																		end
  																	(* We couldn't operate on the Succ clause WITHIN the Pred clause of t2! Continuing to t1... *)
  																	else if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  																end
  														| _ => 
  															let
  																val eval_ret = oneStep(t2_pred)
  															in
  																if isSome(eval_ret) then 
  																	let 
  																		val return = Pred(valOf(eval_ret))
  																	in
  																		SOME return
  																	end
  																	(* We couldn't operate on the Pred clause of t2, proceed to t1...*)
  																	else if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  														end)
  													(*ENDCASE*)
  								| IsZero(t2_iszero) => (case t2_iszero
  														of Zero => SOME True
  														| Succ(nv) => SOME False
  														| _ => 
  															let
  																val eval_ret = oneStep(t2_iszero)
  															in
  																if isSome(eval_ret) then 
  																		let 
  																			val return = IsZero(valOf(eval_ret))
  																		in
  																			SOME return
  																		end
  																else if(not (NB.isBoolVal(t1))) then 
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
  																			end
  																	else
  																		case t1
  																			of True => SOME t2
  																			| False => SOME t3
  																			| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  															end)
  														(*ENDCASE*)
  								| _ => 
  										if(not (NB.isBoolVal(t1))) then 
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
  												end
  										else
  											case t1
  												of True => SOME t2
  												| False => SOME t3
  												| _ => NONE (* Technically we'll never run here but this will get rid of that pesky warning *)
  								)
  							(*ENDCASE*)
  					)
  				(*ENDCASE*)
  			end
  		| Succ(t_succ) => let
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

(* Similar to steps in SmallStep1, but use oneStep from this module. *)
  fun steps (t : term) : term list = 
  	let
  		val step_term = oneStep(t)
  	in
  		if(isSome(step_term))
  			then t::steps(valOf(step_term))
  		else
  			t::nil
  	end

(* Similar to eval in SmallStep1, but use oneStep from this module. *)
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
