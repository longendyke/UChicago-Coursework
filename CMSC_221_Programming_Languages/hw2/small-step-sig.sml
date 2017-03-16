signature SMALL_STEP = sig

(* Take one step of evaluation according to a small-step evaluation 
 * relation on terms in NB. If no evaluation rule applies, i.e, if the
 * term is in normal form, return NONE.
 *)
  val oneStep : NB.term -> NB.term option  

(* Collect all steps that can be taken using oneStep. *)
  val steps   : NB.term -> NB.term list

(* Evaluate a term as far as possible, until it is in normal form. *)
  val eval    : NB.term -> NB.term

end

