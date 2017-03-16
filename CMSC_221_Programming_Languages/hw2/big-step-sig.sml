signature BIG_STEP = sig

(* Evaluate an NB term all the way to SOME value, if possible. *)
(* If there is no such evaluation, return NONE. *)
  val eval : NB.term -> NB.term option

end
