structure Basis = struct

  infix -->
  fun ty1 --> ty2 = Ty.Arrow (ty1, ty2)

(* These are the predicates your list comprehension language knows about.
 * You may add to this basis, but do not remove any of the first five items.
 *)
  val env = 
      [("even", Ty.Nat  --> Ty.Bool),
       ("odd",  Ty.Nat  --> Ty.Bool),
       ("pos",  Ty.Nat  --> Ty.Bool),
       ("zero", Ty.Nat  --> Ty.Bool),
       ("not",  Ty.Bool --> Ty.Bool)]
  (* Tip: you can use List.find to search through lists of pairs. *)

end
