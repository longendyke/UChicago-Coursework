structure Elaborate : sig

(* elab: Rewrite the surface NBT term into a core term.
 *       Specifically, rewrite unless, or, and, not, and naturals
 *       into if, nand, and succ terms as appropriate.
 *)
  val elab : SurfaceNBT.term -> CoreNBT.term

end = struct

  structure S = SurfaceNBT
  structure C = CoreNBT

  fun elab (p : S.term) : C.term = 
  	case p
  		of S.True => C.True
  		| S.False => C.False
  		| S.If(t1, t2, t3) => 
  							C.If(elab(t1), elab(t2), elab(t3))
  		| S.Unless(t1, t2, t3) => 
  								C.If(C.Nand(elab(t1), elab(t1)), elab(t2), elab(t3))
  		| S.Or(t1, t2) => 
  							C.Nand(C.Nand(elab(t1), elab(t1)), C.Nand(elab(t2), elab(t2)))
  		| S.And(t1, t2) => 
  							C.Nand(C.Nand(elab(t1), elab(t2)), C.Nand(elab(t1), elab(t2)))
  		| S.Not(t1) => 
  						C.Nand(elab(t1), elab(t1))
  		| S.Nat(natural) => 
  							if natural = 0 then C.Zero 
  							else C.Succ(elab(S.Nat(natural - 1)))
  		| S.Succ(succ) => 
  							C.Succ(elab(succ))
  		| S.Pred(pred) => 
  							C.Pred(elab(pred))
  		| S.IsZero(is_zero) => 
  								C.IsZero(elab(is_zero))
  	(* EndCase *)
end

