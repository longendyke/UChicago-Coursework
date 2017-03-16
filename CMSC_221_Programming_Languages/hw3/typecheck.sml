structure Typecheck : sig

  exception TypeError of string

(* typeOf: Calculate the type of the given term, if possible.
 *         Follow the typing rules in Chapter 8 of the text.
 *         (Extrapolate the Nand rule.)
 *)
  val typeOf : CoreNBT.term -> Type.ty

(* check: Return unit if the type is calculable, else raise TypeError.
 *        The implementation is given and is a thin wrapper around typeOf. 
 *)
  val check : CoreNBT.term -> unit

end = struct

  exception TypeError of string

  structure C = CoreNBT
  structure T = Type

  fun typeOf (p : C.term) : T.ty = 
  	case p
  		of C.True => T.Bool
  		| C.False => T.Bool
  		| C.Zero => T.Nat
  		| C.If(t1, t2, t3) => let
  									val tp1 = typeOf(t1)
  									val tp2 = typeOf(t2)
  									val tp3 = typeOf(t3)
  								in 
  									if not (tp1 = T.Bool) then raise TypeError "TypeError--If predicate is not of type Bool."
  									else if not(T.eq(tp2, tp3))
  							  		then raise TypeError "TypeError--If statement types do not match."
  							  		else tp2
  							  	end
  		| C.Nand(t1, t2) => let
  								val tp1 = typeOf(t1)
  								val tp2 = typeOf(t2)
  							in 
  								if not(T.eq(tp1, tp2))
  							  	then raise TypeError "TypeError--Nand statement types do not match."
  								else tp1
  							end
  		| C.Succ(succ) => let 
  								val tp1 = typeOf(succ)
  							in
  								if tp1 = T.Nat then tp1
  								else raise TypeError "TypeError--Illegal Succ value."
   							end
   		| C.Pred(pred) => let 
  								val tp1 = typeOf(pred)
  							in
  								if tp1 = T.Nat then tp1
  								else raise TypeError "TypeError--Illegal Pred value."
   							end
   		| C.IsZero(is_zero) => let 
  								val tp1 = typeOf(is_zero)
  							in
  								if tp1 = T.Nat then T.Bool
  								else raise TypeError "TypeError--Illegal isZero value."
   							end

  fun check (t : C.term) : unit =
    let
      val ty = typeOf t (* might raise TypeError *)
    in
      ()
    end

end

