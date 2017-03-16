structure FV : sig

(* FV stands for "free variables." *)

  exception OpenTerm of Var.var list

(* See the definition of FV at 5.3.2. *)
(* Make sure you've looked at Var carefully before attempting. *)
  val fv : TLC.term -> Var.var_set

(* A term is closed if it contains no free variables...*)
  val closedTerm : TLC.term -> bool

(* ...otherwise it's open. *)
(* If it is an open term, return SOME list of its free variables. *)
(* If it's not open, return NONE. *)
  val openTerm : TLC.term -> (Var.var list) option

(* This one is already completed for you. *)
  val check : TLC.term -> unit

end = struct

  structure L = TLC
  structure V = Var

  exception OpenTerm of Var.var list


(* Following the rules in the book... (ie 5.3.2) *)
  fun fv ( term : TLC.term ) : Var.var_set = 
    case term
        of L.Var(x) => Var.singleton(x)
        | L.Abs(x, middle, next_term) => Var.remove(x, fv(next_term))
        | L.App(t1, t2) => Var.union(fv(t1), fv(t2))
        | L.If(cond, t1, t2) => Var.union(fv(cond), Var.union(fv(t1), fv(t2)))
        | _ => nil

  fun closedTerm ( term : TLC.term ) : bool = Var.isEmpty(fv(term))

  fun openTerm ( term : TLC.term ) = 
    if closedTerm(term) then NONE else SOME(Var.asList(fv(term)))

  fun check t = 
   (case openTerm t
     of SOME fs => raise OpenTerm fs
      | NONE => ())

end
