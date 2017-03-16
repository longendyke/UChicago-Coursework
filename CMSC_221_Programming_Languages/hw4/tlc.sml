structure TLC : sig

  datatype ty
    = Bool
    | Arrow of ty * ty

  datatype term
    = Var of Var.var
    | Abs of Var.var * ty * term
    | App of term * term 
    | True
    | False
    | Not
    | If of term * term * term

  val unparseTy : ty -> string
  val unparseTerm : term -> string
  val isVal : term -> bool

end = struct

  datatype ty
    = Bool
    | Arrow of ty * ty

  datatype term
    = Var of Var.var
    | Abs of Var.var * ty * term (* abstractions, a.k.a functions *)
    | App of term * term         (* applications *)
    | True
    | False
    | Not
    | If of term * term * term

  fun unparseTy Bool = "B"
    | unparseTy (Arrow (t1, t2)) = "(" ^ unparseTy t1 ^ " -> " ^ unparseTy t2 ^ ")"

  fun unparseTerm tm = 
    let
        fun term (Var x) = x
            | term (Abs (x, t, b)) = "(L/ " ^ x ^ " : " ^ unparseTy t ^ " . " ^ term b ^ ")"
            | term (App (t1, t2)) = "(" ^ term t1 ^ " " ^ term t2 ^ ")"
	        | term True = "T"
            | term False = "F"
            | term Not = "!"
            | term (If (t1, t2, t3)) = "(If " ^ term t1 ^ " " ^ term t2 ^ " " ^ term t3 ^ ")"
    in
      term tm
    end

  fun isVal True = true
    | isVal False = true
    | isVal Not = true
    | isVal (Abs _) = true
    | isVal _ = false

end
