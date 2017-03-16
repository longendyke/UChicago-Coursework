signature NB_SIG = sig

  datatype term
    = True
    | False
    | If of term * term * term 
    | Zero      
    | Succ of term
    | Pred of term
    | IsZero of term

(* eq: return true if two terms are the same. *)
  val eq  : term * term -> bool

(* tos: compute a string representation of a term. Make the terms
 * look like they do in the text: e.g., "if true then true else false".
 *)
  val tos : term -> string

(* isVal: return true if a term is a value according to Figures 3-1 and 3-2. *)
  val isVal : term -> bool

(* isNumVal: return true if a term is a numeric value according to Fig 3-2. *)
  val isNumVal : term -> bool

(* isNumVal: return true if a term is a boolean value according to Fig 3-1. *)
  val isBoolVal : term -> bool

end
