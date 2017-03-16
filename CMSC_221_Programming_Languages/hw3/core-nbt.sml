structure CoreNBT : sig

  datatype term
    = True
    | False
    | If of term * term * term
    | Nand of term * term
    | Zero
    | Succ of term
    | Pred of term
    | IsZero of term

  val tos      : term -> string
  val isNumVal : term -> bool
  val isBoolVal : term -> bool
  val isVal    : term -> bool
  val eq       : term * term -> bool

end = struct

  datatype term
    = True
    | False
    | If of term * term * term
    | Nand of term * term
    | Zero
    | Succ of term
    | Pred of term
    | IsZero of term

  fun tos True = "true"
    | tos False = "false"
    | tos (If (t1, t2, t3)) = "if(" ^ tos t1 ^ "," ^ tos t2 ^ "," ^ tos t3 ^ ")"
    | tos (Nand (t1, t2)) = "nand(" ^ tos t1 ^ "," ^ tos t2 ^ ")"
    | tos Zero = "0"
    | tos (Succ t) = "succ(" ^ tos t ^ ")"
    | tos (Pred t) = "pred(" ^ tos t ^ ")"
    | tos (IsZero t) = "iszero(" ^ tos t ^ ")"

  fun isNumVal Zero = true
    | isNumVal (Succ nv) = isNumVal nv
    | isNumVal _ = false

  fun isBoolVal True = true
    | isBoolVal False = true
    | isBoolVal _ = false

  fun isVal True = true
    | isVal False = true
    | isVal t = isNumVal t

  fun eq (t1:term, t2) = (t1=t2)

end
