structure SurfaceNBT : sig

  datatype term
    = True
    | False
    | If of term * term * term
    | Unless of term * term * term
    | Or of term * term
    | And of term * term
    | Not of term
    | Nat of int
    | Succ of term
    | Pred of term
    | IsZero of term

end = struct

  datatype term
    = True
    | False
    | If of term * term * term
    | Unless of term * term * term
    | Or of term * term
    | And of term * term
    | Not of term
    | Nat of int
    | Succ of term
    | Pred of term
    | IsZero of term

end
