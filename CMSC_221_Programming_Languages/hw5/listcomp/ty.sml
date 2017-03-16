structure Ty = struct

  datatype ty
    = Nat
    | Bool
    | Tuple of ty list (* should never be fewer than two in a tuple *)
    | List of ty
    | Arrow of ty * ty

  fun tos Nat = "Nat"
    | tos Bool = "Bool"
    | tos (Tuple ts) = "(" ^ String.concatWith " * " (map tos ts) ^ ")"
    | tos (List t) = tos t ^ " List"
    | tos (Arrow (t1, t2)) = tos t1 ^ " -> " ^ tos t2

end
