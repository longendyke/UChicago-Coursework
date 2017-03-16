structure Type : sig

  datatype ty (* Note: we can't use the name "type" because it's reserved in SML. *)
    = Bool
    | Nat

  val tos : ty -> string
  val eq  : ty * ty -> bool

end = struct

  datatype ty
    = Bool
    | Nat

  fun tos Bool = "Bool"
    | tos Nat  = "Nat"

  fun eq (t:ty, u:ty) = (t=u)

end
