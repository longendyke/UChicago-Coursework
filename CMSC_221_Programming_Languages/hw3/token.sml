structure Token : sig

  datatype token 
    = LParen
    | RParen
    | Comma
    | True
    | False
    | If
    | Unless
    | Or
    | And
    | Not
    | Nat of int
    | Plus
    | Minus
    | QuestionMark

  val tos : token -> string
  val eq  : token * token -> bool

end = struct

  datatype token 
    = LParen
    | RParen
    | Comma
    | True
    | False
    | If
    | Unless
    | Or
    | And
    | Not
    | Nat of int
    | Plus
    | Minus
    | QuestionMark

  fun tos tok = (* NOTE: only implement this if you decide you need it *)
                (* (mostly useful for error reporting) *) 
    raise Fail "TODO: Token.tos" 

  fun eq (t:token, u) = (t=u)

end
