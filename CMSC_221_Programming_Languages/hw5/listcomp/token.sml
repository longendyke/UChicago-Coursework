structure Token = struct

  datatype token
    = LBrack
    | RBrack
    | LParen
    | RParen
    | ID of string
    | Nat of int
    | True
    | False
    | Pipe
    | Comma
    | LArrow
    | DotDot

  fun tos LBrack  = "LBrack"
    | tos RBrack  = "RBrack"
    | tos LParen  = "LParen"
    | tos RParen  = "RParen"
    | tos (ID s)  = "ID(" ^ s ^ ")"
    | tos (Nat n) = "Nat(" ^ Int.toString n ^ ")"
    | tos True    = "True"
    | tos False   = "False"
    | tos Pipe    = "Pipe"
    | tos Comma   = "Comma"
    | tos LArrow  = "LArrow"
    | tos DotDot  = "DotDot"

  fun eq (t1:token,t2) = (t1=t2)

end
