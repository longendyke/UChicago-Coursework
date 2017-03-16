structure Token :sig

datatype token
    = True
    | False
    | Not
    | If
    | ID of string
    | Comma
    | Lambda
    | Colon
    | Bool
    | Arrow
    | Dot
    | LParen
    | RParen

val tos :token -> string

      end = struct

datatype token
    = True
    | False
    | Not
    | If
    | ID of string
    | Comma
    | Lambda
    | Colon
    | Bool
    | Arrow
    | Dot
    | LParen
    | RParen

fun tos True = "True"
    | tos False = "False"
    | tos Not = "Not"
    | tos If = "If"
    | tos (ID s) = "ID(" ^ s ^ ")"
    | tos Comma = "Comma"
    | tos Lambda = "Lambda"
    | tos Colon = "Colon"
    | tos Bool = "Bool"
    | tos Arrow = "Arrow"
    | tos Dot = "Dot"
    | tos LParen = "LParen"
    | tos RParen = "RParen"

end
