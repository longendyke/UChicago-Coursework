structure Token = struct

  datatype token 
    = LParen
    | RParen
    | LCurly
    | RCurly
    | Star
    | Arrow
    | Comma
    | Colon
    | Nat
    | Bool
    | Ref
    | List
    | Array
    | Top
    | Label of string 

  fun tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos LCurly = "LCurly"
    | tos RCurly = "RCurly"
    | tos Star   = "Star"
    | tos Arrow  = "Arrow"
    | tos Comma  = "Comma"
    | tos Colon  = "Colon"
    | tos Nat    = "Nat"
    | tos Bool   = "Bool"
    | tos Ref    = "Ref"
    | tos List   = "List"
    | tos Array  = "Array"
    | tos Top    = "Top"
    | tos (Label s) = "Label("^s^")"

end
