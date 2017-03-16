structure Scan : sig

  exception SyntaxError of string

  val scan : string -> Token.token list

end = struct

  exception SyntaxError of string

  structure T = Token

  fun scan ( input : string ) : T.token list = 
    (* Sanity check: if given an empty string, return an empty list *)
    if String.size(input) = 0 then nil
    else

    let
        val exploded = explode(input)
    in
        let 
            val curr_char::remaining = exploded
            val next_string = implode(remaining)
        in
            if Char.isSpace(curr_char) then scan(next_string) else
            (* If we encounter a lower case character (a variable) then we go to a special
               handler which extracts the variable from the input string. *) 
            if Char.isLower(curr_char) then 
                let 
                    val (variable, remaining) = Utils.variable_handler(input)
                in
                    variable::scan(remaining)
                end
            else
            (case curr_char
            of #"T" => T.True::scan(next_string)
            | #"F" => T.False::scan(next_string)
            | #"!" => T.Not::scan(next_string)
            | #"(" => T.LParen::scan(next_string)
            | #")" => T.RParen::scan(next_string)
            | #"." => T.Dot::scan(next_string)
            | #":" => T.Colon::scan(next_string)
            | #"B" => T.Bool::scan(next_string)
            | _ =>   (case exploded
                        of #"I"::(#"f"::tl) => T.If::scan(implode(tl))
                        | #"L"::(#"/"::tl) => T.Lambda::scan(implode(tl))
                        | #"-"::(#">"::tl) => T.Arrow::scan(implode(tl))
                        | _ => raise SyntaxError "SyntaxError: Encountered unknown (non-variable) string."
                        )
            )
        end
    end 

end
