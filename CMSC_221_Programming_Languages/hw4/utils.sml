structure Utils = struct

    structure T = Token

fun var_return ( input : char list ) : char list * char list =
    if input = nil then (nil, nil) else
    let
        val hd::tl = input
    in
        (* Check to see if the next character is any of the chars
           associated with other terms: spaces, L/R-Parens, colons, or 
            exclamation points. If not, then we're still within the variable *)
        if Char.isSpace(hd) then (nil, tl) else 
        if hd = #"!" then (nil, input) else 
        if hd = #"(" then (nil, input) else 
        if hd = #")" then (nil, input) else 
        if hd = #":" then (nil, input) else 
            let 
                val (next, rem) = var_return(tl)  
            in
                (hd::next, rem) 
            end
    end

 fun variable_handler ( input : string ) : T.token * string =
    let
        val exploded = explode(input)
        val (variable, next_chars) = var_return(exploded)
        val var = implode(variable)
        val return = implode(next_chars)
    in
        (T.ID(var), return)
    end

end
