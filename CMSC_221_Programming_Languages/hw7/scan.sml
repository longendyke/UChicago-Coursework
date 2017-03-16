structure Scan = struct

  structure T = Token

(* gather splits list by predicate; it's a sort of "take while" *)
(* ex: gather even [2,4,6,7,8] --> ([2,4,6],[7,8]) *)
(* ex: gather odd  [2,4,6,7,8] --> ([],[2,4,6,7,8]) *)
  fun gather test xs = 
    let
      fun lp ([], acc) = (List.rev acc, [])
	| lp (x::xs, acc) = if test x then lp (xs, x::acc) else (List.rev acc, x::xs)
    in 
      lp (xs, [])
    end

  fun scan s = 
    let
      fun lp [] = []
	| lp (#"("::cs) = T.LParen::lp(cs)
	| lp (#")"::cs) = T.RParen::lp(cs)
	| lp (#"{"::cs) = T.LCurly::lp(cs)
	| lp (#"}"::cs) = T.RCurly::lp(cs)
	| lp (#"*"::cs) = T.Star::lp(cs)
	| lp (#"-":: #">"::cs) = T.Arrow::lp(cs)
	| lp (#","::cs) = T.Comma::lp(cs)
	| lp (#":"::cs) = T.Colon::lp(cs)
	| lp (chars as c::cs) = 
            if Char.isSpace c then lp cs
	    else if Char.isUpper c then upper chars
	    else if Char.isLower c then lower chars
	    else raise Fail ("unexpected character while scanning " ^ (implode chars) ^ "\n")
      and upper cs = 
       (case cs
	  of (#"N":: #"a":: #"t":: cs) => T.Nat::lp(cs)
	   | (#"B":: #"o":: #"o":: #"l":: cs) => T.Bool::lp(cs)
	   | (#"R":: #"e":: #"f":: cs) => T.Ref::lp(cs)
	   | (#"L":: #"i":: #"s":: #"t":: cs) => T.List::lp(cs)
	   | (#"A":: #"r":: #"r":: #"a":: #"y":: cs) => T.Array::lp(cs)
	   | (#"T":: #"o":: #"p":: cs) => T.Top::lp(cs)
	   | _ => raise Fail ("scan error at " ^ (implode cs))
         (* end case *))
      and lower cs = 
       (case gather Char.isLower cs
	  of ([], _) => raise Fail "BUG scanning label"
	   | (cs, cs') => T.Label(implode cs)::lp(cs')
         (* end case *))
    in
      lp (explode s)
    end

end
