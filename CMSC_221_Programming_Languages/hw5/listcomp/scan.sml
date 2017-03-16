structure Scan = struct

  structure T = Token

  fun d c = ord c - ord #"0"

  fun nextNat (acc, []) = (T.Nat acc, [])
    | nextNat (acc, c::cs) = 
        if Char.isDigit c 
	  then nextNat (acc*10+(d c), cs)
	  else (T.Nat acc, c::cs)

  fun nextID (acc, []) = (T.ID (implode (rev acc)), [])
    | nextID (acc, c::cs) = 
        if Char.isLower c
          then nextID (c::acc, cs)
        else (T.ID (implode (rev acc)), c::cs)

  fun next cs =
   (case cs
      of #"["::cs' => (T.LBrack, cs')
       | #"]"::cs' => (T.RBrack, cs')
       | #"("::cs' => (T.LParen, cs')
       | #")"::cs' => (T.RParen, cs')
       | #"t":: #"r":: #"u":: #"e"::cs' => (T.True, cs')
       | #"f":: #"a":: #"l":: #"s":: #"e"::cs' => (T.False, cs')
       | #"|"::cs' => (T.Pipe, cs')
       | #","::cs' => (T.Comma, cs')
       | #"<":: #"-"::cs' => (T.LArrow, cs')
       | #".":: #"."::cs' => (T.DotDot, cs')
       | c::cs' =>
        if Char.isDigit c then nextNat (d c, cs')
        else if Char.isLower c then nextID ([c], cs')
        else if Char.isSpace c then next cs'
        else raise Fail ("scan error at " ^ (implode cs))
       | [] => raise Fail "scan error: unexpected end"
   (* end case *))

  fun scan s = 
    let 
      fun lp [] = []
	| lp cs = munge (next cs)
      and munge (tok, cs) = tok :: lp cs
    in
      lp (explode s)
    end
 
end
