structure Parse : sig

  exception ParseError of string

(* nextTerm: Return the next term and the rest of the tokens.
 *       ex: nextTerm (Token.True::ts) => (SurfaceNBT.True, ts)
 *)
  val nextTerm : Token.token list -> SurfaceNBT.term * Token.token list

(* parse : Return the one expression comprising the program.
 *         The implementation is given, and just calls nextTerm.
 *         This is a so-called _recursive descent parser_.
 *)
  val parse    : Token.token list -> SurfaceNBT.term

end = struct

  exception ParseError of string

  structure T = Token
  structure S = SurfaceNBT

  fun nextTerm tokens = 
  	let 
  		val token::next_tokens = tokens
  	in
  		case token
  			of T.True => (S.True, next_tokens)
  			| T.False => (S.False, next_tokens)
  			| T.Nat(num) => (S.Nat(num), next_tokens)
  			| T.If => 
  						if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the if statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in if statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the if clause, returning the remaining terms *)
  									val (firstTerm, rem_tokens_1)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining terms after the first *)
  									if rem_tokens_1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val comma1::after_comma1 = rem_tokens_1
	  									in
  											(* Move past the first comma *)
  											if not(comma1 = T.Comma)then raise ParseError "Parse Error: Syntax error in if statement." else
  											if after_comma1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  												(let
  													(* Parse through the second term in the if clause, returning the remaining terms *)
  													val (secondTerm, rem_tokens_2) = nextTerm(after_comma1)
  												in
  													(* Make certain there are remaining terms after the second *)
  													if rem_tokens_2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  														(let
  															val comma2::after_comma2 = rem_tokens_2
  														in
	  														(* Move past the second comma *)
  															if not(comma2 = T.Comma)then raise ParseError "Parse Error: Syntax error in if statement." else
  															if after_comma2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  																(let
  																	(* Parse through the third term in the if clause, returning the remaining tokens *)
  																	val (thirdTerm, rem_tokens_3) = nextTerm(after_comma2)
  																in
  																	(* Make certain there is a rparen after the third term *)
  																	if rem_tokens_3 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  																		(let
  																			val rparen::nothing = rem_tokens_3
  																		in
  																			if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in if statement." else
  																			(S.If(firstTerm, secondTerm, thirdTerm), nothing)
  																		end)
  																end)
  														end)
	  											end)
  										end)
  								end)
  						end)
  			| T.Unless => 
  						if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the unless statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in unless statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the unless clause, returning the remaining terms *)
  									val (firstTerm, rem_tokens_1)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining terms after the first *)
  									if rem_tokens_1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val comma1::after_comma1 = rem_tokens_1
	  									in
  											(* Move past the first comma *)
  											if not(comma1 = T.Comma)then raise ParseError "Parse Error: Syntax error in unless statement." else
  											if after_comma1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  												(let
  													(* Parse through the second term in the unless clause, returning the remaining terms *)
  													val (secondTerm, rem_tokens_2) = nextTerm(after_comma1)
  												in
  													(* Make certain there are remaining terms after the second *)
  													if rem_tokens_2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  														(let
  															val comma2::after_comma2 = rem_tokens_2
  														in
	  														(* Move past the second comma *)
  															if not(comma2 = T.Comma)then raise ParseError "Parse Error: Syntax error in unless statement." else
  															if after_comma2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  																(let
  																	(* Parse through the third term in the unless clause, returning the remaining tokens *)
  																	val (thirdTerm, rem_tokens_3) = nextTerm(after_comma2)
  																in
  																	(* Make certain there is a rparen after the third term *)
  																	if rem_tokens_3 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  																		(let
  																			val rparen::nothing = rem_tokens_3
  																		in
  																			if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in unless statement." else
  																			(S.Unless(firstTerm, secondTerm, thirdTerm), nothing)
  																		end)
  																end)
  														end)
	  											end)
  										end)
  								end)
  						end)
  			| T.Or => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the or statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in or statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the or clause, returning the remaining terms *)
  									val (firstTerm, rem_tokens_1)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining terms after the first *)
  									if rem_tokens_1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val comma::after_comma = rem_tokens_1
	  									in
  											(* Move past the first comma *)
  											if not(comma = T.Comma)then raise ParseError "Parse Error: Syntax error in or statement." else
  											if after_comma = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  												(let
  													(* Parse through the second term in the or, returning the remaining tokens *)
  													val (secondTerm, rem_tokens_2) = nextTerm(after_comma)
  												in
  													(* Make certain there are remaining tokens after the second term *)
  													if rem_tokens_2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  														(let
  															val rparen::nothing = rem_tokens_2
  														in
  															if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in or statement." else
  															(S.Or(firstTerm, secondTerm), nothing)
  														end)
	  											end)
  										end)
  								end)
  						end)
  			| T.And => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the and statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in and statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the and clause, returning the remaining terms *)
  									val (firstTerm, rem_tokens_1)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining terms after the first *)
  									if rem_tokens_1 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val comma::after_comma = rem_tokens_1
	  									in
  											(* Move past the first comma *)
  											if not(comma = T.Comma)then raise ParseError "Parse Error: Syntax error in and statement." else
  											if after_comma = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  												(let
  													(* Parse through the second term in the and, returning the remaining tokens *)
  													val (secondTerm, rem_tokens_2) = nextTerm(after_comma)
  												in
  													(* Make certain there are remaining tokens after the second term *)
  													if rem_tokens_2 = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  														(let
  															val rparen::nothing = rem_tokens_2
  														in
  															if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in and statement." else
  															(S.And(firstTerm, secondTerm), nothing)
  														end)
	  											end)
  										end)
  								end)
  						end)
  			| T.Not => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the not statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in not statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the not clause, returning the remaining tokens *)
  									val (firstTerm, rem_tokens)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining tokens after the term *)
  									if rem_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val rparen::nothing = rem_tokens
  										in
  												if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in not statement." else
  												(S.Not(firstTerm), nothing)
  										end)
  								end)
  						end)
  			| T.Plus => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the succ statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in succ statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the succ clause, returning the remaining tokens *)
  									val (firstTerm, rem_tokens)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining tokens after the term *)
  									if rem_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val rparen::nothing = rem_tokens
  										in
  												if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in succ statement." else
  												(S.Succ(firstTerm), nothing)
  										end)
  								end)
  						end)
  			| T.Minus => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the pred statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in pred statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the pred clause, returning the remaining tokens *)
  									val (firstTerm, rem_tokens)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining tokens after the term *)
  									if rem_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val rparen::nothing = rem_tokens
  										in
  												if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in pred statement." else
  												(S.Pred(firstTerm), nothing)
  										end)
  								end)
  						end)
  			| T.QuestionMark => 
  					if next_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  						(let 
  							(* Check to make sure there's a "(" following the iszero statement *)
  							val left_paren::after_lparen = next_tokens
  						in
  							if not(left_paren = T.LParen) then raise ParseError "Parse Error: Syntax error in iszero statement." else
  							if after_lparen = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  								(let
	  								(* Parse through the first term in the iszero clause, returning the remaining tokens *)
  									val (firstTerm, rem_tokens)  = nextTerm(after_lparen)
  								in
  									(* Make certain there are remaining tokens after the term *)
  									if rem_tokens = nil then raise ParseError "Parse Error: not enough arguments for a multi-argument clause" else
  										(let
  											val rparen::nothing = rem_tokens
  										in
  												if not(rparen = T.RParen)then raise ParseError "Parse Error: Syntax error in iszero statement." else
  												(S.IsZero(firstTerm), nothing)
  										end)
  								end)
  						end)
  			| _ => raise ParseError "Parse Error: Received either an unknown token or a known token in an incorrect context."
  	end	

  fun parse tokens = 
   (case nextTerm tokens
     of (exp, []) => exp
      | (exp, _::_) => raise ParseError "extra tokens follow expression"
     (* end case *))

end
