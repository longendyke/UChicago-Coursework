structure Parse : sig

  exception ParseError of string

  val parse : Token.token list -> TLC.term

end = struct

  exception ParseError of string

  structure T = Token
  structure L = TLC
  structure V = Var

  (* Little function to determine the type of a term so I don't have to for every one!*)
  fun type_helper ( tokens : T.token list ) : L.ty * T.token list =
    let
        val curr_tok::next = tokens
    in
        case curr_tok
            of T.Bool => (L.Bool, next)
            | T.LParen => 
                if next = nil then raise ParseError "ParseError: Invalid type argument."
                else
                    (let
                        val (ty1, af_t1) = type_helper(next)
                    in
                        if af_t1 = nil then raise ParseError "ParseError: Invalid type syntax."
                        else
                            (let
                                val arrow::af_arrow = af_t1
                            in
                                if not(arrow = T.Arrow) then raise ParseError "ParseError: Invalid type syntax."
                                else if af_arrow = nil then raise ParseError "ParseError: Invalid type syntax."
                                else
                                    (let
                                        val (ty2, af_ty2) = type_helper(af_arrow)
                                    in
                                        if af_ty2 = nil then raise ParseError "ParseError: Invalid type syntax."
                                        else
                                            (let
                                                val rparen::non_ty = af_ty2
                                            in
                                                if not(rparen = T.RParen) then raise ParseError "ParseError: Invalid type syntax."
                                                else
                                                    (L.Arrow(ty1, ty2), non_ty)
                                            end)
                                    end)
                            end)
                    end)
            | _ => raise ParseError "ParseError: Invalid type syntax."
    end

  fun nextTerm (tokens : T.token list ) : L.term * T.token list = 
  	let
  		val curr_tok::next_toks = tokens
  	in
  		(case curr_tok
  			of T.True => (L.True, next_toks)
            | T.False => (L.False, next_toks)
            | T.Not => (L.Not, next_toks)
            | T.LParen => if next_toks = nil then raise ParseError "ParseError: Encountered out-of-place token." else lparen_handler(next_toks)
            | (T.ID x) => (L.Var(x), next_toks)
            | _ => raise ParseError "ParseError: Encountered out-of-place token."
        )
  	end
  and lparen_handler ( function : T.token list ) : L.term * T.token list =
    let
        val current::next = function
    in
        case current
            of T.Lambda =>
                (if next = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                else 
                    (let
                        val (var, af_var) = nextTerm(next)
                        val (L.Var variable) = var
                    in
                        (*if not(var = (L.Var (V.var x))) then raise ParseError "ParseError: Incorrect syntax in Lambda-term." *)
                        if af_var = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                        else
                            (let
                                val colon::af_colon = af_var
                            in
                                if not(colon = T.Colon) then raise ParseError "ParseError: Incorrect syntax in Lambda-term." 
                                else if af_colon = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                else
                                    (let
                                        val (typ, af_type) = type_helper(af_colon)
                                    in
                                        if af_type = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                        else
                                            (let
                                                val dot::af_dot = af_type
                                            in
                                                if not(dot = T.Dot) then raise ParseError "ParseError: Incorrect syntax in Lambda-term." 
                                                else if af_dot = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                                else
                                                    (let
                                                        val (lambda_term, after_lt) = nextTerm(af_dot)
                                                    in
                                                        if after_lt = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                                        else
                                                            (let
                                                                val rparen::nothing = after_lt
                                                            in
                                                                if not(rparen = T.RParen) then raise ParseError "ParseError: Incorrect syntax in Lambda-term."
                                                                else
                                                                    (L.Abs(variable, typ, lambda_term), nothing)
                                                            end)
                                                    end)
                                            end)
                                    end)
                            end)

                    end)
                )
            | T.If =>
                if next = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                else
                (let
                    val (cond, af_cond) = nextTerm(next)
                in
                    if af_cond = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                    else
                        (let
                            val (t1, af_t1) = nextTerm(af_cond)
                        in
                            if af_t1 = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                            else
                                (let
                                    val (t2, af_t2) = nextTerm(af_t1)
                                in
                                    if af_t2 = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                    else
                                        (let
                                            val rparen::nothing = af_t2
                                        in
                                            if not(rparen = T.RParen) then raise ParseError "ParseError: Incorrect If syntax."
                                            else
                                                (L.If(cond, t1, t2), nothing)
                                        end)
                                end)
                        end)
                end)
            (* If we get here, the only remaining option is for an application! *)
            | _ => 
                    (let
                        val (t1, af_t1) = nextTerm(function)
                    in
                        if af_t1 = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                        else
                            (let
                                val (t2, af_t2) = nextTerm(af_t1)
                            in
                                if af_t2 = nil then raise ParseError "ParseError: Not enough arguments for multi-argument clause."
                                else
                                    (let
                                        val rparen::nothing = af_t2
                                    in
                                        if not(rparen = T.RParen) then raise ParseError "ParseError: Incorrect application syntax."
                                        else
                                            (L.App(t1, t2), nothing)
                                    end)
                            end)
                    end)

    end


  fun parse tokens = 
   (case nextTerm tokens
     of (exp, []) => exp
      | (exp, _::_) => raise ParseError "extra tokens follow expression"
     (* end case *))


end
