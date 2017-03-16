structure Parse = struct

  structure A = AST
  structure T = Token

  fun app1  f (xs, ys) = (f xs, ys)

  fun cons1 x = app1 (fn xs => x::xs)

  fun nats (T.Nat(n)::T.Comma::ts) = cons1 n (nats ts)
    | nats (T.Nat(n)::T.RBrack::ts) = ([n], ts)
    | nats _ = raise Fail "parse error in nats"

  fun bools (T.True::T.Comma::ts)   = cons1 true  (bools ts)
    | bools (T.False::T.Comma::ts)  = cons1 false (bools ts) 
    | bools (T.True::T.RBrack::ts)  = ([true],  ts)
    | bools (T.False::T.RBrack::ts) = ([false], ts)
    | bools _ = raise Fail "parse error in bools"

  fun gen (T.LBrack::T.Nat(a)::T.DotDot::T.Nat(b)::T.RBrack::ts) = 
        (A.Range (a, b), ts)
    | gen (T.LBrack::(ns as T.Nat(n)::_))  = app1 A.Nats  (nats ns)
    | gen (T.LBrack::(bs as T.True::_))    = app1 A.Bools (bools bs)
    | gen (T.LBrack::(bs as T.False::_))   = app1 A.Bools (bools bs)
    | gen _ = raise Fail "error parsing gen"
						 
  fun gen_pred (T.ID(x)::T.LArrow::ts1) =
        let val (g, ts2) = gen ts1
	in  
	  (A.Gen (x, g), ts2)
	end
    | gen_pred (T.ID(pred)::T.ID(x)::ts1) = (A.Pred (pred, x), ts1)
    | gen_pred _ = raise Fail "gen_pred parse error"

  fun gens_preds toks =
   (case gen_pred toks
     of (gp, T.Comma::ts2) => cons1 gp (gens_preds ts2)
      | (gp, T.RBrack::ts2) => ([gp], ts2)
      | _ => raise Fail "gens_preds parse error")

  fun nextTuple toks =
    let
        fun item (T.ID x)  = A.ID x
	       | item (T.Nat n) = A.Nat n 
	       | item T.True    = A.Bool true
	       | item T.False   = A.Bool false
	       | item _         = raise Fail "parse error unexpected item in tuple"
        fun lp (t::T.Comma::ts) = cons1 (item t) (lp ts)
	       | lp (t::T.RParen::ts) = ([item t], ts)
	       | lp _ = raise Fail "parse error: malformed tuple"
    in
        case toks
        of T.ID(x)::T.Pipe::ts  => ([A.ID x], T.Pipe::ts)
            | T.Nat(n)::T.Pipe::ts => ([A.Nat n], T.Pipe::ts)
            | T.True::T.Pipe::ts   => ([A.Bool true], T.Pipe::ts)
            | T.False::T.Pipe::ts  => ([A.Bool false], T.Pipe::ts)
            | T.LParen::ts => lp ts
            | _ => raise Fail "parse error: unexpected token in list comprehension LHS"
    end

  fun list_comp toks = 
   (case toks
      of T.LBrack::ts =>
          (case nextTuple ts
	           of (tup, T.Pipe::ts1) =>
		          (case gens_preds ts1
		              of (gs, ts2) => (A.ListComp (tup, gs), ts2))
	                   | _ => raise Fail "parse error: expected pipe")
                | _ => raise Fail "parse error: expected left bracket")

  fun parse toks =
   (case list_comp toks
      of (lc as A.ListComp _, []) => lc
       | (_, _::_) => raise Fail "parse error: extra tokens after list comprehension")

end
