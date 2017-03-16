structure Parse = struct

  structure T = Token

  fun catToks toks = String.concatWith " " (List.map T.tos toks)

  fun toksError toks = "parse error: " ^ (catToks toks)

  (* Checks if a label is repeated in a record list. If so, return true, otherwise false *)
  fun rec_check (lbl : string, rec_list : Ty.record_item list) : bool =
    case rec_list
        of nil => false
        | h::t => 
            let
                val Ty.RI(chk, ty) = h
            in
                if lbl = chk then true else rec_check(lbl, t)
            end
            

(* Check that all labels in a list of record items are pairwise distinct.  *)
(* If all labels are mutually distinct, return unit. Otherwise raise Fail. *)
  fun chkDistinctLabels items : unit = 
    case items
        of nil => ()
        | h::t => 
            let
                 val Ty.RI(lbl, ty) = h
             in
                 if rec_check(lbl, t) then raise Fail "ERROR: labels not distinct!"
                 else chkDistinctLabels t
             end 


  fun next [] = raise Fail "BUG" (* should never get here *)
    | next toks =
       (case toks
	  of T.Nat::ts  => (Ty.Nat,ts)
	   | T.Bool::ts => (Ty.Bool,ts)
	   | T.Top::ts  => (Ty.Top,ts)
	   | T.LParen::T.Star::ts  => next2 (Ty.Pair,"pair type") ts
	   | T.LParen::T.Arrow::ts => next2 (Ty.Arrow,"function type") ts
	   | T.LParen::T.Ref::ts   => next1 (Ty.Ref,"ref type") ts
	   | T.LParen::T.List::ts  => next1 (Ty.List, "list type") ts
           | T.LParen::T.Array::ts => next1 (Ty.Array,"array type") ts
	   | T.LCurly::ts => record ts
	   | _ => raise Fail (toksError toks))
  and next1 (constructor, msg) ts = 
       (case next ts
	  of (ty1, T.RParen::ts1) => (constructor(ty1),ts1)
	   | _ => raise Fail ("parse error: " ^ msg))
  and next2 (constructor, msg) ts =
        let
          val (ty1, ts1) = next ts
	  val (ty2, ts2) = next ts1
	in
          case ts2
	    of T.RParen::ts3 => (constructor(ty1,ty2),ts3)
	     | _ => raise Fail ("parse error: " ^ msg)
	end
  and record ts = 
        let
          fun lp (T.Label(l)::T.Colon::ts, acc) = 
	       (case next ts
		  of (ty1, T.Comma::ts1) => lp (ts1, Ty.RI(l,ty1)::acc)
		   | (ty1, T.RCurly::ts1) => 
                      (case List.rev (Ty.RI(l,ty1)::acc)
			 of items as ri::ris =>
                            let
                              val _ = chkDistinctLabels items 
			    in
		              (Ty.Record (ri, ris), ts1)
			    end
			  | [] => raise Fail "parse error of record type (1)")
		   | _ => raise Fail "parse error of record type (2)")
	    | lp _ = raise Fail "parse error of record type (3)"
	in
          lp (ts, [])
	end

  fun parse toks = 
   (case next toks
      of (ty, []) => ty
       | (ty, extra) => 
	   raise Fail ("extra tokens after type " ^
		       Ty.unparse ty ^ ": " ^
		       (catToks extra)))

end
