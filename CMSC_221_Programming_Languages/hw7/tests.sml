structure Tests = struct

  val fromString = Parse.parse o Scan.scan

  fun $$ s1 s2 = (fromString s1, fromString s2)

(* Check scanner and parser ("SP") *)
  fun chkSP string ty = Check.expect (fromString string, ty, string)

(* Check truth/falsity of subtyping relation on selected pairs *)
  fun chkT s1 s2 msg = Check.expect (Ty.subtype ($$ s1 s2), true, msg)
  fun chkF s1 s2 msg = Check.expect (Ty.subtype ($$ s1 s2), not true, msg)

(* some convenient abbreviations *)
  val tyN = Ty.Nat
  val tyB = Ty.Bool
  val ~*  = Ty.Pair
  val ~>  = Ty.Arrow

(* a convenience function to construct record types *)
  fun recTy ((label,ty)::ris) = Ty.Record (Ty.RI (label,ty), List.map Ty.RI ris)
    | recTy _ = raise Fail "empty"

  val tests = [
(* testing the front end *)
(* - I left these in. n.b. I wouldn't just give you a scanner and parser without testing them! *)
    chkSP "Nat" tyN,
    chkSP "Bool" tyB,
    chkSP "(* Nat Bool)" (~* (tyN, tyB)),
    chkSP "(* (* Nat Nat) Bool)" (~* (~* (tyN, tyN), tyB)),
    chkSP "(* Bool (* Nat Nat))" (~* (tyB, ~* (tyN, tyN))),
    chkSP "(-> Nat Bool)" (~> (tyN, tyB)),
    chkSP "(-> (* Nat Bool) Bool)" (~> (~* (tyN, tyB), tyB)),
    chkSP "(Ref Nat)" (Ty.Ref tyN),
    chkSP "(List Nat)" (Ty.List tyN),
    chkSP "(Array Nat)" (Ty.Array tyN),
    chkSP "(Array (List (Ref Nat)))" (Ty.Array (Ty.List (Ty.Ref tyN))),
    chkSP "{a:Bool}" (recTy [("a",tyB)]),
    chkSP "{a:Bool,b:Nat}" (recTy [("a",tyB),("b",tyN)]),
    chkSP "{a:{b:Bool}}" (recTy [("a",recTy[("b",tyB)])]),
    chkSP "{abc:(-> Bool Nat),def:(* Nat Bool),ghi:(Array (List Nat))}"
          (recTy [("abc",~>(tyB,tyN)),("def",~*(tyN,tyB)),("ghi",Ty.Array(Ty.List tyN))]),
(* testing the subtype relation *)
(* - I left in some of my own tests to get the ball rolling. *)

    chkT "Nat" "Nat" "subty00",
    chkF "Nat" "Bool" "subty01",
    chkT "{a:Bool,b:Nat}" "{a:Bool}" "subty02",
    chkF "{a:Bool}" "{a:Bool,b:Nat}" "subty03",
    chkF "{a:Bool,b:Nat}" "{a:Nat}" "subty04",
    chkT "{a:Bool}" "{a:Top}" "subty05",
    chkF "{a:Top}" "{a:Bool}" "subty06",
    chkT "{a:{aa:Nat}}" "{a:{aa:Nat}}" "subty07",
    chkT "{a:{aa:Nat, ab:Bool}}" "{a:{aa:Nat}}" "subty08",
    chkT "{a:Nat,b:Bool,c:Top,d:Bool,e:{aa:Bool, bb:Nat}}" 
         "{c:Top,b:Bool,e:{bb:Nat,aa:Bool}, a:Nat}" "subty09"

  ]

  val n = List.length tests

  val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ 
	    (if n=1 then "" else "s") ^ " passed!\n"

  val _ = TextIO.print (if n>0 then msg else "###### no tests run\n")

end
