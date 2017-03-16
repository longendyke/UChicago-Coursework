structure Compile : sig

(* compile: Put all the pieces together! 
 *          The implementation is given.
 *)
  val compile : string -> CoreNBT.term

end = struct

  structure C = CoreNBT

  fun println s = TextIO.print (s ^ "\n")

  fun compile (filename : string) : C.term = 
    let
      val code    = Read.file filename
      val toks    = Scan.scan code
	  handle Scan.ScanError s =>
		 (println ("### scan error: " ^ s);
		  raise Scan.ScanError s)
      val ast     = Parse.parse toks
	  handle Parse.ParseError s =>
		 (println ("### parse error: " ^ s);
		  raise Parse.ParseError s)
      val ast'    = Elaborate.elab ast
      val _       = Typecheck.check ast' 
          (* Note: we do nothing with the type value, we just check it. *)
	  handle Typecheck.TypeError m => 
		 (println ("### type error: " ^ m);
		  raise Typecheck.TypeError m)
      val result  = Evaluate.eval ast'
      val _       = Check.assert (C.isVal result, "result is a value")
    in
      result
    end

end
