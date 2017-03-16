structure Compile :
sig

(* We'll just compile from strings this time, as opposed to files, to enable more direct testing. *)
 val fromString : string -> TLC.term * TLC.ty

(* This compiles the code and prints the result in the REPL, for convenience. *)
val show : string -> unit

 end = struct

fun err label msg =
 (* Am I currying because I don't feel like typing parens? Yes I am. *)
  let
    val m = label ^ ": " ^ msg
  in
    (TextIO.print ("===>> " ^ m) ; TextIO.print "\n" ; raise Fail m)
  end

fun fromString code =
    let
        val tokens = Scan.scan code
        handle Scan.SyntaxError msg => err "syntax error" msg
        val ast = Parse.parse tokens
        handle Parse.ParseError msg => err "parse error" msg
        val _ = FV.check ast
        handle FV.OpenTerm fs => err "undefined variables" (String.concatWith "," fs)
        val ty = Typecheck.typeOf (Typecheck.empty, ast)
        handle Typecheck.TypeError msg => err "type error" msg
        val result = Eval.eval ast
        handle Eval.RuntimeError msg => err "runtime error" msg
        val _ = Check.assert (TLC.isVal result, "result is a value")
    in
        (result, ty)
    end

fun show code =
    let
        val (term, ty) = fromString code
        val s = TLC.unparseTerm term ^ " : " ^ TLC.unparseTy ty
    in
        (TextIO.print s ; TextIO.print "\n")
    end

end
