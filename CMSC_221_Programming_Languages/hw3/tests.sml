structure Tests = struct

  structure T = Token
  structure TY = Type
  structure S = SurfaceNBT
  structure C = CoreNBT

  val tests = 
    [Check.expect (1+1, 2, "t00"),

    (* Scan Tests *)
    Check.expect (Scan.scan("if(t, t, f)"), [T.If, T.LParen, T.True, T.Comma, T.True, T.Comma, T.False, T.RParen], "scan01"),
    Check.expect (Scan.scan("unless(t, f, f)"), [T.Unless, T.LParen, T.True, T.Comma, T.False, T.Comma, T.False, T.RParen], "scan02"),
    Check.expect (Scan.scan("or(t, f, -(+(0)))"), [T.Or, T.LParen, T.True, T.Comma, T.False, T.Comma, T.Minus, T.LParen, T.Plus, T.LParen, T.Nat(0), T.RParen, T.RParen, T.RParen], "scan03"),
    Check.expect (Scan.scan("and(?(12345), not(f))"), [T.And, T.LParen, T.QuestionMark, T.LParen, T.Nat(12345), T.RParen, T.Comma, T.Not, T.LParen, T.False, T.RParen, T.RParen], "scan04"),

    Check.error ((fn _ => Scan.scan(" n o t")), "scan05"),
    Check.error ((fn _ => Scan.scan(" adn(t, f)")), "scan06"),

    (* Parse Tests *)
    Check.expect (Parse.parse([T.And, T.LParen, T.True, T.Comma, T.False, T.RParen]), S.And(S.True, S.False), "parse01"),
    Check.expect (Parse.parse([T.Or, T.LParen, T.QuestionMark, T.LParen, T.Nat(42), T.RParen, T.Comma, T.False, T.RParen]), S.Or(S.IsZero(S.Nat(42)), S.False), "parse02"),
    Check.expect (Parse.parse([T.If, T.LParen, T.Not, T.LParen, T.True, T.RParen, T.Comma, T.QuestionMark, T.LParen, T.Minus, T.LParen, T.Plus, T.LParen, T.Nat(0), T.RParen, T.RParen, T.RParen, T.Comma, T.True, T.RParen]), 
    	S.If(S.Not(S.True), S.IsZero(S.Pred(S.Succ(S.Nat(0)))), S.True), "parse03"),
    Check.expect (Parse.parse([T.Unless, T.LParen, T.True, T.Comma, T.False, T.Comma, T.False, T.RParen]), S.Unless(S.True, S.False, S.False), "parse04"),

    Check.error ((fn _ => Parse.parse([T.And, T.LParen, T.True, T.Comma, T.Comma, T.False, T.RParen])), "parse05"),
    Check.error ((fn _ => Parse.parse([T.LParen, T.True, T.LParen])), "parse06"),
    Check.error ((fn _ => Parse.parse([T.If, T.LParen, T.If, T.RParen, T.LParen, T.True, T.Comma, T.False, T.RParen])), "parse07"),

    (* Elaborator Tests *)
    Check.expect (Elaborate.elab(S.Nat(4)), C.Succ(C.Succ(C.Succ(C.Succ(C.Zero)))), "elab01"),
    (* Not checking the rest as they're pretty obvious... *)

    (* TypeChecker Tests *)
    Check.expect(Typecheck.check(C.If(C.True, C.False, C.True)), (), "tcheck01"),
    Check.expect(Typecheck.check(C.Nand(C.IsZero(C.Zero), C.True)), (), "tcheck02"),
    Check.expect(Typecheck.check(C.Pred(C.Succ(C.If(C.True, C.Zero, C.Pred(C.Zero))))), (), "tcheck03"),

    Check.error((fn _ => Typecheck.check(C.If(C.True, C.False, C.Zero))), "tpcheck04"),
    Check.error((fn _ => Typecheck.check(C.Nand(C.IsZero(C.Zero), C.Pred(C.Zero)))), "tpcheck05"),
    Check.error((fn _ => Typecheck.check(C.Pred(C.True))), "tpcheck06"),
    Check.error((fn _ => Typecheck.check(C.IsZero(C.True))), "tpcheck07"),
    Check.error((fn _ => Typecheck.check(C.Succ(C.True))), "tpcheck08"),

    (* Eval Check *)
    Check.expect(Evaluate.eval(C.Pred(C.If(C.True, C.If(C.IsZero(C.Pred(C.Succ(C.Zero))), C.Succ(C.Zero), C.Zero), C.Succ(C.Succ(C.Zero))))), C.Zero, "eval01"),
    Check.expect(Evaluate.eval(C.Nand(C.If(C.True, C.False, C.True), C.IsZero(C.Pred(C.Succ(C.Zero))))), C.True, "eval02")

    ]

  val n = List.length tests

  val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ 
	    (if n=1 then "" else "s") ^ " passed!\n"

  val _ = TextIO.print msg

end
