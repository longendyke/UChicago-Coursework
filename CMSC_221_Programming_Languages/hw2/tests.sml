structure Tests = struct

(* This datatype declaration allows us to type True instead of NB.True,
 * False instead of NB.False, etc.
 *)
  datatype term = datatype NB.term

(* Some renamings for convenience *)
  val step1  = SmallStep1.oneStep
  val steps1 = SmallStep1.steps
  val eval1  = SmallStep1.eval

  val step2  = SmallStep2.oneStep
  val steps2 = SmallStep2.steps
  val eval2  = SmallStep2.eval

  val tests = 
   [Check.expect (1+1, 2, "t00"),
    Check.expect (True, True, "t01"),

    (*nb.sml Tests*)
    Check.expect (NB.eq(True, True), true, "t02"),
    Check.expect (NB.eq(Succ(Succ(Zero)), Succ(Pred(Zero))), false, "t03"),

    Check.expect (NB.tos(Succ(Pred(IsZero(True)))), "Succ(Pred(IsZero(True)))", "t04"),
    Check.expect (NB.tos(If(True, False, Succ(Pred(IsZero(True))))), "If True then False else Succ(Pred(IsZero(True)))", "t05"),

    Check.expect (NB.isNumVal(Succ(Succ(Succ(Succ(Zero))))), true, "t06"),
    Check.expect (NB.isNumVal(Succ(Succ(Succ(Pred(Zero))))), false, "t07"),
    Check.expect (NB.isNumVal(Succ(Succ(Succ(Succ(False))))), false, "t08"),
    Check.expect (NB.isNumVal(Succ(Succ(Succ(Succ(If(True, Zero, False)))))), false, "t09"),

    Check.expect (NB.isBoolVal(True), true, "t10"),
    Check.expect (NB.isBoolVal(Zero), false, "t11"),

    Check.expect (NB.isVal(Succ(Succ(Succ(Succ(Zero))))), true, "t12"),
    Check.expect (NB.isVal(Zero), true, "t13"),
    Check.expect (NB.isVal(If(True, False, False)), false, "t14"),

    (* small-step-1 Tests*)
    Check.expect (valOf(SmallStep1.oneStep(If(If(If(True, False, False), True, True), False, False))), If(If(False, True, True), False, False), "t15"),
    Check.expect (isSome(SmallStep1.oneStep(If(IsZero(True), False, False))), false, "t16"),
    Check.expect (isSome(SmallStep1.oneStep(Succ(Succ(Zero)))), false, "t17"),
    Check.expect (valOf(SmallStep1.oneStep(Succ(Succ(Pred(Zero))))), Succ(Succ(Zero)), "t18"),
    Check.expect (valOf(SmallStep1.oneStep(Succ(Pred(Succ(Pred(Zero)))))), Succ(Pred(Succ(Zero))), "t19"),
    Check.expect (valOf(SmallStep1.oneStep(IsZero(Pred(Succ(Zero))))), IsZero(Zero), "t20"),
    Check.expect (valOf(SmallStep1.oneStep(IsZero(Zero))), True, "t21"),

    Check.expect (SmallStep1.steps(If(If(True, False, False), True, True)), [If(If(True, False, False), True, True), 
                                                                              If(False, True, True),
                                                                              True], "t22"),

    Check.expect (SmallStep1.eval(If(If(True, False, False), True, True)), True, "t23"),

    (* small-step-2 Tests *)
    Check.expect (valOf(SmallStep2.oneStep(If(True, False, If(True, True, True)))), If(True, False, True), "t24"),
    Check.expect (valOf(SmallStep2.oneStep(If(False, If(True, True, True), False))), If(False, True, False), "t25"),

    (* big-step Tests *)
    Check.expect (valOf(BigStep.eval(If(If(True, False, False), True, True))), True, "t26"),
    Check.expect (isSome(BigStep.eval(If(IsZero(True), False, False))), false, "t27"),
    Check.expect (valOf(BigStep.eval(Pred(If(True, If(IsZero(Pred(Succ(Zero))), Succ(Zero), True), False)))), Zero, "t28")
    ]

  val n = List.length tests

  val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ 
	    (if n=1 then "" else "s") ^ " passed!\n"

  (*val test = NB.tos(valOf(BigStep.eval(If(If(True, False, False), True, True))))*)

  val _ = TextIO.print msg
  (*val _ = TextIO.print test*)

end
