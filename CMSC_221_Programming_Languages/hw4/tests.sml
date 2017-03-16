structure Tests = struct

structure L = TLC

fun chk code value label =
  let
    val (tm, ty) = Compile.fromString code
  in
    Check.expect (tm, value, label)
  end

fun err code label =
    (TextIO.print (label ^ ": ");
    Check.error (fn _ => Compile.fromString code, label))

    val tests = 
                [chk "T" L.True "t00",
                chk "!" L.Not "t01",
                chk "(! T)" L.False "t02",
                err "*&%$" "t03",
                err "(())!!" "t04",
                err "(! x)" "t05",
                err "(! !)" "t06",
                err "(T T)" "t07",
                err "(If T ! F)" "t08",
                err "(If ! T F)" "t09",

                (* My tests! Things I've learned from this assignment: 
                        I cannot write lambda calc to save my life. *)
                chk "(If T ((L/ x: B . (! x)) F) F)" L.True "t10",
                chk "(L/ x:(B -> B).(L/x:B.x))" (L.Abs("x", L.Arrow(L.Bool, L.Bool),L.Abs("x", L.Bool, L.Var("x"))))  "t11",
                err "(If (L/x:B.x) T F)" "t12",
                err "((L/ x:(B -> B).((L/x:B.x) x)) !)" "t13",
                err "(L/y:B.x)" "t14"
                ]




    val n = List.length tests

    val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ (if n=1 then "" else "s") ^ " passed!\n"

    val _ = TextIO.print (if n>0 then msg else "###### no tests run\n")

end
