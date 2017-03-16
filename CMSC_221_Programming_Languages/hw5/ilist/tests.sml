structure Tests = struct

  structure I = IList

  fun less (x,y) = x < y
  val ord = {lt = less}
  val list1 = I.cons(1, I.List(ord, I.Nil))
  val list2 = I.cons(2, list1)
  val list3 = I.cons(3, list2)
  val map_test = I.cons(6, I.cons(5, I.cons(4, I.List(ord, I.Nil))))
  val rev_test = I.cons(1, I.cons(2, I.cons(3, I.List(ord, I.Nil))))
  fun equal(a, b) = (a=b)

  val tests = [
        Check.expect(I.hd(list3), SOME 3, "t01"),
        Check.expectBy (I.same equal) (valOf(I.tl(list3)), list2, "t0x"),
        Check.expect(I.length(list3), 3, "t02"),
        Check.expect(I.max(list3), SOME 3, "t03"),
        Check.expect(I.min(list3), SOME 1, "t04"),
        Check.expectBy (I.same equal) ((I.map ord (fn x => x+3) list3), map_test, "t05"),
        Check.expectBy (I.same equal) ((I.rev list3), rev_test, "t06"),
        Check.expect(I.toList(list3), [3,2,1], "t07"),
        Check.expectBy (I.same equal) (I.fromList ord [3,2,1], list3, "t08")

  ]

  val n = List.length tests

  val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ 
	    (if n=1 then "" else "s") ^ " passed!\n"

  val _ = TextIO.print (if n>0 then msg else "###### no tests run\n")

end
