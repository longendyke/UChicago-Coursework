structure Tests = struct

  val _ = TestListComp.reset()

  val tests = [
  		(* Cross-compiling tests *)
  		TestListComp.testListComp ("[ x | x <- [1..10]]", "val result = [1,2,3,4,5,6,7,8,9,10];"),
  		TestListComp.testListComp ("[ x | x <- [1,6,4,3,2] ]", "val result = [1,6,4,3,2];"),
  		TestListComp.testListComp ("[ x | x <- [true, false, false] ]", "val result = [true, false, false];"),
  		TestListComp.testListComp ("[ (x, x) | x <- [1, 2] ]", "val result = [[1,1],[1,2],[2,1],[2,2]];"),
  		TestListComp.testListComp ("[ (x, 1) | x <- [1..5] ]", "val result = [[0,1],[1,1],[2,1],[3,1],[4,1],[5,1]];"),
  		TestListComp.testListComp ("[ (x, y) | x <- [1..4], y <- [1..4] ]", "val result = [[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[1,1],[1,2],[1,3],[1,4],[2,0],[2,1],[2,2],[2,3],[2,4],[3,0],[3,1],[3,2],[3,3],[3,4],[4,0],[4,1],[4,2],[4,3],[4,4]];"),
  		TestListComp.testListComp ("[ (x, y) | x <- [1..4], y <- [1..4], even x ]", "val result = [[0,0],[0,1],[0,2],[0,3],[0,4],[2,0],[2,1],[2,2],[2,3],[2,4],[4,0],[4,1],[4,2],[4,3],[4,4]];"),
  		TestListComp.testListComp ("[ (x, y) | x <- [1..4], y <- [1..4], even x, odd y ]", "val result = [[0,1],[0,3],[2,1],[2,3],[4,1],[4,3]];"),
  		TestListComp.testListComp ("[ (x, y) | x <- [1..4], y <- [true, false], even x, not y ]", "val result = [[0,false],[2,false],[4,false]];")
  		
  		(* Typechecking tests *)
  		(* Tested these by hand, I think... *)
  		]

  val n = List.length tests

  val msg = "========> Tests: " ^ Int.toString n ^ " test" ^ 
	    (if n=1 then "" else "s") ^ " passed!\n"

  val _ = TextIO.print (if n>0 then msg else "###### no tests run\n")

end
