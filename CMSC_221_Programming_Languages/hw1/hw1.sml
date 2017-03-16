(* CMSC 22100 Spring 2016 The University of Chicago *)
(* HW1: SML warm-up exercises *)

(* Willem Longendyke *)

(* due Wed Apr 6 2016 11:59pm in hw1/hw1.sml in your repository *)

(* You can download and install the latest SML/NJ implementation from here:
 * http://www.smlnj.org/dist/working/110.79/index.html 
 *)

(* === Preliminaries === *)

(* some useful type synonyms *)

type 'a pair = 'a * 'a
type 'a pred = 'a -> bool
type 'a cmp  = 'a * 'a -> bool

(* The Check module is a lightweight testing toolkit, 
 * modeled after Racket's check-expect family of tools.
 *)

signature CHECK = sig

  (* check if two items are equal by built-in polymorphic equality *)
  val expect   : ''a * ''a * string -> unit

  (* check if two items are equal by equality function *)
  val expectBy : 'a cmp -> 'a * 'a * string -> unit

  (* check if two floating-point values are within epsilon of another *)
  val within   : real -> real * real * string -> unit

  (* check if given delayed computation raises an error *)
  val error : (unit -> 'a) * string -> unit

end

structure Check : CHECK = struct

  fun msg s m = "Check." ^ s ^ " failure: " ^ m

  fun expect (x : ''a, y : ''a, m : string) : unit = 
    if x=y then () else raise Fail (msg "expect" m)

  fun expectBy (eq : 'a cmp) (x : 'a, y : 'a, m : string) : unit =
    if eq(x,y) then () else raise Fail (msg "expectBy" m)

  fun within (eps : real) (x : real, y : real, m : string) : unit = 
    if abs(x-y)<=eps then () else raise Fail (msg "within" m)

  fun error (compute : unit -> 'a, m : string) : unit =
    let
      val x = SOME (compute ()) handle _ => NONE
    in
      case x
       of NONE => ()
	| SOME _ => raise Fail (msg "error" m)
    end

end (* structure Check *)

(* === Homework Problems === *)

(* === Problem 1 === *)

(* Here are inductively defined natural numbers. *)

datatype nat
  = Zero
  | Succ of nat

(* In natToInt, replace the raise expression with an
 * implementation of the function.
 *)

fun natToInt (n : nat) : int = 
  (case n of 
    Zero => 0
    | Succ n' => 1 + natToInt(n'))

(* Having written natToInt, uncomment the following tests. *)


 val _ = Check.expect (natToInt Zero, 0, "natToInt Zero")
 val _ = Check.expect (natToInt (Succ Zero), 1, "natToInt (Succ Zero)")


(* Continue by implementing intToNat and uncommenting the tests below. *)

fun intToNat (i : int) : nat = 
  if i = 0 then Zero
  else if i < 0 then Zero
  else Succ(intToNat(i - 1))


 val _ = Check.expect (intToNat 0, Zero, "intToNat 0")
 val _ = Check.expect (intToNat 1, Succ Zero, "intToNat 1")
 val _ = Check.expect (intToNat ~1, Zero, "intToNat ~1")


fun natAdd (m : nat, n : nat) : nat = 
  (case n of
  	Zero => m
  	| Succ n' => Succ(natAdd(m, n')))

 val _ = Check.expect (natAdd (Zero, Zero), Zero, "natAdd Zero")
 val _ = Check.expect (natAdd ((Succ Zero), (Succ Zero)) , Succ (Succ Zero), "natAdd Succ Succ Zero")

(* Write some of your own Check.expect tests here, and continue
 * doing so throughout this set of exercises.
 *)

fun natEq (m : nat, n : nat) : bool = 
  case m of
  	Zero => (case n of
  		Zero => true
  		| _ => false)
  	| Succ m' => (case n of
  		Zero => false
  		| Succ n' => natEq(m', n'))

 val _ = Check.expect (natEq (Zero, Succ Zero), false, "natEq false")
 val _ = Check.expect (natEq ((Succ Zero), (Succ Zero)) , true, "natEq true")


fun natGT (m : nat, n : nat) : bool = 
  case n of
  	Zero => (case m of
  		Zero => false
  		| Succ m' => true)
  	| Succ n' => (case m of
  		Zero => false
  		| Succ m' => natGT(m', n'))

 val _ = Check.expect (natGT (Zero, Succ Zero), false, "natGT false")
 val _ = Check.expect (natGT ((Succ (Succ Zero)), (Succ Zero)) , true, "natGT true")
					     
(* natToString should build a strings like "Zero",
 * "Succ(Zero)","Succ(Succ(Zero))", etc.
 * The string concatenation operator is ^, as in "a" ^ "b".
 *)

fun natToString (n : nat) : string = 
  case n of 
  	Zero => "Zero"
  	| Succ n' => "Succ("^natToString(n')^")"

 val _ = Check.expect (natToString (Zero), "Zero", "natToString Zero ")
 val _ = Check.expect (natToString (Succ (Succ Zero)) , "Succ(Succ(Zero))", "natToString Succ(Succ(Zero))")

(* === Problem 2 === *)

datatype filesize 
  = B of int
  | KB of real
  | MB of real
  | GB of real
  | TB of real

fun toMB (s : filesize) : filesize = 
  case s of
  	B(s) => (MB (Real.fromInt(s)/(1024.0 * 1024.0)))
  	| KB(s) => (MB (s/1024.0))
  	| MB(s) => (MB s)
  	| GB(s) => (MB (s * 1024.0))
  	| TB(s) => (MB (s * 1024.0 * 1024.0))

(* Function to take the filesize data type and convert it back to a real number
*  so that I can use it in the Check.within function.
*)

fun sizeToReal (s : filesize) : real =
	case s of
	  B(s) => Real.fromInt(s)
	  | KB(s) => s
  	  | MB(s) => s
  	  | GB(s) => s
  	  | TB(s) => s

 val _ = Check.within (1.0)(sizeToReal(toMB (MB 1.0)), 1.0, "toMB 1.0")
 val _ = Check.within (1.0)(sizeToReal(toMB (GB 1.0)), 1024.0, "toMB 1024")

(* === Problem 3 === *)

(* copies must produce a list of n copies of x. *)

fun copies (x : 'a, n : int) : 'a list = 
  case n of 
  	0 => nil
  	| _ => if n > 0 
  			then x :: copies(x, n - 1)
  			else nil

 val _ = Check.expect(copies (nil, 0), nil, "copies nil")
 val _ = Check.expect(copies (1, 0), nil, "copies nil")
 val _ = Check.expect(copies (1, 3), [1,1,1] , "copies [1,1,1]")
 val _ = Check.expect(copies ("Suh, dude?", 5), ["Suh, dude?","Suh, dude?","Suh, dude?","Suh, dude?","Suh, dude?"], 
 						"copies [Suh, dude?,Suh, dude?,Suh, dude?,Suh, dude?,Suh, dude?]")

(* weave should weave together xs and ys.
 * When one or the other list runs out, it should stop weaving.
 * ex: weave ([1,2],[10,20]) => [1,10,2,20]
 * ex: weave ([1],[10,20]) => [1,10]
 * ex: weave ([1,2],[10])  => [1,10]
 *)

fun weave (xs : 'a list, ys : 'a list) : 'a list =
    case (xs, ys) of
      (nil, _) => nil
      | (_, nil) => nil
      | (x::x', y::y') => x::y::weave(x', y')

val _ = Check.expect(weave ([1,2],[3,4]), [1,3,2,4], "weave [1,3,2,4]")
val _ = Check.expect(weave ([1,2],[3]), [1,3], "weave [1,3]")
val _ = Check.expect(weave ([1],[3,4]), [1,3], "weave [1,3]")

(* number must pair each item with an index, starting at start.
 * ex: number(0,["a","b"]) => [(0,"a"),(1,"b")]
 * ex: number(1,["a","b"]) => [(1,"a"),(2,"b")]
 *)

fun number (start : int, xs : 'a list) : (int * 'a) list =
  case xs of
   nil => nil
   | h::t => (start, h) :: number(start + 1, tl(xs))

val _ = Check.expect(number (0, [1,2,3]), [(0,1),(1,2),(2,3)], "number [(0,1),(1,2),(2,3)]")
val _ = Check.expect(number (0, nil), nil, "number nil")
val _ = Check.expect(number (~3, ["a","b","c"]), [(~3,"a"),(~2,"b"),(~1,"c")], 
							"number [(~3,a),(~2,b),(~1,c)]")

(* === Problem 4 === *)

(* The infix directive instructs the parser that the
 * given identifier is an infix operator. 
 *)

infix \/
infix /\

(* \/ is a "disjunctive composition" operator for tests.
 * Assuming you have tests isPrime and isOdd, then
 * the test (isPrime \/ isOdd) identifies primes and/or odds.
 *)

fun (p : 'a pred) \/ (q : 'a pred) : 'a pred = 
  let 
  	fun h x = p x orelse q x
  in 
  	h
  end;

 (* Tests! *)

 (* NOTE:: This code taken from http://www.cs.cornell.edu/courses/cs312/2008sp/lectures/lec02.html*)
fun isPrime(n: int): bool =
  let fun noDivisorsAbove(m: int) =
    if n mod m = 0 then false
	else if m*m >= n then true
	else noDivisorsAbove(m+1)
  in
    noDivisorsAbove(2)
  end

(* This code taken from https://www.cs.cmu.edu/~rwh/introsml/core/recfns.htm *)
fun even 0 = true
  | even n = odd (n-1)
and odd 0 = false
  | odd n = even (n-1)

 val _ = Check.expect((isPrime \/ odd)(5), true, "(isPrime OR odd)(5) true")
 val _ = Check.expect((isPrime \/ odd)(6), false, "(isPrime OR odd)(6) false")
 val _ = Check.expect((isPrime \/ even)(12), true, "(isPrime OR odd)(12) true")

(* /\ is a "conjunctive composition" operator for tests.
 * Assuming you have tests isPrime and isOdd, then
 * the test (isPrime /\ isOdd) identifies odd primes.
 *)

fun (p : 'a pred) /\ (q : 'a pred) : 'a pred = 
  let fun h x = p x andalso q x
  in 
  	h
  end;

 val _ = Check.expect((isPrime /\ odd)(5), true, "(isPrime AND odd)(5) true")
 val _ = Check.expect((isPrime /\ odd)(15), false, "(isPrime AND odd)(15) false")
 val _ = Check.expect((isPrime /\ even)(12), false, "(isPrime AND odd)(12) false")


(* === Problem 5 === *)

(* commutes: If the operator commutes on all pairs, return NONE.  
 * To commute on a pair means that oper(x,y) is the same as oper(y,x).  
 * If the operator does not commute on a pair, return it, wrapped 
 * in SOME, as a counterexample to commutativity.
 *)

fun commutes (oper : 'a pair -> ''b, pairs : 'a pair list) : 'a pair option =
  case pairs 
  	of nil => NONE
  	| h::t => let
  				val (x, y) = h
  			  in
  				if oper(x,y) = oper(y, x) 
  				then commutes(oper, tl(pairs))
  				else SOME h
  	  		  end

 (* TESTS! *)
 fun pairadd(sum : int pair) : int =
 	let
 		val (x,y) = sum
 	in
 		x + y
 	end

 fun pairsub(sum : int pair) : int =
 	let
 		val (x,y) = sum
 	in
 		x - y
 	end

 val _ = Check.expect(commutes (pairadd, nil), NONE, "commutes (pairadd, nil")
 val _ = Check.expect(commutes (pairadd, [(1,2),(3,4)]), NONE, "commutes (pairadd, [(1,2),(3,4)]")
 val _ = Check.expect(commutes (pairsub, [(1,2),(3,4)]), SOME (1,2), "commutes (pairsub, [(1,2),(3,4)]")


(* === Problem 6 === *)

(* Here is a mutually recursive datatype for trees
 * that alternate between having 2 and 3 children at
 * each level, and furthermore alternate between 
 * having 'a and 'b data at each level. 
 *)

datatype ('a, 'b) t2
  = E2
  | Nd2 of 'a * ('a, 'b) t3 * ('a, 'b) t3
and ('a, 'b) t3
  = E3
  | Nd3 of 'b * ('a, 'b) t2 * ('a, 'b) t2 * ('a, 'b) t2

(* Count the number of nodes in a given tree. Nodes to be 
 * counted are Nd2 and Nd3 values, not E2 or E3.
 *)

fun numNodes2 (t : ('a, 'b) t2) : int =
  case t of
  	E2 => 0
  	| Nd2 t' => let 
  				  	val (t1, x, y) = t'
  				in
  					1 + numNodes3(x) + numNodes3(y)
  				end
and numNodes3 (t : ('a, 'b) t3) : int =
  case t of
  	E3 => 0
  	| Nd3 t' => let 
  				  	val (t1, x, y, z) = t'
  				in
  					1 + numNodes2(x) + numNodes2(y) + numNodes2(z)
  				end

(* TESTS *)

val t2leaf = Nd2(1, E3, E3)
val t3leaf = Nd3("a", E2, E2, E2)
val tree = Nd2(1, t3leaf, Nd3("b", t2leaf, t2leaf, Nd2(100, t3leaf, t3leaf)))

val _ = Check.expect(numNodes2(E2), 0, "numNodes2(E2)")
val _ = Check.expect(numNodes3(E3), 0, "numNodes2(E3)")
val _ = Check.expect(numNodes2(t2leaf), 1, "numNodes2(t2leaf)")
val _ = Check.expect(numNodes3(t3leaf), 1, "numNodes3(t3leaf)")
val _ = Check.expect(numNodes2(tree), 8, "numNodes3(tree)")