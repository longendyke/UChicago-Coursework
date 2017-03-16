structure Check :
sig

(* check if two items are equal by built-in polymorphic equality *)
val expect : ''a * ''a * string -> unit

(* check if two items are equal by equality function *)
val expectBy : ('a * 'a -> bool) -> 'a * 'a * string -> unit

(* check if two floating-point values are within epsilon of another *)
val within : real -> real * real * string -> unit

(* check if given delayed computation raises an error *)
val error : (unit -> 'a) * string -> unit

(* if the assertion fails, raise an exception *)
val assert : bool * string -> unit

  end = struct

fun msg s m = "Check." ^ s ^ " failure: " ^ m ^ "\n"

fun expect (x, y, m) = if x=y then () else raise Fail (msg "expect" m)

fun expectBy eq (x, y, m) = if eq(x,y) then () else raise Fail (msg "expectBy" m)

fun within (eps:real) (x, y, m) = if abs(x-y)<=eps then () else raise Fail (msg "within" m)

fun error (compute, m) =
    let
        val x = SOME (compute ()) handle _ => NONE
    in
        case x
        of NONE => ()
        | SOME _ => raise Fail (msg "error" m)
    end

fun assert (t, m) = if t then () else raise Fail (msg "assert" m)

end
