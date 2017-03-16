structure Fresh : sig

(* fresh: generate a fresh variable name. *)
(* These are of the form "#0", "#1". *)
  val var : unit -> string

end = struct

  val nextVarIndex = ref 0

  fun var () = 
    let
      val i = !nextVarIndex
      val _ = nextVarIndex := (i+1)
    in
      "#" ^ Int.toString i
    end

(* These variable names are, by design, illegal in the surface
 * language. As such, they are guaranteed not to be in the program
 * already.
 *)

end
