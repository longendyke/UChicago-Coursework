structure AST = struct

  type id = string

  datatype item
    = ID of id
    | Nat of int
    | Bool of bool

  datatype gen
    = Nats  of int list
    | Bools of bool list
    | Range of int * int

  datatype gen_pred
    = Gen of id * gen
    | Pred of id * id

  datatype list_comp
    = ListComp of item list * (gen_pred list)
		  
  local (* string utils *)
    val commas = String.concatWith ", "
    val itos = Int.toString
    val btos = fn true => "true" | false => "false"
    fun paren s = "(" ^ s ^ ")"
    fun brack s = "[" ^ s ^ "]"
  in
    fun tos c =
      let
        fun item (ID x) = x
	       | item (Nat n) = itos n
	       | item (Bool b) = if b then "true" else "false"
	    fun tuple [] = raise Fail "empty tuple"
	       | tuple [i] = item i
	       | tuple is = paren (commas (map item is))
	    fun gen (Nats ns) = brack (commas (map itos ns))
	       | gen (Bools bs) = brack (commas (map btos bs))
	       | gen (Range (a, b)) = brack (itos a ^ ".." ^ itos b)
	    fun gen_pred (Gen (x, g)) = x ^ "<-" ^ gen g
	       | gen_pred (Pred (p, x)) = p ^ " " ^ x
	    fun list_comp (ListComp (tup, gps)) = 
            brack (tuple tup ^ " | " ^ commas (map gen_pred gps))
      in
	  list_comp c
      end
  end (* local *)

end
