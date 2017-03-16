structure Var :> sig

(* This is a module for variables and sets of variables. *)

  type var = string
  type var_set = var list

(* Implement var_set however you like: list, tree, etc. *)
(* Modules outside Var are not allowed to know how var_set is implemented. *)
(* They can only compute with var_set values through the exposed operations. *)
(* The "opaque signature ascription" symbol :> makes it so. *)

  exception OpenTerm of var list
  exception VarSetError of var list

(* A selection of set operations. You will need most of them, possibly all of them. *)
 
  val empty     : var_set
  val singleton : var -> var_set
  val isEmpty   : var_set -> bool
  val member    : var * var_set -> bool
  val insert    : var * var_set -> var_set
  val remove    : var * var_set -> var_set
  val union     : var_set * var_set -> var_set
  val asList    : var_set -> var list

end = struct

  type var = string

  type var_set = var list (* You must change this to something else! Type "unit" certainly won't do. *)

  exception OpenTerm of var list
  exception VarSetError of var list

  val empty = nil

  fun isEmpty ( vars : var_set ) : bool = List.null(vars)

  fun singleton ( v : var ) : var_set = v::nil

  fun member ( search : var, vars : var_set ) : bool = 
    case vars
      of nil => false
      | (hd::tl) => if hd = search then true else member(search, tl)

  fun insert ( ins : var, vars : var_set ) : var_set = 
    if member(ins, vars) then vars else ins::vars

  fun remove ( rm : var, vars : var_set ) : var_set = 
    case vars
      of nil => vars
      | (hd::tl) => if rm = hd then tl else hd::remove(rm, tl)


  fun union ( vars1 : var_set, vars2 : var_set ) : var_set = 
    case (vars1 ,vars2)
      of (nil, nil) => nil
      | (_, nil) => vars1
      | (nil, _) => vars2
      (* Walk down the first list, appending in order until we run out of the first list *)
      | (hd1::tl1, hd2::tl2) => if member(hd1, vars2) then union(tl1, vars2) 
                                else hd1::union(tl1, vars2)

  fun asList (vars : var_set) : var list = vars

end
