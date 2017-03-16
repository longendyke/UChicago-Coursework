structure Ty = struct

  type label = string

  datatype ty
    = Nat
    | Bool
    | Pair of ty * ty
    | Arrow of ty * ty
    | Record of record_item * (record_item list)
(* NOTE: Record is defined so empty record types are impossible. *)
    | Ref of ty
    | List of ty
    | Array of ty
    | Top
  and record_item = RI of label * ty

  fun unparse t =
    let
      fun lp Nat = "Nat"
	| lp Bool = "Bool"
	| lp (Pair (s,t)) = "(* " ^ lp s ^ " " ^ lp t ^ ")"
	| lp (Arrow (s,t)) = "(-> " ^ lp s ^ " " ^ lp t ^ ")"
	| lp (Record (ri,tl)) = 
	    "{" ^ String.concatWith "," (List.map recordItem (ri::tl)) ^ "}"
	| lp (Ref t) = "(Ref " ^ lp t ^ ")"
	| lp (List t) = "(List " ^ lp t ^ ")"
	| lp (Array t) = "(Array " ^ lp t ^ ")"
	| lp Top = "Top"
      and recordItem (RI (label, ty)) = label ^ ":" ^ lp ty
    in
      lp t
    end
 
  fun receq record list_item : bool =
    let
        val RI(search_lab, search_ty) = record
        val RI(list_lab, list_ty) = list_item
    in
        if search_lab = list_lab andalso search_ty = list_ty then true else false
    end

  fun rec_perm_help (recs1 : record_item list, recs2 : record_item list, remaining_to_check : int) : bool = 
    case (recs1, recs2, remaining_to_check)
        of ( _, _, 0) => true
        | (nil, _, _) => false
        | (_, nil, _) => false
        | (h1::t1, h2::t2, _) => if List.exists (receq h1) recs2 then rec_perm_help(t1, recs2, remaining_to_check-1)
                                    else false
  fun rec_perm (rec1, rec2) : bool =
    let
         val Record(f1, r1) = rec1
         val Record(f2, r2) = rec2
         val len1 = List.length(f1::r1)
         val len2 = List.length(f2::r2)
     in
         if not(len1 = len2) then false
         else rec_perm_help(f1::r1, f2::r2, len1)
     end 

  fun ty_ret (find : record_item, rec_list : record_item list) : ty option * ty option =
    case rec_list
        of nil => (NONE, NONE)
        | h::t => 
            let
                val RI(chk, ty) = h
                val RI(lbl, ty_find) = find
            in
                if lbl = chk then (SOME(ty), SOME(ty_find)) else ty_ret(find, t)
            end

  fun subtype (ty1, ty2) : bool = 
    if ty1 = ty2 then true else
    case (ty1, ty2)
        of (_ , Top) => true
        | (Pair(p11, p12), Pair(p21, p22)) => subtype(p11, p21) andalso subtype(p12, p22)
        | (Ref(t1), Ref(t2)) => subtype(t1, t2) andalso subtype(t2, t1)
        | (List(t1), List(t2)) => subtype(t1, t2)
        | (Array(t1), Array(t2)) => subtype(t1, t2)
        | (Arrow(domain1, image1), Arrow(domain2, image2)) => subtype(domain2, domain1) andalso subtype(image1, image2)
        | (Record(first1, rest1), Record(first2, rest2)) => rec_width_and_depth(first1::rest1, first2::rest2) orelse rec_perm(ty1, ty2)
        | (_, _) => false
  and rec_width_and_depth( recs1 : record_item list, recs2 : record_item list ) : bool =
    case (recs1, recs2)
        of (_, nil) => true
        | (nil, _) => false
        | (h1::t1, h2::t2) =>
            let
                val (ty_wide, ty_narrow) = ty_ret(h2, recs1) 
            in
                if isSome(ty_wide) then 
                    subtype(valOf(ty_wide), valOf(ty_narrow)) andalso rec_width_and_depth(recs1, t2)
                else false
            end

end
