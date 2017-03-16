structure Typecheck = struct

  structure A  = AST
  structure T = Ty
  structure B = Basis

  (* Lookup function for variable context space gamma *)
  fun gamma_lookup( var : A.id, gamma : (A.id * T.ty) list ) : T.ty option =
    case gamma
        of nil => NONE
        | h::t => 
            let
                val (id, ty) = h
            in
                if var = id then SOME ty else gamma_lookup(var, t) 
            end

  (* Extend function for variable context space gamma *)
  fun gamma_extend (var : A.id, ty : T.ty, gamma : (A.id * T.ty) list ) : (A.id * T.ty) list = 
    case gamma
        of nil => [(var, ty)]
        | curr::ts => (var, ty)::gamma

  (* Membership check function for variable context space gamma *)
  fun gamma_member (gamma : (A.id * T.ty) list, x : A.id) : bool =
    case gamma
        of nil => false
        | curr::ts => 
                    let
                        val (v, ty) = curr
                    in
                        if x = v then true else gamma_member(ts, x)
                    end

  (* Little function to determine whether or not a variable is mentioned in a generator in a gen/pred list *)
  fun member (find : A.id, g_p : A.gen_pred list ) : bool =
    case g_p
        of nil => false
        | h::t => 
            (case h
                of A.Gen(id, gen) => if find = id then true else member(find, t)
                | A.Pred(basic, id) => member(find, t)
            )
  (* Function to handle legal generators and assign types to their variables, storing those types in gamma *)
  fun gen_handler ( x : A.id, gen : A.gen, gamma : (A.id * T.ty) list) : T.ty * ((A.id * T.ty) list) = 
    case gen
        of A.Bools(bools) => 
            let
                val new_gamma = gamma_extend(x, T.List(T.Bool), gamma)
            in
                (T.List(T.Bool), new_gamma)
            end
        | _ => 
            let
                val new_gamma = gamma_extend(x, T.List(T.Nat), gamma)
            in
                (T.List(T.Nat), new_gamma)
            end

  (* Traverse the RHS of a comprehension, seeking to assign a legal type to a variable *)
  fun lookup( find : A.id, g_p : A.gen_pred list, gamma : (A.id * T.ty) list) : T.ty * ((A.id * T.ty) list) = 
    case g_p
        of nil => raise Fail ("TypeError: Found a free variable: " ^ find)
        | h::t =>
            (case h
                of A.Gen(x, g) => 
                    if find = x then
                        (if member(find, t) then raise Fail "Error: Multiple generators for single variable."
                        else gen_handler(find, g, gamma)
                        )
                    else lookup(find, t, gamma)
                | A.Pred(pred, x) => 
                    if find = x 
                        andalso not(gamma_member(gamma, find)) then raise Fail "Error: Predicate preceeding generator."
                    else lookup(find, t, gamma)
            )

  fun find_for_pc ( pred : A.id, env : (A.id * T.ty) list) : T.ty = 
    case env
        of nil => raise Fail "TypeErrorError: Encountered unknown predicate."
        | h::t => 
            let
                 val (name, ty) = h
             in
                 if pred = name then ty else find_for_pc(pred, t)
             end 

  fun predi_check ( item : A.item, g_p_list : A.gen_pred list, gamma : (A.id * T.ty) list ) : bool =
    case item
        of A.ID(term) =>
            (case g_p_list
                of nil => true
                | h::t => 
                    (case h
                        of A.Gen(id, gen) => predi_check(item, t, gamma)
                        | A.Pred(pred, id) =>
                            if (term = id) then
                                let
                                    val T.Arrow(ty1, ty2) = find_for_pc(pred, B.env)
                                    val term_ty = valOf(gamma_lookup(term, gamma))
                                in
                                    (case term_ty
                                        of T.List(ty) => 
                                            let
                                                val inner_ty = ty
                                            in
                                                if not(inner_ty = ty1) then false
                                                else predi_check(item, t, gamma)
                                            end
                                        | _ => 
                                            if not(term_ty = ty1) then false
                                            else predi_check(item, t, gamma)
                                    )
                                end
                            else predi_check(item, t, gamma)
                    )
            )
        | _ => true

(* Searches through a list of items given an id of a predicate, searching to see if it appears in the list *)
fun rhs_helper ( lhs : A.item list, g_p_id : A.id ) : bool =
    case lhs
        of nil => false
        | h::t => 
            (case h
                of A.ID(x) => 
                    if g_p_id = x then true else rhs_helper(t, g_p_id)
                | _ => rhs_helper(t, g_p_id)
            )

  fun rhs_free_vars ( lhs : A.item list, g_p : A.gen_pred list ) : bool =
    case g_p
        of nil => true
        | h::t => 
            (case h
                of A.Gen(id, gen) => rhs_free_vars(lhs, t)
                | A.Pred(pred, id) =>
                    if not(rhs_helper(lhs, id)) then false
                    else rhs_free_vars(lhs, t)
            )

  (* Determine the type of a single item *)
  fun typeOf( t : A.item, g_p : A.gen_pred list, gamma : (A.id * T.ty) list) : T.ty * ((A.id * T.ty) list) =
    case t
        of A.Nat(num) => (T.Nat, gamma)
        | A.Bool(b) => (T.Bool, gamma)
        | A.ID(x) => 
            if gamma_member(gamma, x) then (valOf(gamma_lookup(x, gamma)), gamma)
            else lookup(x, g_p, gamma)

  fun tupler ( ts : A.item list, g_p : A.gen_pred list, gamma : (A.id * T.ty) list ) : (T.ty list)  = 
    case ts
        of nil => nil
        | (h::t) => 
            let
                 val (ty, new_gamma) = typeOf(h, g_p, gamma)
                 val predicheck = predi_check(h, g_p, new_gamma)
             in
                if predicheck then
                    ty::tupler(t, g_p, new_gamma)
                else
                    raise Fail ("TypeError: Invalid predicate application to variable.")
             end 


(* Make sure the list comprehension is well-typed and has
 * no free variables. Return its type. Raise Fail in case
 * of a type error or free variable(s).
 *)
  fun chk ( c : A.list_comp ) : Ty.ty = 
    let
        val A.ListComp(lhs, rhs) = c
    in
        case lhs
            of nil => raise Fail "TypeError: Received empty LHS"
            | h::t =>
                if rhs_free_vars(lhs, rhs) then
                    if null(t) then 
                        let
                            val (ty, gamma) = typeOf(h, rhs, nil)
                            val predicheck = predi_check(h, rhs, gamma)
                        in
                            if predicheck then
                                ty
                            else
                                raise Fail ("TypeError: Invalid predicate application to variable.")
                        end 
                    else 
                        let
                            val result_list = tupler(lhs, rhs, nil)
                        in
                            T.Tuple(result_list)
                        end
                else raise Fail "TypeError: Free variables on RHS."
    end
    
end
