structure Typecheck :> sig

  exception TypeError of string

  type type_env

(* I've defined a typing environment as a list of (var * type) pairs
 * You can either stick with that implementation, or use a different, more
 * efficient one if you prefer. 
 *)

  val empty  : type_env 
  val typeOf : type_env * TLC.term -> TLC.ty

end = struct

  exception TypeError of string

  structure L = TLC

  type term = L.term

  type type_env = (Var.var * L.ty) list

  val empty = []

  fun eq (t:L.ty, u:L.ty) = (t=u)

(* The stubs of lookup and extend are left here as suggestions for helpful helpers. *)  
  fun lookup (gamma : type_env, x : Var.var) : Var.var * L.ty = 
    case gamma
        of nil => raise TypeError "TypeError: Encountered unknown variable."
        | curr::ts => 
                    let
                        val (v, ty) = curr
                    in
                        if x = v then curr else lookup(ts, x)
                    end

  fun extend (gamma : type_env, x : Var.var, ty : L.ty) : type_env = 
    case gamma
        of nil => [(x, ty)]
        | curr::ts => (x, ty)::gamma

  fun member (gamma : type_env, x : Var.var) : bool =
    case gamma
        of nil => false
        | curr::ts => 
                    let
                        val (v, ty) = curr
                    in
                        if x = v then true else member(ts, x)
                    end

    (* Again, basically following the book's definition *)
  fun typeOf (gamma : type_env, t : term) : L.ty = 
    case t
        of L.True => L.Bool
        | L.False => L.Bool
        | L.Not => L.Arrow(L.Bool, L.Bool)
        | L.Var(var) => 
                        let
                            val (v, ty) = lookup(gamma, var)
                        in
                            ty
                        end
        | L.If(cond, t1, t2) => 
                                let
                                    val t_c = typeOf(gamma, cond)
                                    val t1 = typeOf(gamma, t1)
                                    val t2 = typeOf(gamma, t2)
                                in
                                    if not(t_c = L.Bool) then raise TypeError "TypeError: Illegal If condition" 
                                    else if not(eq(t1, t2)) then raise TypeError "TypeError: If statement types do not match."
                                    else t1
                                end
        | L.Abs(var, ty, body) =>
                                let
                                    val ext_gamma = extend(gamma, var, ty)
                                    val t_body = typeOf(ext_gamma, body)
                                in
                                    L.Arrow(ty, t_body)
                                end
        | L.App(t1, t2) =>
                            let
                                val ty1 = typeOf(gamma, t1)
                                val ty2 = typeOf(gamma, t2)
                            in
                                case ty1
                                    of L.Arrow(t11, t12) => if not(eq(t11, ty2)) then raise TypeError "TypeError: Application domain and argument type do not match."
                                                            else t12
                                    | _ => raise TypeError "TypeError: Illegal application type."
                            end
end
