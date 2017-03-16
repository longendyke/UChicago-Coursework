structure Eval : sig

  exception RuntimeError of string

(* Substitution with renaming as necessary. See the text 5.3.5. *)
  val subst : Var.var * TLC.term * TLC.term -> TLC.term

  val eval : TLC.term -> TLC.term

end = struct

  exception RuntimeError of string

  structure L = TLC
  structure V =Var

  (* Replaces a variable (to_be_replaced) with another (replacer) in a term *)
  fun replace (to_be_replaced : Var.var, replacer : Var.var, term : L.term) : L.term =
  	case term
  		of L.Var(x) => if to_be_replaced = x then L.Var(replacer) else term
        | L.Abs(var, ty, body) => L.Abs(replacer, ty, replace(to_be_replaced, replacer, body))
        | L.App(t1, t2) => L.App(replace(to_be_replaced, replacer, t1), replace(to_be_replaced, replacer, t2))
        | L.If(cond, t1, t2) => L.If(replace(to_be_replaced, replacer, cond), replace(to_be_replaced, replacer, t1), replace(to_be_replaced, replacer, t2))
        | _ => term

    (* Replace variable to t1 in t2 *)
  fun subst (variable : V.var, t1 : L.term, t2 : L.term) : L.term = 
  	case t2
        of L.Var(x) => if variable = x then t1 else t2
        | L.Abs(x, ty, body) => 
                let
                    val free_vars = FV.fv(t1)
                in
                    (* The check from 5.3 in the book *)
                    if V.member(x, free_vars) orelse variable = x then 
                        let
                            val renamed_term = replace(x, Fresh.var(), t2)
                        in
                            subst(variable, t1, renamed_term)
                        end
                    else subst(variable, t1, body)
                end
        | L.App(app_t1, app_t2) => L.App(subst(variable, t1, app_t1), subst(variable, t1, app_t2))
        | L.If(cond, if_t1, if_t2) => L.If(subst(variable, t1, cond), subst(variable, t1, if_t1), subst(variable, t1, if_t2))
        | _ => t2

    (* Following the same form of my eval from last week. Not much more here... *)
  fun eval (term : L.term) : L.term =
    if L.isVal(term) then term else 
    case term
        of L.If(cond, t1, t2) => 
            (case cond
                of L.True => eval(t1)
                | L.False => eval(t2)
                | _ => 
                    let
                        val eval_ret = eval(cond)
                    in
                        let
                            val next_step = L.If(eval_ret, t1, t2)
                        in
                            eval(next_step)
                        end
                    end
            (* EndCase *))
        | L.App(t1, t2) => 
                if L.isVal(t1) then 
                    if L.isVal(t2) then
                        (case t1
                            of L.Abs(var, ty, body) =>
                                let
                                    val substituted = subst(var, t2, body)
                                in
                                    eval(substituted) 
                                end
                            | L.Not => 
                                (case t2 
                                    of L.True => L.False
                                    | L.False => L.True
                                    | _ => 
                                        let
                                            val ev_t2 = eval(t2)
                                        in
                                            (let
                                                val next_step = L.App(t1, ev_t2)
                                            in
                                                eval(next_step)
                                            end)
                                        end
                                (* EndCase *))
                            | _ => raise RuntimeError "RuntimeError: you weren't even supposed to get here in the code??"
                        )
                    else 
                        (let
                            val ev_t2 = eval(t2)
                        in
                            (let
                                val next_step = L.App(t1, ev_t2)
                            in
                                eval(next_step)
                            end)
                        end)
                else
                    (let
                        val ev_t1 = eval(t1)
                    in
                        (let
                            val next_step = L.App(ev_t1, t2)
                        in
                            eval(next_step)
                        end)
                    end)
        | L.Var(x) => raise RuntimeError "RuntimeError: Apparently a variable has escaped the typechecker?? wtf?"
        | _ => raise RuntimeError "RuntimeError: Invalid term."
    (*end*)

end
