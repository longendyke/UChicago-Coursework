structure NB : NB_SIG = struct
  
  datatype term
    = True
    | False
    | If of term * term * term 
    | Zero      
    | Succ of term
    | Pred of term
    | IsZero of term


(* Wrote this before Adam told us we could literally just write x = y... -__- *)
  fun eq (x : term , y : term) : bool = 
    case (x,y)
      of (True, True) => true
      | (False, False) => true
      | (If(xif), If(yif)) => 
        let
          val (x1, x2, x3) = xif
          val (y1, y2, y3) = yif
        in
          if eq (x1, y1) andalso eq (x2, y2) andalso eq (x3, y3) then true
          else false
        end
      | (Zero, Zero) => true
      | (Succ(x_succ), Succ(y_succ)) =>
        if eq(x_succ, y_succ) then true
        else false
      | (Pred(x_pred), Pred(y_pred)) =>
        if eq(x_pred, y_pred) then true
        else false
      | (IsZero(x_0), IsZero(y_0)) =>
        if eq(x_0, y_0) then true
        else false
      |(_,_) => false

  fun tos (t : term) : string = 
    case t
      of True => "True"
      | False => "False"
      | If(tif) => 
          let
            val (t1, t2, t3) = tif
          in
            "If " ^ tos(t1) ^ " then " ^ tos(t2) ^ " else " ^ tos(t3)
          end
      | Zero => "Zero"
      | Succ(t_succ) => "Succ("^tos(t_succ)^")"
      | Pred(t_pred) => "Pred("^tos(t_pred)^")"
      | IsZero(t_0) => "IsZero("^tos(t_0)^")"
    
  fun isNumVal (t : term) : bool = 
    case t
      of Zero => true
      | Succ(t_succ) => isNumVal(t_succ)
      | _ => false

  fun isBoolVal (t : term) : bool = 
    case t
      of True => true
      | False => true
      | _ => false

  fun isVal (t : term) : bool = 
    if isNumVal(t) orelse isBoolVal(t) then true
    else false

end
