structure CrossCompile = struct

  structure A = AST

  (* Cartesian product calculator *)
  fun cartesian(l) = 
    case l
        of [] => []
        | [x] => map (fn e => [e]) x
        | h::t =>
            let
                val cart_recurse = cartesian(t)
            in
                foldr (fn (h',result) =>
                        foldr (fn (h'', tail) => (h'::h'') :: tail ) result cart_recurse) 
                [] h
            end

  (* Handler functions for creating ranges. *)
  fun range_handle( start : int, fin : int ) : int list =
    if start > fin then []
    else if not(start = 1) then
        let
            val add = start - 1
        in
            List.tabulate(fin - start + 1, fn x => x+start)
        end
    else List.tabulate(fin +1, fn x => x)

  (* Even/odd evaluator. *)
  fun odd (n) = 
        if n=0 then false else even (n-1)
  and
    even (n) = 
        if n=0 then true else odd (n-1)

  (* Handler function for boolean predicates. *)
  fun bool_predicate_handler ( predicate : A.id, data : bool list ) : bool list = 
    case predicate
        of "not" => List.filter not data
        | _ => raise Fail "Error: Unknown predicate."

  (* Handler function for numeric predicates *)
  fun num_predicate_handler( predicate : A.id, data : int list ) : int list =
    case predicate
        of "even" => List.filter even data
        | "odd" => List.filter odd data
        | "pos" => List.filter (fn x => x > 0) data
        | "zero" => List.filter (fn x => x = 0) data
        | _ => raise Fail "Error: Unknown predicate."
      
  (* Takes a single predicate and a generator and applies that predicate
      to that generator. *)      
  fun predicate_handler ( predicate : A.id, data : A.gen ) : A.gen =
    case predicate
        of "not" => 
            let
                val A.Bools(booleans) = data
                val ret_bools = bool_predicate_handler(predicate, booleans)
                val return = A.Bools(ret_bools)
            in
                return
            end
        | _ => 
            (case data
                of A.Nats(naturals) =>
                    let
                        val ret_nats = num_predicate_handler(predicate, naturals)
                        val return = A.Nats(ret_nats)
                    in
                        return
                    end
                | A.Range(start, finish) =>
                    let
                        val to_pass_nats = range_handle(start, finish)
                        val ret_nats = num_predicate_handler(predicate, to_pass_nats)
                        val return = A.Nats(ret_nats)
                    in
                        return
                    end
                | _ => raise Fail "cannot get here"
            ) 

  (* Takes a generator (veiled in a gen_pred) var and a list of predicates
     (veiled in a gen_pred_list) and returns the fully evaluated generator over
     those predicates. *)
  fun predicate_evaluate (var : A.gen_pred, preds : A.gen_pred list) : A.gen = 
    let
        val A.Gen(id, gen) = var
    in
        case preds
            of [] => gen
            | h::t => 
                let
                    val A.Pred(pred, id) = h
                    val new_gen = predicate_handler(pred, gen)
                    val new_var = A.Gen(id, new_gen)
                in
                    predicate_evaluate(new_var, t)
                end
    end
    
  (* Receives a variable id and the rhs of a list comp, iterates over the rhs
     and finds all predicates where our given variable is mentioned. *)
  fun var_define ( var : A.id, rhs : A.gen_pred list ) : A.gen_pred list =
    case rhs
        of [] => []
        | h::t => 
            (case h
                of A.Gen(id, gen) =>
                    if var = id then h::var_define(var, t)
                    else var_define(var, t)
                | A.Pred(pred, id) =>
                    if var = id then h::var_define(var, t)
                    else var_define(var, t)
            )

  (*  Receives, one by one, variables from the LHS, then receives each variable's gen_pred list
      from var_define, and passes that list to predicate_evaluate. Returns with either a 
      list of nats (A.Nats(naturals)) or a list of booleans(A.Bools(booleans)), since ranges are
      dealt with in predicate_handler *)
  fun var_return (var : A.id, rhs : A.gen_pred list) : A.gen = 
    let
         val var_gp_list = var_define(var, rhs)
     in
         case var_gp_list
            of [] => raise Fail "Error: Somehow a free varialbe escaped my clutches."
            | h::t => predicate_evaluate(h, t)
     end

  fun determine_variables ( lhs : A.item list, rhs : A.gen_pred list) : A.gen list =
    case lhs
        of [] => []
        | h::t =>
            (case h
                of A.Nat(num) => A.Nats([num])::determine_variables(t, rhs)
                | A.Bool(boo) => A.Bools([boo])::determine_variables(t, rhs)
                | A.ID(x) =>
                    let
                        val evaluated_generator = var_return(x, rhs)
                    in
                    	case evaluated_generator
                    		of A.Range(start, finish) =>
                    			let
                    				val range = range_handle(start, finish)
                    				val fixed = A.Nats(range)
                    			in
                    				fixed::determine_variables(t, rhs)
                    			end
                        	| _ => evaluated_generator::determine_variables(t, rhs)
                    end
            )

  fun nats_explode ( ints : int list ) : A.item list = 
    case ints
        of [] => []
        | h::t => 
            A.Nat(h)::nats_explode(t)

  fun bools_explode ( bools : bool list ) : A.item list = 
    case bools
        of [] => []
        | h::t => 
            A.Bool(h)::bools_explode(t)

  fun explode_gen ( gen : A.gen ) : A.item list =
    case gen
        of A.Nats(naturals) =>
            nats_explode(naturals)
        | A.Bools(booleans) =>
            bools_explode(booleans)
        | _ => raise Fail "Error: cannot explode a range! Wouldn't that be dope if I could, though??"

  fun create_itemized_result( all_generators : A.gen list ) : (A.item list) list =
    case all_generators
        of [] => []
        | h::t => explode_gen(h)::create_itemized_result(t)

  fun d_i_explode ( step : A.item list ) : string list =
    case step
        of [] => []
        | h::t =>
            (case h
                of A.Nat(num) => Int.toString(num)::d_i_explode(t)
                | A.Bool(b) => if b then "true"::d_i_explode(t) else "false"::d_i_explode(t)
                | _ => raise Fail "Can't have a variable here, they should be all gone by now!"
            )

  fun d_i_glue ( step : string list ) : string = 
    "["^(String.concatWith "," step)^"]"

  fun de_itemize ( itemized_result : (A.item list) list ) : string list =
    case itemized_result
        of [] => []
        | h::t => 
            let
                val single_gen = d_i_explode(h)
                val glued = d_i_glue(single_gen)
            in
                glued::de_itemize(t)
            end

  fun all_together_now( glued : string list ) : string =
    d_i_glue(glued)

  fun do_the_thing( input : A.list_comp ) : string =
    let
        val A.ListComp(lhs, rhs) = input
        val result = determine_variables(lhs, rhs)
	in
		case result
        	of [] => ""
        	| [x] =>
        		(let
        			val exploded = explode_gen(x)
        			val un_exploded = d_i_explode(exploded)
        			val done = all_together_now(un_exploded)
        		in
        			done
        		end)
        	| h::t =>
        		let 
        			val itemized = create_itemized_result(result)
        			val produced = cartesian(itemized)
        			val almost = de_itemize(produced)
        			val done = all_together_now(almost)
    			in
        			done
	    		end
	end

  fun toSML (lc : A.list_comp) : string = 
    "val result = " ^ do_the_thing(lc)^";"

  fun fromString listCompCode = 
    let
      val file    = "listcomp-xc-result.sml" 
      val toks    = Scan.scan listCompCode
      val lc      = Parse.parse toks      
      val ty      = Typecheck.chk lc
      val smlCode = toSML lc
      val stream  = TextIO.openOut file
    in
     (TextIO.output (stream, smlCode); 
      TextIO.flushOut stream;
      TextIO.print ("The result is in the file \"" ^ file ^ "\".\n"))
    end

end

