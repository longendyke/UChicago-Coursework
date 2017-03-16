structure IList : ILIST = struct

  type 'a ord   = {lt : 'a * 'a -> bool}
  type 'a instr = {len : int, max : 'a, min : 'a}

  datatype 'a seq
    = Nil
    | Cons of 'a instr * ('a * 'a seq)

  datatype 'a ilist
    = List of 'a ord * 'a seq

  (* Functions for updating instrumentation *)
  fun find_length (l : 'a seq) : int = 
    case l 
        of Nil => 0 
        | Cons(instr, (node, next)) => 1 + find_length(next)

  fun reset_min ord seq min = 
    case seq  
        of Nil => min 
        | Cons(instr, (node, next)) => 
            let
                val {lt = f} = ord
            in 
                if f(node,min) then reset_min ord next node
                else reset_min ord next min
            end 

  fun fmin_init ord seq  = 
    case seq 
        of Nil => raise Fail "Nil doesn't have a minimum either!"
        | Cons(instr, (node, next)) => (reset_min ord next node)

  fun reset_max ord seq max = 
    case seq 
        of Nil => max 
        | Cons(instr, (node, next)) => 
            let
                val {lt = f} = ord
            in 
                if f(node,max) then reset_max ord next max
                else reset_max ord next node
            end

  fun fmax_init ord seq  = 
    case seq
        of Nil => raise Fail "Nil has no maximum, fool!"
        | Cons(instr, (node, next)) => (reset_max ord next node)

  fun build_new_instr ord seq = 
    case seq
        of Nil => raise Fail "get_new_instrumentation called on Nil"
        | Cons (instr, (node, next)) => 
            let
                val new_min = fmin_init ord seq
                val new_max = fmax_init ord seq 
                val new_length = find_length seq 
                val new_instr = {len = new_length, max = new_max, min = new_min}
            in
                new_instr
            end

  fun place_instr instr seq = 
    case seq 
      of Nil => Nil 
      | Cons (curr_instr, (node, next)) => Cons(instr, (node, (place_instr instr next)))

  fun hd ( l : 'a ilist ) : 'a option = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => NONE
  			| Cons(instr, (head, tail)) => SOME head
  	end

  fun tl ( l : 'a ilist ) : 'a ilist option = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => NONE
  			| Cons(instr, (head, tail)) => SOME (List(order, tail))
  	end



  fun cons ( add : 'a, l : 'a ilist ) : 'a ilist = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil =>
  				let
  					val n_instr = {len = 1, max = add, min = add}
  					val new_list = (add,Nil)
  				in
  					List(order, Cons(n_instr, new_list))
  				end
  			| Cons(instr, data) =>
  				let
                    val n_seq = Cons(instr, (add, sequence))
                    val n_instr = build_new_instr order n_seq
                    val n_fixed_seq = place_instr n_instr n_seq
  				in
  					List(order, n_fixed_seq)
  				end

  	end

  fun length ( l : 'a ilist ) : int = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => 0
  			| Cons(instr, data) => (#len instr)
  	end

  fun max ( l : 'a ilist ) : 'a option = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => NONE
  			| Cons(instr, data) => SOME (#max instr)
  	end

  fun min ( l : 'a ilist ) : 'a option = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => NONE
  			| Cons(instr, data) => SOME (#min instr)
  	end



  fun map ord_b funct a_list = 
  	let
  		val List(order, sequence) = a_list
  	in
  		case sequence
  			of Nil => List(ord_b, Nil)
  			| Cons(instr, data) =>
  				let
  					val head = valOf(hd(a_list))
  					val tail = valOf(tl(a_list))
  				in
  					cons(funct(head), (map ord_b funct tail))
  				end

  	end
  
  fun filter filt l  = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => l
  			| Cons(instr, data) =>
  				let
  					val head = valOf(hd(l))
  					val tail = valOf(tl(l))
  				in
  					if(filt(head)) then cons(head, (filter filt tail))
  					else filter filt tail
  				end

  	end

  fun foldr funct b l = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => b
  			| Cons(instr, data) =>
  				let
  					val head = valOf(hd(l))
  					val tail = valOf(tl(l))
  				in
  					funct(head, (foldr funct b tail))
  				end

  	end

  fun foldl funct b l = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => b
  			| Cons(instr, data) =>
  				let
  					val head = valOf(hd(l))
  					val tail = valOf(tl(l))
  				in
  					foldl funct (funct(head, b)) tail
  				end

  	end

  (* Required that order1 = order2 *)
  fun append ( l1 : 'a ilist, l2 : 'a ilist) : 'a ilist =
  	let
  		val List(order1, sequence1) = l1
  		val List(order2, sequence2) = l2
  	in
  		case (sequence1, sequence2)
  			of (Nil, Nil) => List(order1, Nil)
  			| (_, Nil) => l1
  			| (Nil, _) => l2
  			| (Cons(instr1, data1), Cons(instr2, data2)) =>
  				let
  					val head1 = valOf(hd(l1))
  					val tail1 = valOf(tl(l1))
  				in
  					if length(tail1) = 0 then cons(head1, l2)
  					else cons(head1, append(tail1, l2))
  				end

  	end


  fun rev( l : 'a ilist) : 'a ilist = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => l
  			| Cons(instr, data) =>
  				let
  					val head = valOf(hd(l))
  					val tail = valOf(tl(l))
  				in
  					append(rev(tail), cons(head, List(order,Nil)))
  				end

  	end


  fun same comp (l1, l2) = 
  	let
  		val List(order1, sequence1) = l1
  		val List(order2, sequence2) = l2
  	in
  		case (sequence1, sequence2)
  			of (Nil, Nil) => true
  			| (_, Nil) => false
  			| (Nil, _) => false
  			| (Cons(instr1, data1), Cons(instr2, data2)) =>
  				let
  					val head1 = valOf(hd(l1))
  					val head2 = valOf(hd(l2))
  					val tail1 = valOf(tl(l1))
  					val tail2 = valOf(tl(l2))
  				in
  					if comp(head1, head2) then same comp (tail1, tail2)
  					else false
  				end
  	end

  fun toList ( l :'a ilist) : 'a list = 
  	let
  		val List(order, sequence) = l
  	in
  		case sequence
  			of Nil => []
  			| Cons(instr, data) => 
  				let
  					val head = valOf(hd(l))
  					val tail = valOf(tl(l))
  				in
  					head::toList(tail)
  				end
  	end

  fun fromList order l = 
  	case l
  		of [] => List(order, Nil)
  		| h::t => cons(h, (fromList order t))

end
