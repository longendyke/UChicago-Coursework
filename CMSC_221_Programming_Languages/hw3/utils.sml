structure Utils = struct

	structure T = Token

	fun return_number_string ( c_list : char list , ret_string : string ) : char list * string =
		let 
			val curr_char::rem_chars = c_list
		in
			if Char.isDigit(curr_char) then 
				if rem_chars = nil  then (nil, ret_string ^ Char.toString(curr_char))
				else return_number_string(rem_chars, ret_string ^ Char.toString(curr_char))
			else (c_list, ret_string)
		end

  	fun construct_natural_num( c_list : char list) : T.token * string = 
  		let
  			val (ret_chars, num_as_string) = return_number_string(c_list, "")
  		in
  			(T.Nat(valOf(Int.fromString(num_as_string))), implode(ret_chars))
  		end


end
