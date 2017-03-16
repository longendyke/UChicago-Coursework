structure Scan : sig

  exception ScanError of string

(* scan : Divide the string into a list of tokens.
 *        Make use of the functions Char.isDigit, Char.isSpace, etc.
 *        Skip whitespace, where whitespace is defined by Char.isSpace.
 *        See http://sml-family.org/Basis/char.html.
 *)
  val scan : string -> Token.token list

end = struct

  exception ScanError of string

  structure T = Token

  fun scan ( s : string ) : Token.token list = 
  	(* Sanity check: Test to see if we've been passed an empty string *)
  	if(String.size(s) = 0) then nil
  		else  		
  	(* No empty string, operate normally *)
  	let 
  		(* Filter our spaces in our exploded char list *)
  		val exploded = explode(s)
  	in
  		let 
  			val curr_char::rem_char = exploded
  			val next_string = implode(rem_char)
  		in 
  			if Char.isDigit(curr_char) then 
  				let
  					val (natural, returned_string) = Utils.construct_natural_num(exploded)
  				in
  					natural::scan(returned_string)
  				end

  			else if Char.isSpace(curr_char) then scan(next_string)
  			else
  			(case curr_char
  				of #"(" => Token.LParen::scan(next_string)
  				| #")" => Token.RParen::scan(next_string)
  				| #"," => Token.Comma::scan(next_string)
  				| #"t" => Token.True::scan(next_string)
  				| #"f" => Token.False::scan(next_string)
  				| #"+" => Token.Plus::scan(next_string)
  				| #"-" => Token.Minus::scan(next_string)
  				| #"?" => Token.QuestionMark::scan(next_string)
  				| _ => (case exploded
  							of #"i"::(#"f"::ts) => Token.If::scan(implode(ts))
  							| #"u"::(#"n"::(#"l"::(#"e"::(#"s"::(#"s"::ts))))) => Token.Unless::scan(implode(ts))
  							| #"o"::(#"r"::ts) => Token.Or::scan(implode(ts))
  							| #"a"::(#"n"::(#"d"::ts)) => Token.And::scan(implode(ts))
  							| #"n"::(#"o"::(#"t"::ts)) => Token.Not::scan(implode(ts))
  							| _ => raise ScanError("Error::Scan returned with no recognizeable strings")
  						)
  			)
  		end
  	end
  									

end