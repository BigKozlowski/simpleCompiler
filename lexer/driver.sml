datatype token = IF | THEN | ELSE | BEGIN | END | PRINT | SEMI | NUM | EQ | EOF

structure Parse =
struct 
	fun parse filename =
		let 
			val file = TextIO.openIn filename
			fun get _ = TextIO.input file
			val lexer = Mlex.makeLexer get

			fun do_it() =
				let 
					val t = lexer()
					val token = case t of
									"IF" => IF
									| "THEN" => THEN 
									| "ELSE" => ELSE
									| "BEGIN" => BEGIN
									| "END" => END
									| "PRINT" => PRINT
									| "SEMI" => SEMI
									| "NUM" => NUM
									| "EQ" => EQ
									| "EOF" => EOF
									| _ => EOF
					val tokens = [token]
				in 
					(* print t; 
					print "\n"; *)
					
					if substring(t,0,3)="EOF" 
						then tokens 
					else (tokens @ do_it())
				end

			val tokens = do_it()
			val _ = TextIO.closeIn file
		in 
			tokens
		end

end

