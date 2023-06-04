type tokenlist = token list

signature PARSER =
sig
    val tok: token ref
    val advance: unit -> unit
    val eat: (token) -> unit
    val S: unit -> unit
    val L: unit -> unit
    val E: unit -> unit
    val tokens: tokenlist ref
    val getToken: unit -> token
    val error: unit -> unit
    val printOne: unit -> unit
end

structure Parser :> PARSER =
struct
    val tokens = ref (Parse.parse "../lexer/program.simple")
    val tok = ref END

    fun getToken () =  case !tokens of
                        (t::ts) => (tokens := ts; t)
                        | [] => EOF

    fun printOne () = print("1")
    
    val tok = ref (getToken())
    fun error () = ()
    fun advance() = tok := getToken()
    fun eat(t) = if (!tok = t) then advance() else error()

    fun S() = case !tok of
        IF => (eat(IF); E(); eat(THEN); S(); eat(ELSE); S())
        | BEGIN => (eat(BEGIN); S(); L())
        | PRINT => (eat(PRINT); E())
        | _ => ()
    and L() = case !tok of
        END => (eat(END))
        | SEMI => (eat(SEMI); S(); L())
        | _ => ()
    and E() = (eat (NUM); eat(EQ); eat(NUM))

end

structure Test =
struct
    fun main (name, args) = 
        let
            val _ = Parser.advance()
        in
            1
        end
end