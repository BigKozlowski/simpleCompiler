datatype token = IF | THEN | ELSE | BEGIN | END | PRINT | SEMI | NUM | EQ

val tok = ref (getToken())
fun advance() = tok := getToken()
fun eat(t) = if (!tok = t) then advance() else error()

fun S = case !tok of
    IF => (eat(IF); E(); eat(THEN); S(); eat(ELSE); S())
    | BEGIN => (eat(BEGIN); S(); L())
    | PRINT => (eac(PRINT); E())
and L() = case !tok of
    END => (eat(END))
    | SEMI => (eat(SEMI); S(); L())
and E() = (eat (NUM); eat(EQ); eat(NUM))