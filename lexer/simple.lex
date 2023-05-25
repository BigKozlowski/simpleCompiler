type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentDepth = ref 0
val str = ref ""
val strStart = ref 0
val stringClosed = ref true;
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = 
    let
        val pos = hd(!linePos)
    in
        if !commentDepth > 0
            then (ErrorMsg.error pos ("Unclosed comment"); commentDepth := 0; Tokens.EOF(pos, pos))
        else 
            let
                val res = (Tokens.EOF(pos, pos));
                val _ = lineNum := 0
                val _ = linePos := [];
                val _ = strStart := 0;
                val _ = stringClosed := true;
            in
                res
            end
    end

%% 

%s STRING COMMENT;
(* escapes = \\([nt\\"]|\^[@A-Z\[\\\]\^_]|[0-9]{3}|[\ \n\t\f]+\\); *)
printables = [\t\f]|[\ !#-\[]|[\]-~];

%%
<INITIAL,COMMENT>\n         => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>(" "|"\t")         => (continue());
<INITIAL>","                => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>":"                => (Tokens.COLON(yypos, yypos+1));
<INITIAL>";"                => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>"("                => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")"                => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"["                => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]"                => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"{"                => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}"                => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"."                => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"+"                => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-"                => (Tokens.MINUS(yypos, yypos-1));
<INITIAL>"*"                => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/"                => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"="                => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>"               => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"<"                => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<="               => (Tokens.LE(yypos, yypos+2));
<INITIAL>">"                => (Tokens.GT(yypos, yypos+1));
<INITIAL>">="               => (Tokens.GE(yypos, yypos+2));
<INITIAL>"&"                => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|"                => (Tokens.OR(yypos, yypos+1));
<INITIAL>":="               => (Tokens.ASSIGN(yypos, yypos+2));

<INITIAL>"type"             => (Tokens.TYPE(yypos, yypos + (size yytext)));
<INITIAL>var                => (Tokens.VAR(yypos, yypos + (size yytext)));
<INITIAL>function           => (Tokens.FUNCTION(yypos, yypos + (size yytext)));
<INITIAL>break              => (Tokens.BREAK(yypos, yypos + (size yytext)));
<INITIAL>of                 => (Tokens.OF(yypos, yypos + (size yytext)));
<INITIAL>end                => (Tokens.END(yypos, yypos + (size yytext)));
<INITIAL>in                 => (Tokens.IN(yypos, yypos + (size yytext)));
<INITIAL>nil                => (Tokens.NIL(yypos, yypos + (size yytext)));
<INITIAL>"let"              => (Tokens.LET(yypos, yypos + (size yytext)));
<INITIAL>do                 => (Tokens.DO(yypos, yypos + (size yytext)));
<INITIAL>to                 => (Tokens.TO(yypos, yypos + (size yytext)));
<INITIAL>for                => (Tokens.FOR(yypos, yypos + (size yytext)));
<INITIAL>while              => (Tokens.WHILE(yypos, yypos + (size yytext)));
<INITIAL>else               => (Tokens.ELSE(yypos, yypos + (size yytext)));
<INITIAL>then               => (Tokens.THEN(yypos, yypos + (size yytext)));
<INITIAL>if                 => (Tokens.IF(yypos, yypos + (size yytext)));
<INITIAL>array              => (Tokens.ARRAY(yypos, yypos + (size yytext)));

<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos + (size yytext)));
<INITIAL>[0-9]+             => (Tokens.INT (Option.valOf (Int.fromString yytext), yypos, yypos+(size yytext)));

<INITIAL>"\""               => (YYBEGIN STRING; str := ""; strStart := yypos; stringClosed := false; continue());
<STRING>({printables}|\\n|\\t|"\\\"") => (str := !str ^ valOf(String.fromString yytext); continue());
<STRING>"\""                => (YYBEGIN INITIAL; stringClosed := true; Tokens.STRING(!str, !strStart, yypos-1));
<STRING>.                   => (str := !str ^ yytext; continue());

<INITIAL>"/*"               => (commentDepth := !commentDepth + 1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"               => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>"/*"               => (commentDepth := !commentDepth + 1; continue());
<COMMENT>.                  => (continue());
.                           => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());