type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentDepth = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%% 

%s COMMENT;

(* =<><<=>>=&! : = *)

%%
"123"	                    => (Tokens.INT(123,yypos,yypos+3));
<INITIAL>\n                 => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>.                  => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL>(" "|"\n"|"\t")    => (continue());
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

<INITIAL>var                => (Tokens.VAR(yypos, yypos + (size yytext)));
<INITIAL>"/*"               => (commentDepth := !commentDepth + 1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"               => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>"/*"               => (commentDepth := !commentDepth + 1; continue());
<COMMENT>.                  => (continue());