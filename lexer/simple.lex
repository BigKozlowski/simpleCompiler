type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ErrorMsg.commentDepth
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%% 

%s COMMENT;

%%
\n	            => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	            => (Tokens.COMMA(yypos,yypos+1));
var  	        => (Tokens.VAR(yypos,yypos+3));
"123"	        => (Tokens.INT(123,yypos,yypos+3));
.               => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<INITIAL>"/*"   => (commentDepth := !commentDepth + 1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"   => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>"/*"   => (commentDepth := !commentDepth + 1; continue());
<COMMENT>.      => (continue());