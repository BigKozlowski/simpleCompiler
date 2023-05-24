type lexresult = Tokens.token
fun eof () = Tokens.EOF(0,0)
%%
digits=[0-9]+
%%
if                                      => (Tokens.IF(yypos, yypos+2));
[a-z][a-z0-9]*                          => (Tokens.ID(yytext, yypos, yypos+size yytext));
{digits}                                => (Tokens.NUM(Int.fromString yytext, yypos, yypos + size yytext));
({digits}"."[0-9]* )|([0-9]*"."{digits}) => Tokens.REAL(Real.fromString yytext, yypos, yypos + size yytext);
("--"[a-z]*"\n")|(" "|"\n"|"\t")        => (continue())
                                        => (ErrorMsg.error yypos "illegal character";
                                            continue());