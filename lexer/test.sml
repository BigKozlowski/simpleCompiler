(*  REGEXP NOTATION:
    M* = "" | M | MM | MMM | ...
    [abcd] = a | b | c | d
    [b-g] = [bcdefg]
    [b-gM-Qkr] = [bcdefgMNOPQkr]
    M? = (M | "") 
    M+ = ( M.M* )
    M(.)N = MN 
    . = any single char except newline
    "smth" = string in quotes stands for itself *)

(* REGEXP EXAMPLES:
    if = (IF)
    [a-z][a-z0-9]* = (ID)
    [0-9]+ = (NUM)
    ([0-9]+"."[0-9]* ) | ([0-9]*"."[0-9]+) = (REAL)
    ("--"[a-z]*"\n") | ("  "|"\n"|"\t")+ = (continue())
    . = (error(); continue()); *)

structure Test =
struct
  fun main (name, (a1::_)) =
    let
        val tokens = Parse.parse "test.tig"
    in
        1
    end
end