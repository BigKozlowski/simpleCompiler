structure Tokens =
struct
  type pos = int
  datatype token = EOF of pos * pos
                 | IF of pos * pos
                 | IF of string * pos * pos
                 | NUM of int * pos * pos
                 | REAL of real * pos * pos
end

structure Test =
struct
  fun main (name, args) =
    let
        val _ = print "TEST\n"
    in
        1
    end
end