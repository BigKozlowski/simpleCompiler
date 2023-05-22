type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
              | AssignStm of id * exp
              | PrintStm of exp list
      and exp = IdExp of id
              | NumExp of int
              | OpExp of exp * binop * exp
              | EseqExp of stm * exp

datatype token = Stm of stm | Exp of exp

type table = (id * int) list

val prog: stm = 
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
              CompoundStm(AssignStm("b",
                                    EseqExp(PrintStm[IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
                                    OpExp(NumExp 10, Times, IdExp "a"))),   
                          PrintStm[IdExp "b"]))

signature ASSIGNMENTS =
  sig
    val interpEach: token list -> table -> table
    val interpStm: table * stm -> table
    val interpExp: table * exp -> table * int
    val lookup: table * id -> int
    val update: table * id * int -> table
    val interp: stm -> unit
  end

  

structure Assignments :> ASSIGNMENTS =
  struct
    fun lookup (t, i) =
      case t of
        [] => 0
        | (x::xs) => 
          let val (id, v) = x
          in
            case id = i of
              true => v
              | false => lookup (xs, i)
          end
    
    fun update (t, i, v) =
      let 
        fun updateInner (t, i, v) res =
          case t of
            [] => (i, v)::t
            | (x::xs) =>
              let val (id, _) = x
              in
                case id = i of
                  true => res @ ((i, v)::xs)
                  | false => res @ (x::(updateInner (xs, i, v) []))
              end
      in 
        updateInner (t,i,v) []
      end

      fun interpEach (x::xs) t =
        let
          val newT = case x of
            Exp e => 
              let
                val (t1, vv) = interpExp(t, e)
                val _ = print (Int.toString vv)
                val _ = print "\n"
              in
                t1
              end
            | Stm s => 
              let
                val t1 = interpStm(t, s)
              in
                t1
              end
        in
          interpEach xs newT
        end
      | interpEach [] t = t
    and interpStm (t, s) =
      case s of
        AssignStm (id, exp) =>
          let
            val (t1, v) = interpExp(t, exp)
          in 
            update (t1, id, v)
          end
        | PrintStm xs =>
          let
            val t1 = interpEach (map (fn x => Exp x) xs) t
          in
            t1
          end
        | CompoundStm (l, r) => 
          let
            val t1 = interpStm(t, l)
          in 
            interpStm(t1, r)
          end
    and interpExp (t, e) =
      case e of
        IdExp id => (t, lookup(t, id))
        | NumExp num => (t, num)
        | OpExp (l, oper, r) => 
          let
            val (lt, left) = interpExp(t, l)
            val (rt, right) = interpExp(lt, r)
          in
            case oper of
              Plus => (rt, (left + right))
              | Minus => (rt, (left - right))
              | Times => (rt, (left * right))
              | Div => (rt, (left div right))
          end
        | EseqExp (s, ex) =>
          let
            val t1 = interpStm(t, s)
          in
            interpExp(t1, ex)
          end

    fun interp s =
      let val _ = interpStm ([], s)
      in
        ()
      end
  end



fun out () =
  let
    val newT = Assignments.interp prog
  in 
    1
  end

structure Test = struct
  fun main (name, args) =
    let
      val _ = print (Int.toString (out()))
      val _ = print "\n"
    in 
      1
    end
end