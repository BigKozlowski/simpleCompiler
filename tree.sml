type key = string
datatype color = R | B
type 'a node = color * key * 'a
datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree

signature TREE =
sig
  val insert: 'a tree * key * 'a -> 'a tree
  val lookup: int tree * key -> int
  val insertList: (key * 'a) list -> 'a tree -> 'a tree
  val balance: 'a tree -> 'a tree
end

structure Tree :> TREE =
struct
    val empty = LEAF
    fun insert (tree, key, v) = 
        let
            val _ = print "INSERT NOT IMPLEMENTED\n"
        in
            empty
        end

    fun lookup (tree, key) = 
        let
            val _ = print "LOOKUP NOT IMPLEMENTED\n"
        in
            0
        end

    fun insertList list tree =
        let
            val _ = print "INSERTLIST NOT IMPLEMENTED\n"
        in
            empty
        end
    
    fun balance tree = 
        let
            val _ = print "BALANCE NOT IMPLEMENTED\n"
        in
            empty
        end
    
end