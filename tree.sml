type key = string
datatype color = R | B
type 'a node = color * key * 'a
datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree

signature TREE =
sig
  val insert: 'a tree * key * 'a -> 'a tree
  val lookup: 'a tree * key -> 'a tree
  val insertList: (key * 'a) list -> 'a tree -> 'a tree
  val balance: 'a tree -> 'a tree
end

structure Tree :> TREE =
struct
    val empty = LEAF

    fun insert (LEAF, key, v) = TREE (LEAF, (R, key, v), LEAF)
    | insert (tree, key, v) = 
        let
            val _ = print "INSERT NOT IMPLEMENTED\n"
        in
            empty
        end

    fun lookup (LEAF, _) = LEAF
    | lookup (tree, k) = 
        let 
            val TREE (l, b, r) = tree
            val (_, key, value) = b
        in
            if k = key
                then tree
            else if k < key
                then lookup(l, k)
            else lookup(r, k)
        end

    fun insertList [] tree = tree
    | insertList ((key, v)::xs) tree = insertList xs (insert(tree, key, v)) 
    
    fun balance LEAF = LEAF
    | balance tree = 
        let
            val _ = print "BALANCE NOT IMPLEMENTED\n"
        in
            empty
        end
    
end