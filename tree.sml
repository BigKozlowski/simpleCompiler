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
  val rotateLeft: 'a tree -> 'a tree
  val rotateRight: 'a tree -> 'a tree
end

structure Tree :> TREE =
struct
    val empty = LEAF

    fun rotateLeft LEAF = LEAF
    | rotateLeft tree =
        let
            val _ = print "ROTATELEFT NOT IMPLEMENTED\n"
        in
            tree
        end
    
    fun rotateRight LEAF = LEAF
    | rotateRight tree = 
        let
            val _ = print "ROTATERIGHT NOT IMPLEMENTED\n"
        in
            tree
        end

    fun balance LEAF = LEAF
    | balance tree = 
        let
            val _ = print "BALANCE NOT IMPLEMENTED\n"
        in
            tree
        end
    
    fun insert (LEAF, key, v) = TREE (LEAF, (R, key, v), LEAF)
    | insert (tree, k, v) = 
        let
            val TREE (l, (color, key, value), r) = tree
            val newTree = if k = key
                            then TREE (l, (color, key, v), r)
                        else if k < key
                            then TREE (insert(l, k, v), (color, key, value), r)
                        else TREE (l, (color, key, value), insert(r, k, v))
            val res = balance newTree
        in
            res
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

end