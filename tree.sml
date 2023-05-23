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

    fun balance LEAF = LEAF
    | balance (TREE(TREE(TREE(lll,(R,llk,llv),llr),(R,lk,lv),lr),(B,k,v),r)) = TREE(TREE(lll,(B,llk,llv),llr),(R,lk,lv),TREE(lr,(B,k,v),r))
    | balance (TREE(TREE(ll,(R,lk,lv),TREE(lrl,(R,lrk,lrv),lrr)),(B,k,v),r)) = TREE(TREE(ll,(B,lk,lv),lrl),(R,lrk,lrv),TREE(lrr,(B,k,v),r))
    | balance (TREE(l,(B,k,v),TREE(TREE(rll,(R,rlk,rlv),rlr),(R,rk,rv),rr))) = TREE(TREE(l,(B,k,v),rll),(R,rlk,rlv),TREE(rlr,(B,rk,rv),rr))
    | balance (TREE(l,(B,k,v),TREE(rl,(R,rk,rv),TREE(rrl,(R,rrk,rrv),rrr)))) = TREE(TREE(l,(B,k,v),rl),(R,rk,rv),TREE(rrl,(B,rrk,rrv),rrr))
    | balance tree = tree

    fun insert (tree, k, v) = 
        let
            fun insertInner LEAF = TREE (LEAF, (R, k, v), LEAF)
            | insertInner (TREE (l, (color, key, value), r)) =
                if k = key then TREE (l, (color, key, v), r)
                else if k < key then balance (TREE (insertInner l, (color, key, value), r))
                else balance (TREE (l, (color, key, value), insertInner r))

            val TREE (left, (_, key, value), right) = insertInner tree
        in
            TREE (left, (B, key, value), right)
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