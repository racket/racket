#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module hw02 "../../typed-scheme.ss"
  (require "support.ss")
  ;;; --------------------------------------------------------------------
  ;;; Question 1
  
  ;; list-product : (list-of number) (list-of number) -> number
  ;; computes the dot-product of two lists (as vector representations)
  ;; (Assumes the two inputs are of equal length)
  (define: (list-product [l1 : (Listof number)] [l2 : (Listof number)]) : number
    (foldl #{+ :: (number number -> number)} 0 (map #{* :: (number number -> number)} l1 l2)))
  
  ;; tests
  (test (list-product '(1 2 3) '(4 5 6)) => 32)
  (test (list-product '() '()) => 0)
  
  
  ;;; --------------------------------------------------------------------
  ;;; Question 2
  
  #|

   <AE>   ::= <AE> + <fac>
            | <AE> - <fac>
            | <fac>

   <fac>  ::= <fac> * <atom>
            | <fac> / <atom>
            | <atom>

   <atom> ::= <number>
            | + <atom>
            | - <atom>
            | ( <AE> )

   In the rules for <atom>, note that any number of unary +/-
   operators can be used, and that the resulting grammar is not
   ambiguous since they are put in yet another level below <fac> (they
   have higher precedence than the binary operators).

|#
  
  
  ;;; --------------------------------------------------------------------
  ;;; Question 3
  
  ;; [3a]
  (define-type BINTREE
    [Node (l BINTREE) (r BINTREE)]
    [Leaf (n number)])
  
  ;; used for tests:
  (define: 1234-tree : BINTREE
    (Node (Node (Leaf 1) (Leaf 2))
          (Node (Leaf 3) (Leaf 4))))
  (define: 528-tree : BINTREE (Node (Leaf 5) (Node (Leaf 2) (Leaf 8))))
  #;(provide (all-defined))
  #| [3b] BNF:

   <BINTREE> ::= <Node> | <Leaf>

   <Node> ::= (Node <BINTREE> <BINTREE>)

   <Leaf> ::= (Leaf <num>)

|#
  
  ;; [3c]
  
  ;; tree-reduce : BINTREE (num num -> num) -> num
  ;; Reduces a BINTREE to a number by descending recursively and combining
  ;; results with `op'.
  (define: (tree-reduce [tree : BINTREE] [op : (number number -> number)]) : number
    (cases tree
      [(Node l r) (op (tree-reduce l op) (tree-reduce r op))]
      [(Leaf n) n]))
  ;; tests:
  (test 10 <= (tree-reduce 1234-tree +))
  (test 10 <= (tree-reduce (Leaf 10) +))
  (test 24 <= (tree-reduce 1234-tree *))
  
  ;; tree-min : BINTREE -> num
  ;; Finds the minimum number in a BINTREE.
  (define: (tree-min [tree : BINTREE]) : number
    (tree-reduce tree min))
  ;; tests:
  (test 1 <= (tree-min 1234-tree))
  (test 1 <= (tree-min (Leaf 1)))
  
  ;; tree-min : BINTREE -> num
  ;; Finds the maximum number in a BINTREE.
  (define: (tree-max [tree : BINTREE]) : number
    (tree-reduce tree max))
  ;; tests:
  (test 4 <= (tree-max 1234-tree))
  (test 1 <= (tree-max (Leaf 1)))
  
  ;; tree-sorted? : BINTREE -> bool
  ;; Tests whether the tree is sorted or not.
  (define: (tree-sorted? [tree : BINTREE]) : boolean
    (cases tree
      [(Node l r) (and (tree-sorted? l)
                       (tree-sorted? r)
                       (<= (tree-max l) (tree-min r)))]
      [(Leaf n) #t]))
  ;; tests:
  (test (tree-sorted? 1234-tree))
  (test (tree-sorted? (Leaf 1)))
  (test (not (tree-sorted? 528-tree)))
  #|
  #| [3d]

   Say that the cost function is cost(n) for a tree with n leaves, and
   that we're given a balanced tree of 32 leaves.  We have:

     cost(32) = 16         ; for finding the max the left side
                + 16       ; for finding the min the right side
                + cost(16) ; for the recursive call on the left
                + cost(16) ; for the recursive call on the right
                + 1        ; some constant for the `and' and the `<'

   In general, we can drop the last one since it doesn't matter and get:

     cost(n) = 2*(n/2) + 2*cost(n/2) = n + 2*cost(n/2)

   and

     cost(1) = 1

   Continueing with the case of 32:

     cost(32) = 32 + 2*cost(16)
              = 32 + 2*(16 + 2*cost(8))
              = 32 + 32 + 4*cost(8)
              = 32 + 32 + 32 + 8*cost(4)
              = 32 + 32 + 32 + 32 + 16*cost(2)
              = 32 + 32 + 32 + 32 + 32 + 32*cost(1)
              = 32 + 32 + 32 + 32 + 32 + 32

   So the total cost for n leaves is n*log2(n).

|#
  
  ;; 3e
  
  ;; tree-sorted*? : BINTREE -> bool
  ;; Tests whether the tree is sorted or not in linear time.
  ;; -- The trick is to check for sortedness by recursively walking the
  ;;    tree and remembering the last value we have seen and making sure
  ;;    that new leaves are always bigger.  The return value of the helper
  ;;    is either the right-most value if it is sorted, or #f if not.
  (define (tree-sorted*? tree)
    ;; `and' is used to turn the result into a proper boolean
    (and (sorted*?-helper
          tree
          (- (left-most-value tree) 1)) ; initialize a last value
         #t))
  
  ;; left-most-value : BINTREE -> num
  ;; Finds the left-most value in a BINTREE.
  (define (left-most-value tree)
    (cases tree
      [(Leaf n) n]
      [(Node l r) (left-most-value l)]))
  
  ;; sorted*?-helper : BINTREE num -> bool-or-num
  ;; Helper for the above -- checks that the given tree is sorted and
  ;; bigger than the given number, and returns the right-most number if it
  ;; is sorted.
  (define (sorted*?-helper tree last)
    (cases tree
      [(Leaf n)
       (and (< last n) n)]
      [(Node l r)
       (let ([left-last (sorted*?-helper l last)])
         (and left-last (sorted*?-helper r left-last)))]))
  
  ;; tests:
  (test (tree-sorted*? 1234-tree))
  (test (tree-sorted*? (Leaf 1)))
  (test (not (tree-sorted*? 528-tree)))
  
  
  ;;; --------------------------------------------------------------------
  ;;; Question 4
  
  ;; tree-map : (num -> num) BINTREE -> BINTREE
  ;; Maps the given function recursively over the given tree, returning a
  ;; tree of the results with the same shape.
  (define (tree-map f tree)
    (cases tree
      [(Leaf n) (Leaf (f n))]
      [(Node l r) (Node (tree-map f l) (tree-map f r))]))
  
  ;; tests
  (test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
        => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
  (test (tree-map add1 1234-tree)
        => (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 5))))
  (test (tree-map add1 (Leaf 1))
        => (Leaf 2))
  
  
  ;;; --------------------------------------------------------------------
  ;;; Question 5
  
  ;; tree-insert : BINTREE num -> BINTREE
  (define (tree-insert tree n)
    (cases tree
      [(Leaf m)   (if (< n m)
                      (Node (Leaf n) tree)
                      (Node tree (Leaf n)))]
      [(Node l r) (if (< n (tree-max l))
                      (Node (tree-insert l n) r)
                      (Node l (tree-insert r n)))]))
  
  ;; tests:
  (test (tree-sorted?
         (tree-insert (Node (Leaf 2) (Node (Leaf 4) (Leaf 6)))
                      3)))
  (test (tree-sorted? (tree-insert 1234-tree 0)))
  (test (tree-sorted? (tree-insert 1234-tree 5)))
  
  
  ;;; --------------------------------------------------------------------
  ;;; Question 6
  
  #|

   The problem is that we need to keep both flattened copies in memory
   for the comparison.  This means that if we have two big trees, say
   200MB each, then during the comparison we will need to have 800MB of
   RAM!  The solution for this is very hard for now, but later in the
   course we will see one easy way to solve it.

|#|#

  )
