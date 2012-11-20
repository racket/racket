#lang racket/base
(require (for-syntax racket/base))

;; Implementation of an augmented red-black tree, where extra
;; information supports position-based queries.
;;
;; Author: Danny Yoo (dyoo@hashcollision.org)
;;
;; 
;; The usage case of this structure is to maintain an ordered sequence
;; of items.  Each item has an internal length.  We want to support
;; quick lookup by position, as well as the catenation and splitting
;; of the sequence.
;;
;; These operations are typical of an editor's buffer, which must
;; maintain a sequence of tokens in order, allowing for arbitrary
;; search, insert, and delete into the sequence.
;;
;;
;;
;; We follow the basic outline for order-statistic trees described in
;; CLRS.
;;
;;     Cormen, Leiserson, Rivest, Stein.  Introduction to Algorithms, 3rd edition.
;;     http://mitpress.mit.edu/books/introduction-algorithms
;;
;; In our case, each node remembers the total width of its
;; subtree.  This allows us to perform search-by-position very
;; quickly.
;;
;; We also incorporate some elements of the design in:
;;
;;     Ron Wein.  Efficient implemenation of red-black trees with
;;     split and catenate operations.  (2005)
;;     http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.109.4875
;;
;; where we keep track of the first and last pointers.  My implementation of
;; split is, in my opinion, easier to read, as we go bottom-up rather than
;; top-down, without the weird need for two separate pivots.
;;
;;
;; This module has test cases in a test submodule below.
;;
;; Use: 
;;
;;     raco test red-black.rkt to execute these tests.
;;


(provide tree?
         tree-root
         tree-first
         tree-last
         node?
         nil
         nil?
         node-data
         node-self-width
         node-subtree-width
         node-parent
         node-left
         node-right
         node-color
         red?
         black?
         
         new-tree
         new-node
         insert-first!
         insert-before!
         insert-after!
         insert-first/data!
         insert-last/data!
         insert-before/data!
         insert-after/data!
         
         delete!
         join!
         split!
 
         search

         minimum
         maximum
         successor
         predecessor
         position
         
         tree-items)


;; First, our data structures:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define red 'red)
(define black 'black)


(struct tree (root  ;; node     The root node of the tree.
              first ;; node     optimization: Points to the first element.
              last  ;; node     optimization: Points to the last element.
              bh)   ;; natural  optimization: The black height of the entire tree.
  #:mutable)


(struct node (data          ;; Any
              self-width    ;; Natural
              subtree-width ;; Natural
              parent     ;; node
              left       ;; node
              right      ;; node
              color)     ;; (U red black)
  #:mutable)


;; As in CLRS, we use a single nil sentinel node that represents nil.
(define nil (let ([v (node #f 0 0 #f #f #f black)])
              (set-node-parent! v v)
              (set-node-left! v v)
              (set-node-right! v v)
              v))

;; nil?: node -> boolean
;; Tell us if we're at the distinguished nil node.
(define-syntax-rule (nil? x) (eq? x nil))

;; red?: node -> boolean
;; Is the node red?
(define-syntax-rule (red? x) 
  (let ([v x])
    (eq? (node-color v) red)))

;; black?: node -> boolean
;; Is the node black?
(define-syntax-rule (black? x) 
  (let ([v x])
    (eq? (node-color v) black)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Next, the operations:


;; new-tree: -> tree
;; Creates a fresh tree.
(define (new-tree)
  (tree nil nil nil 0))


;; new-node: -> node
;; Creates a new singleton node.
(define (new-node data width)
  (node data width width nil nil nil red))


;; minimum: node -> node
;; Looks for the minimum element of the tree rooted at n.
(define (minimum n)
  (let loop ([n n])
    (define left (node-left n))
    (cond
      [(nil? left)
       n]
      [else
       (loop left)])))

;; maximum: node -> node
;; Looks for the maximum element of the tree rooted at n.
(define (maximum n)
  (let loop ([n n])
    (define right (node-right n))
    (cond
      [(nil? right)
       n]
      [else
       (loop right)])))


;; successor: node -> node
;; Given a node, walks to the successor node.
;; If there is no successor, returns the nil node.
(define (successor x)
  (cond [(not (nil? (node-right x)))
         (minimum (node-right x))]
        [else
         (let loop ([x x]
                    [y (node-parent x)])
           (cond
             [(and (not (nil? y)) (eq? x (node-right y)))
              (loop y (node-parent y))]
             [else
              y]))]))

;; predecessor: node -> node
;; Given a node, walks to the predecessor node.
;; If there is no predecessor, returns the nil node.
(define (predecessor x)
  (cond [(not (nil? (node-left x)))
         (maximum (node-left x))]
        [else
         (let loop ([x x]
                    [y (node-parent x)])
           (cond
             [(and (not (nil? y)) (eq? x (node-left y)))
              (loop y (node-parent y))]
             [else
              y]))]))


;; update-node-subtree-width!: node -> void
;; INTERNAL
;; Assuming the node-subtree-width of the left and right are
;; correct, computes the subtree-width of n and updates a-node.
;;
;; Note: this does not trust the local cache in (node-subtree-width
;; n), but does trust node-subtree-width of the left and right
;; subtrees.
(define (update-node-subtree-width! a-node)
  (let* ([n a-node]
         [left (node-left n)]
         [right (node-right n)])
    (set-node-subtree-width! a-node
                             (+ (node-subtree-width left) 
                                (node-self-width n)
                                (node-subtree-width right)))))



;; update-subtree-width-up-to-root!: node -> void
;; INTERNAL
;; Updates the subtree width statistic from a-node upward to the root.
;;
;; * The subtree width field of a-node and its ancestors should be updated.
(define (update-subtree-width-up-to-root! a-node)
  (let loop ([n a-node])
    (cond
      [(nil? n)
       (void)]
      [else
       (update-node-subtree-width! n)
       (loop (node-parent n))])))



;; insert-first!: tree (and/c (not nil?) node?) -> void
;; Insert node x as the first element in the tree.
;; x is assumed to be a singleton element whose fields
;; are valid.
(define (insert-first! a-tree x)
  (set-node-color! x red)
  (cond
    [(nil? (tree-root a-tree))
     (set-tree-root! a-tree x)
     (set-tree-first! a-tree x)
     (set-tree-last! a-tree x)]
    [else
     (define first (tree-first a-tree))
     (set-node-left! first x)
     (set-node-parent! x first)
     (set-tree-first! a-tree x)])
  (update-subtree-width-up-to-root! (node-parent x))
  (fix-after-insert! a-tree x))



;; insert-last!: tree (and/c (not nil?) node?) -> void
;; Insert node x as the last element in the tree.
;; x is assumed to be a singleton element whose fields
;; are valid.
(define (insert-last! a-tree x)
  (set-node-color! x red)
  (cond
    [(nil? (tree-root a-tree))
     (set-tree-root! a-tree x)
     (set-tree-first! a-tree x)
     (set-tree-last! a-tree x)]
    [else
     (define last (tree-last a-tree))
     (set-node-right! last x)
     (set-node-parent! x last)
     (set-tree-last! a-tree x)])
  (update-subtree-width-up-to-root! (node-parent x))
  (fix-after-insert! a-tree x))


;; insert-before!: tree node (and/c (not nil?) node?) -> void
;; Insert node x before element 'before' of the tree.
;; x will be the immmediate predecessor of before upon completion.
;; x is assumed to be a singleton element whose fields
;; are valid.
(define (insert-before! a-tree before x)
  (cond
    [(nil? (node-left before))
     (set-node-left! before x)
     (set-node-parent! x before)]
    [else    
     (define y (maximum (node-left before)))
     (set-node-right! y x)
     (set-node-parent! x y)])  
  (set-node-color! x red)
  (when (eq? before (tree-first a-tree))
    (set-tree-first! a-tree x))
  (update-subtree-width-up-to-root! (node-parent x))
  (fix-after-insert! a-tree x))


;; insert-after!: tree node (and/c (not nil?) node?) -> void
;; Insert node x after element 'after' of the tree.
;; x will be the immmediate successor of after upon completion.
;; x is assumed to be a singleton element whose fields
;; are valid.
(define (insert-after! a-tree after x)
  (cond
    [(nil? (node-right after))
     (set-node-right! after x)
     (set-node-parent! x after)]
    [else    
     (define y (minimum (node-right after)))
     (set-node-left! y x)
     (set-node-parent! x y)])
  (set-node-color! x red)
  (when (eq? after (tree-last a-tree))
    (set-tree-last! a-tree x))
  (update-subtree-width-up-to-root! (node-parent x))
  (fix-after-insert! a-tree x))


;; insert-first/data!: tree data width -> void
;; Insert before the first element of the tree.
(define (insert-first/data! a-tree data width)
  (define x (new-node data width))
  (insert-first! a-tree x))


;; insert-last/data!: tree data width -> void
;; Insert after the last element in the tree.
(define (insert-last/data! a-tree data width)
  (define x (new-node data width))
  (insert-last! a-tree x))


;; insert-before/data!: tree data width -> void
;; Insert before the first element of the tree.
(define (insert-before/data! a-tree n data width)
  (define x (new-node data width))
  (insert-before! a-tree n x))


;; insert-after/data!: tree node data width -> void
;; Insert after the last element in the tree.
(define (insert-after/data! a-tree n data width)
  (define x (new-node data width))
  (insert-after! a-tree n x))




;; left-rotate!: tree node natural -> void
;; INTERNAL
;; Rotates the x node node to the left.
;; Preserves the auxiliary information for position queries.
(define (left-rotate! a-tree x)
  (define y (node-right x))
  (set-node-right! x (node-left y))
  (unless (nil? (node-left y))
    (set-node-parent! (node-left y) x))
  (set-node-parent! y (node-parent x))
  (cond [(nil? (node-parent x))
         (set-tree-root! a-tree y)]
        [(eq? x (node-left (node-parent x)))
         (set-node-left! (node-parent x) y)]
        [else
         (set-node-right! (node-parent x) y)])
  (set-node-left! y x)
  (set-node-parent! x y)
  
  ;; Looking at Figure 1.32 of CLRS:
  ;; The change to the statistics can be locally computed after the
  ;; rotation:
  (set-node-subtree-width! y (node-subtree-width x))
  (update-node-subtree-width! x))


;; right-rotate!: tree node natural -> void
;; INTERNAL
;; Rotates the y node node to the right.
;; (Symmetric to the left-rotate! function.)
;; Preserves the auxiliary information for position queries.
(define (right-rotate! a-tree y)
  (define x (node-left y))
  (set-node-left! y (node-right x))
  (unless (nil? (node-right x))
    (set-node-parent! (node-right x) y))
  (set-node-parent! x (node-parent y))
  (cond [(nil? (node-parent y))
         (set-tree-root! a-tree x)]
        [(eq? y (node-right (node-parent y)))
         (set-node-right! (node-parent y) x)]
        [else
         (set-node-left! (node-parent y) x)])
  (set-node-right! x y)
  (set-node-parent! y x)
  
  ;; Looking at Figure 1.32 of CLRS:
  ;; The change to the statistics can be locally computed after the
  ;; rotation:
  (set-node-subtree-width! x (node-subtree-width y))
  (update-node-subtree-width! y))


;; fix-after-insert!: tree node natural -> void
;; INTERNAL
;; Corrects the red/black tree property via node rotations after an
;; insertion.  If there's a violation, then it's at z where both z and
;; its parent are red.
(define (fix-after-insert! a-tree z)
  (let loop ([z z])
    (define z.p (node-parent z))
    (when (red? z.p)
      (define z.p.p (node-parent z.p))
      (cond [(eq? z.p (node-left z.p.p))
             (define y (node-right z.p.p))
             (cond [(red? y)
                    (set-node-color! z.p black)
                    (set-node-color! y black)
                    (set-node-color! z.p.p red)
                    (loop z.p.p)]
                   [else
                    (cond [(eq? z (node-right z.p))
                           (let ([new-z z.p])
                             (left-rotate! a-tree new-z)
                             (set-node-color! (node-parent new-z) black)
                             (set-node-color! (node-parent (node-parent new-z)) red)
                             (right-rotate! a-tree (node-parent (node-parent new-z)))
                             (loop new-z))]
                          [else
                           (set-node-color! z.p black)
                           (set-node-color! z.p.p red)
                           (right-rotate! a-tree z.p.p)
                           (loop z)])])]
            [else
             (define y (node-left z.p.p))
             (cond [(red? y)
                    (set-node-color! z.p black)
                    (set-node-color! y black)
                    (set-node-color! z.p.p red)
                    (loop z.p.p)]
                   [else
                    (cond [(eq? z (node-left z.p))
                           (let ([new-z z.p])
                             (right-rotate! a-tree new-z)
                             (set-node-color! (node-parent new-z) black)
                             (set-node-color! (node-parent (node-parent new-z)) red)
                             (left-rotate! a-tree 
                                           (node-parent (node-parent new-z)))
                             (loop new-z))]
                          [else
                           (set-node-color! z.p black)
                           (set-node-color! z.p.p red)
                           (left-rotate! a-tree z.p.p)
                           (loop z)])])])))
  (when (red? (tree-root a-tree))
    (set-tree-bh! a-tree (add1 (tree-bh a-tree)))
    (set-node-color! (tree-root a-tree) black)))




;; delete!: tree node -> void
;; Removes the node from the tree.
;; Follows the description of CLRS, but with a few extensions:
;;
;;    * Importantly, we do not ever mutate the sentinel nil node's parent.
;;      This means we takes special care of remembering what it should be,
;;      so that the deletion fixup can properly walk the tree.
;;      We do this so that we can preserve thread-safety: the nil node is global,
;;      so mutating it is a Bad Thing.
;;
;;    * Does the statistic update, following the strategy of augmented
;;      red-black trees.
(define (delete! a-tree z)
  
  ;; First, adjust tree-first and tree-last if we end up
  ;; removing either from the tree.
  (when (eq? z (tree-first a-tree))
    (set-tree-first! a-tree (successor z)))
  (when (eq? z (tree-last a-tree))
    (set-tree-last! a-tree (predecessor z)))
  
  (define y z)
  (define y-original-color (node-color y))
  (let-values ([(x y-original-color nil-parent)
                (cond
                  ;; If either the left or right child of z is nil,
                  ;; deletion is merely replacing z with its other child x.
                  ;; (Of course, we then have to repair the damage.)
                  [(nil? (node-left z))
                   (define z.p (node-parent z))
                   (define x (node-right z))
                   (define nil-parent (transplant-for-delete! a-tree z x))
                   ;; At this point, we need to repair the statistic where
                   ;; where the replacement happened, since z's been replaced with x.
                   ;; The x subtree is ok, so we need to begin the statistic repair
                   ;; at z.p.
                   (when (not (nil? z.p))
                     (update-subtree-width-up-to-root! z.p))
                   (values x y-original-color nil-parent)]
                  
                  ;; This case is symmetric with the previous case.
                  [(nil? (node-right z))
                   (define z.p (node-parent z))
                   (define x (node-left z))
                   (define nil-parent (transplant-for-delete! a-tree z x))
                   (when (not (nil? z.p))
                     (update-subtree-width-up-to-root! z.p))
                   (values x y-original-color nil-parent)]
                  
                  ;; The hardest case is when z has non-nil left and right.
                  ;; We take the minimum of z's right subtree and replace
                  ;; z with it.
                  [else
                   (let* ([y (minimum (node-right z))]
                          [y-original-color (node-color y)])
                     ;; At this point, y's left is nil by definition of minimum.
                     (define x (node-right y))
                     (define nil-parent
                       (cond
                         [(eq? (node-parent y) z)
                          ;; In CLRS, this is steps 12 and 13 of RB-DELETE.
                          ;; Be aware that x here can be nil.  Rather than
                          ;; change the contents of nil, we record that value
                          ;; in nil-parent and pass that along to the rest of
                          ;; the computation.
                          (cond [(nil? x)
                                 y]
                                [else
                                 (set-node-parent! x y)
                                 nil])]
                         [else
                          (let ([nil-parent
                                 (transplant-for-delete! a-tree y (node-right y))])
                            (set-node-right! y (node-right z))
                            (set-node-parent! (node-right y) y)
                            nil-parent)]))
                     
                     ;; y can't be nil here, so has no effect on nil-parent
                     (transplant-for-delete! a-tree z y)
                     
                     (set-node-left! y (node-left z))
                     
                     ;; Similarly, (node-left y) here can't be nil by the case
                     ;; analysis, so this has no effect on nil-parent.
                     (set-node-parent! (node-left y) y)
                     
                     (set-node-color! y (node-color z))
                     (update-subtree-width-up-to-root! 
                      (if (nil? x) nil-parent (node-parent x)))
                     (values x y-original-color nil-parent))])])
    (cond [(eq? black y-original-color)
           (fix-after-delete! a-tree x nil-parent)]
          [else
           (void)])))
    
   





;; transplant-for-delete: tree node (U node nil) -> (U nil node)
;; INTERNAL
;; Replaces the instance of node u in a-tree with v.
;;
;; If v is nil, then rather than mutate nil, it returns
;; a non-nil node that should be treated as nil's parent.
;; Otherwise, returns nil.
(define (transplant-for-delete! a-tree u v)
  (define u.p (node-parent u))
  (cond [(nil? u.p)
         (set-tree-root! a-tree v)]
        [(eq? u (node-left u.p))
         (set-node-left! u.p v)]
        [else
         (set-node-right! u.p v)])
  (cond [(nil? v)
         u.p]
        [else
         (set-node-parent! v u.p)
         nil]))



;; fix-after-delete!: tree node -> void
;; INTERNAL
;; Correct several possible invariant-breaks after a black node
;; has been removed from the tree.  These include:
;;
;; * turning the root red
;; * unbalanced black paths
;; * red-red links
;; 
;; Note that this function has been augmented so that it keeps special
;; track of nil-parent.
(define (fix-after-delete! a-tree x nil-parent)

  ;; n-p: node -> node
  ;; Should be almost exactly like node-parent, except for one
  ;; special case: in the context of this function alone, 
  ;; if we're navigating the parent of nil, use the nil-parent constant.
  (define-syntax-rule (n-p x)
    (let ([v x])
      (if (nil? v)
          nil-parent
          (node-parent v))))

  (let loop ([x x]
             [early-escape? #f])
    (cond [(and (not (eq? x (tree-root a-tree)))
                (black? x))
           (cond
             [(eq? x (node-left (n-p x)))
              (define w (node-right (n-p x)))
              (define w-1 (cond [(eq? (node-color w) red)
                                 (set-node-color! w black)
                                 (set-node-color! (n-p x) red)
                                 (left-rotate! a-tree (n-p x))
                                 (node-right (n-p x))]
                                [else
                                 w]))
              (cond [(and (black? (node-left w-1)) (black? (node-right w-1)))
                     (set-node-color! w-1 red)
                     (loop (n-p x) #f)]
                    [else
                     (define w-2 (cond [(black? (node-right w-1))
                                        (set-node-color! (node-left w-1) black)
                                        (set-node-color! w-1 red)
                                        (right-rotate! a-tree w-1)
                                        (node-right (n-p x))]
                                       [else
                                        w-1]))
                     (set-node-color! w-2 (node-color (n-p x)))
                     (set-node-color! (n-p x) black)
                     (set-node-color! (node-right w-2) black)
                     (left-rotate! a-tree (n-p x))
                     (loop (tree-root a-tree) 
                           #t)])]
             [else
              (define w (node-left (n-p x)))
              (define w-1 (cond [(red? w)
                                 (set-node-color! w black)
                                 (set-node-color! (n-p x) red)
                                 (right-rotate! a-tree (n-p x))
                                 (node-left (n-p x))]
                                [else
                                 w]))
              (cond [(and (black? (node-left w-1)) (black? (node-right w-1)))
                     (set-node-color! w-1 red)
                     (loop (n-p x) #f)]
                    [else
                     (define w-2 (cond [(black? (node-left w-1))
                                        (set-node-color! (node-right w-1) black)
                                        (set-node-color! w-1 red)
                                        (left-rotate! a-tree w-1)
                                        (node-left (n-p x))]
                                       [else
                                        w-1]))
                     (set-node-color! w-2 (node-color (n-p x)))
                     (set-node-color! (n-p x) black)
                     (set-node-color! (node-left w-2) black)
                     (right-rotate! a-tree (n-p x))
                     (loop (tree-root a-tree) 
                           #t)])])]
          [else
           ;; When we get to this point, if x is at the root
           ;; and still black, we are discarding the double-black
           ;; color, which means the height of the tree should be
           ;; decremented.
           (when (and (eq? x (tree-root a-tree))
                      (black? x)
                      (not early-escape?))
             (set-tree-bh! a-tree (sub1 (tree-bh a-tree))))
           
           (set-node-color! x black)])))



;; search: tree natural -> (U node nil)
;; Search for the node closest to offset.
;; The total length of the left subtree will be at least offset, if possible.
;; Returns nil if the offset is not within the tree.
(define (search a-tree offset)
  (let loop ([offset offset]
             [a-node (tree-root a-tree)])
    (cond
      [(nil? a-node) nil]
      [else
       (define left (node-left a-node))
       (define left-subtree-width (node-subtree-width left))
       (cond [(< offset left-subtree-width)
              (loop offset left)]
             [else 
              (define residual-offset (- offset left-subtree-width))
              (define self-width (node-self-width a-node))
              (cond
                [(< residual-offset self-width)
                 a-node]
                [else
                 (loop (- residual-offset self-width)
                       (node-right a-node))])])])))


;; position: node -> (or natural -1)
;; Given a node in the tree, returns its position such that
;; a search in the tree with that position will return the node.
;; Note: (position nil) will return -1.
(define (position n)
  (cond 
    [(nil? n)
     -1]
    [else
     (let loop ([n n]
                [came-from-right? #f]
                [acc (node-subtree-width (node-left n))])
       (cond
         [(nil? n)
          acc]
         [came-from-right?
          (loop (node-parent n)
                (eq? (node-right (node-parent n)) n)
                (+ acc
                   (node-subtree-width (node-left n))
                   (node-self-width n)))]
         [else
          (loop (node-parent n)
                (eq? (node-right (node-parent n)) n)
                acc)]))]))




;; join!: tree tree -> tree
;; Destructively concatenates trees t1 and t2, and
;; returns a tree that represents the join.
(define (join! t1 t2)
  (cond
    [(nil? (tree-root t2))
     t1]
    [(nil? (tree-root t1))
     t2]
    [else
     ;; First, remove element x from t2.  x will act as the
     ;; pivot point.
     (define x (tree-first t2))
     (delete! t2 x)
     ;; Next, delegate to the more general concat! function, using
     ;; x as the pivot.
     (concat! t1 x t2)]))


;; concat!: tree node tree -> tree
;; INTERNAL
;; Joins t1, x and t2 together, using x as the pivot.
;;
;; x will be treated as if it were a singleton tree; its x.left and x.right
;; will be overwritten during concatenation.
;;
;; Note that x must NOT be attached to the tree t1 or t2, or else this
;; will introduce an illegal cycle.
;;
;; Also, this should not depend on t1 and t2 having valid
;; tree-first/tree-last pointers on entry; we compute this lazily due
;; to how this is used by split!.
(define (concat! t1 x t2)
  (cond

    [(nil? (tree-root t1))
     (set-node-left! x nil)
     (set-node-right! x nil)
     (set-node-subtree-width! x (node-self-width x))
     ;; if t2 is lazy about having a tree-first, force it.
     ;; This only happens in the context of split!
     (force-tree-first! t2)
     (insert-first! t2 x)
     t2]
    
    [(nil? (tree-root t2))
     ;; symmetric with the case above:
     (set-node-left! x nil)
     (set-node-right! x nil)
     (set-node-subtree-width! x (node-self-width x))
     (force-tree-last! t1)
     (insert-last! t1 x)
     t1]
    
    [else
     (define t1-bh (tree-bh t1))
     (define t2-bh (tree-bh t2))
     (cond
      [(>= t1-bh t2-bh)     
       ;; Note: even if tree-last is invalid, nothing gets hurt here.
       (set-tree-last! t1 (tree-last t2))

       (define a (find-rightmost-black-node-with-bh t1 t2-bh)) 
       (define b (tree-root t2))
       (transplant-for-concat! t1 a x)
       (set-node-color! x red)
       (set-node-left! x a)
       (set-node-parent! a x)
       (set-node-right! x b)
       (set-node-parent! b x)

       ;; Possible TODO: Ron Wein recommends a lazy approach here,
       ;; rather than recompute the metadata eagerly.  I've tried so,
       ;; but in my experiments, the overhead of testing and forcing
       ;; actually hurts us.  So I do not try to compute subtree-width
       ;; lazily.
       (update-subtree-width-up-to-root! x)
       (fix-after-insert! t1 x)
       t1]
      
      [else
       ;; Symmetric case:
       (set-tree-first! t2 (tree-first t1))
       (define a (tree-root t1))
       (define b (find-leftmost-black-node-with-bh t2 t1-bh))
       (transplant-for-concat! t2 b x)
       (set-node-color! x red)
       (set-node-left! x a)
       (set-node-parent! a x)
       (set-node-right! x b)
       (set-node-parent! b x)
       (update-subtree-width-up-to-root! x)
       (fix-after-insert! t2 x)
       t2])]))


;; transplant-for-concat!: tree node node -> void
;; INTERNAL
;; Replaces node u in a-tree with v.
(define (transplant-for-concat! a-tree u v)
  (define u.p (node-parent u))
  (cond [(nil? u.p)
         (set-tree-root! a-tree v)]
        [(eq? u (node-left u.p))
         (set-node-left! u.p v)]
        [else
         (set-node-right! u.p v)])
  (set-node-parent! v u.p))



;; find-rightmost-black-node-with-bh: tree positive-integer -> node
;; INTERNAL
;; Finds the rightmost black node with the particular black height
;; we're looking for.
(define (find-rightmost-black-node-with-bh a-tree bh)
  (let loop ([n (tree-root a-tree)]
             [current-height (tree-bh a-tree)])
    (cond
      [(black? n)
       (cond [(= bh current-height)
              n]
             [else
              (loop (node-right n) (sub1 current-height))])]
      [else
       (loop (node-right n) current-height)])))


;; find-leftmost-black-node-with-bh: tree positive-integer -> node
;; INTERNAL
;; Finds the rightmost black node with the particular black height
;; we're looking for.
(define (find-leftmost-black-node-with-bh a-tree bh)
  (let loop ([n (tree-root a-tree)]
             [current-height (tree-bh a-tree)])
    (cond
      [(black? n)
       (cond [(= bh current-height)
              n]
             [else
              (loop (node-left n) (sub1 current-height))])]
      [else
       (loop (node-left n) current-height)])))


;; split!: tree node -> (values tree tree)
;; Partitions the tree into two trees: the predecessors of x, and the
;; successors of x.
;;
;; Note: during the loop, the L and R trees do not necessarily have
;; a valid tree-first or tree-last.  I want to avoid recomputing
;; it for each fresh subtree I construct.
(define (split! a-tree x)
  (define x-child-bh (computed-black-height (node-left x)))
  ;; The loop walks the ancestors of x, adding the left and right
  ;; elements appropriately.
  (let loop ([ancestor (node-parent x)]
             [ancestor-child-bh (if (black? x) (add1 x-child-bh) x-child-bh)]
             [coming-from-the-right? (eq? (node-right (node-parent x)) x)]
             [L (node->tree/bh (node-left x) x-child-bh)]
             [R (node->tree/bh (node-right x) x-child-bh)])
    (cond
      [(nil? ancestor)
       ;; Now that we have our L and R, fix up their last and first
       ;; pointers, then return.
       (force-tree-first! L)
       (force-tree-last! L)
       (force-tree-first! R)
       (force-tree-last! R)
       (values L R)]
      [else
       (define new-ancestor (node-parent ancestor))
       (define new-ancestor-child-bh (if (black? ancestor) 
                                         (add1 ancestor-child-bh)
                                         ancestor-child-bh))
       (define new-coming-from-the-right? (eq? (node-right new-ancestor) ancestor))

       ;; Important!  Make sure ancestor is detached.  This is
       ;; required by concat!, or else Bad Things happen.
       (detach! ancestor)

       (cond
        [coming-from-the-right?
         (define subtree (node->tree/bh (node-left ancestor) 
                                        ancestor-child-bh))
         (loop new-ancestor
               new-ancestor-child-bh
               new-coming-from-the-right?
               (concat! subtree ancestor L)
               R)]
        [else
         (define subtree (node->tree/bh (node-right ancestor) 
                                        ancestor-child-bh))
         (loop new-ancestor
               new-ancestor-child-bh
               new-coming-from-the-right?
               L
               (concat! R ancestor subtree))])])))


;; force-tree-first!: tree -> void
;; INTERNAL
;; Force tree-first's value.
;; For non-empty trees, it's set to nil only in node->tree/bh.
(define (force-tree-first! t)
  (when (nil? (tree-first t))
    (set-tree-first! t (minimum (tree-root t)))))


;; force-tree-last!: tree -> void
;; INTERNAL
;; Force tree-last's value.
;; For non-empty trees, it's set to nil only in node->tree/bh.
(define (force-tree-last! t)
  (when (nil? (tree-last t))
    (set-tree-last! t (maximum (tree-root t)))))
  

;; detach!: node -> void
;; INTERNAL
;; Disconnects n from its parent.
(define (detach! n)
  (define p (node-parent n))
  (cond [(nil? p)
         (void)]
        [(eq? (node-right p) n)
         (set-node-right! p nil)
         (set-node-parent! n nil)]
        [else
         (set-node-left! p nil)
         (set-node-parent! n nil)]))


;; node->tree/bh: node natural -> tree
;; INTERNAL: for use by split! only.
;; Create a partially-instantiated node out of a tree, where we should
;; already know the black height of the tree rooted at a-node.
;;
;; Note that the first and last of the tree have not been initialized
;; here yet.  split! must eventually force the L and R trees to
;; have valid first/last pointers, or else Bad Things happen.
(define (node->tree/bh a-node bh)
  (cond
    [(nil? a-node)
     (new-tree)]
    [else
     (define new-bh (if (red? a-node) (add1 bh) bh))
     (set-node-parent! a-node nil)
     (set-node-color! a-node black)
     (tree a-node
           nil
           nil
           new-bh)]))


;; computed-black-height: node -> natural
;; INTERNAL: for use by split! only.
;; Computes the black height of the tree rooted at x.
(define (computed-black-height x)
  (let loop ([x x]
             [acc 0])
    (cond
      [(nil? x)
       acc]
      [else
       (cond [(black? x)
              (loop (node-right x) (add1 acc))]
             [else
              (loop (node-right x) acc)])])))



;; tree-items: tree -> (listof (list X natural))
;; Returns the list of items in the tree.
(define (tree-items t)
  (let loop ([n (tree-root t)]
             [acc null])
    (cond
      [(nil? n)
       acc]
      [else
       (loop (node-left n)
             (cons (list (node-data n)
                         (node-self-width n))
                   (loop (node-right n) acc)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal tests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit 
           rackunit/text-ui
           racket/string
           racket/list
           racket/class
           racket/promise)
  
  
  ;; tree-items: tree -> (listof (list X number))
  ;; Returns a list of all the items stored in the tree.
  (define (tree-items a-tree)
    (let loop ([node (tree-root a-tree)]
               [acc null])
      (cond
        [(nil? node)
         acc]
        [else
         (loop (node-left node)
               (cons (list (node-data node)
                           (node-self-width node))
                     (loop (node-right node) acc)))])))
  
  
  ;; tree-height: tree -> natural
  ;; For debugging: returns the height of the tree.
  (define (tree-height a-tree)
    (let loop ([node (tree-root a-tree)])
      (cond
        [(nil? node)
         0]
        [else
         (+ 1 (max (loop (node-left node))
                   (loop (node-right node))))])))
  
  ;; tree-node-count: tree -> natural
  ;; Counts the nodes of the tree.
  (define (tree-node-count a-tree)
    (let loop ([node (tree-root a-tree)]
               [acc 0])
      (cond
        [(nil? node)
         acc]
        [else
         (loop (node-left node) (loop (node-right node) (add1 acc)))])))
  
  
  ;; Debugging: counts the number of black nodes by manually
  ;; traversing both subtrees.
  (define (node-count-black a-node)
    (let loop ([a-node a-node]
               [acc 0])
      (cond
        [(nil? a-node)
         acc]
        [else
         (define right-count (loop (node-right a-node)
                                   (+ (if (black? a-node) 1 0)
                                      acc)))
         (define left-count  (loop (node-left a-node) 
                                   (+ (if (black? a-node) 1 0)
                                      acc)))
         (check-equal? right-count 
                       left-count
                       (format "node-count-black: ~a vs ~a" right-count left-count))
         right-count])))
  
  
  
  ;; check-rb-structure!: tree -> void
  ;; The following is a heavy debugging function to ensure
  ;; tree-structure is as expected.  Note: this functions is
  ;; EXTRAORDINARILY expensive.  Do not use this outside of tests.
  (define (check-rb-structure! a-tree)
    
    ;; nil should always be black: algorithms depend on this!
    (check-eq? (node-color nil) black)
    (check-eq? (node-subtree-width nil) 0)
    (check-eq? (node-self-width nil) 0)
    (check-eq? (node-parent nil) nil)
    (check-eq? (node-left nil) nil)
    (check-eq? (node-right nil) nil)

    
    ;; The internal linkage between all the nodes should be consistent,
    ;; and without cycles!
    (let ([ht (make-hasheq)])
      (let loop ([node (tree-root a-tree)])
        (cond
          [(nil? node)
           (void)]
          [else
           (check-false (hash-has-key? ht node))
           (hash-set! ht node #t)
           (define left (node-left node))
           (define right (node-right node))
           (when (not (nil? left))
             (check-eq? (node-parent left) node)
             (loop left))
           (when (not (nil? right))
             (check-eq? (node-parent right) node)
             (loop right))])))
    
    
    ;; No two red nodes should be adjacent:
    (let loop ([node (tree-root a-tree)])
      (cond
        [(nil? node)
         (void)]
        [(red? node)
         (check-false (or (red? (node-left node))
                          (red? (node-right node)))
                      "rb violation: two reds are adjacent")
         (loop (node-left node))
         (loop (node-right node))]))
    
    
    ;; The maximum and minimum should be correctly linked
    ;; as tree-last and tree-first, respectively:
    (let ([correct-tree-first (if (nil? (tree-root a-tree))
                                  nil 
                                  (minimum (tree-root a-tree)))])
      (check-eq? (tree-first a-tree)
                 correct-tree-first
                 (format "minimum (~a) is not first (~a)" 
                         (node-data correct-tree-first)
                         (node-data (tree-first a-tree)))))
    (let ([correct-tree-last (if (nil? (tree-root a-tree)) nil (maximum (tree-root a-tree)))])
      (check-eq? (tree-last a-tree)
                 correct-tree-last
                 (format "maximum (~a) is not last (~a)"
                         (node-data correct-tree-last)
                         (node-data (tree-last a-tree)))))
    
    
    ;; The left and right sides should be black-balanced, for all subtrees.
    (let loop ([node (tree-root a-tree)])
      (cond
        [(nil? node)
         (void)]
        [else
         (check-equal? (node-count-black (node-left node))
                       (node-count-black (node-right node))
                       "rb violation: not black-balanced")
         (loop (node-left node))
         (loop (node-right node))]))
    (define observed-black-height (node-count-black (tree-root a-tree)))
    ;; The observed black height should equal that of the recorded one
    (check-equal? (tree-bh a-tree) 
                  observed-black-height
                  (format "rb violation: observed height ~a is not equal to recorded height ~a"
                          observed-black-height 
                          (tree-bh a-tree)))
    
    
    ;; The overall height must be less than 2 lg(n+1)
    (define count (tree-node-count a-tree))
    (define observed-height (tree-height a-tree))
    (define (lg n) (/ (log n) (log 2)))
    (check-false (> observed-height (* 2 (lg (add1 count))))
                 (format "rb violation: height ~a beyond 2 lg(~a)=~a" 
                         observed-height (add1 count)
                         (* 2 (log (add1 count)))))
  
    
    ;; The subtree widths should be consistent:
    (let loop ([n (tree-root a-tree)])
      (cond
        [(nil? n)
         0]
        [else
         (define left-subtree-size (loop (node-left n)))
         (define right-subtree-size (loop (node-right n)))
         (check-equal? (node-subtree-width n)
                       (+ left-subtree-size right-subtree-size
                          (node-self-width n))
                       (format "stale subtree width: expected ~a, but observe ~a"
                               (+ left-subtree-size right-subtree-size
                                  (node-self-width n))
                               (node-subtree-width n)))
         (+ left-subtree-size right-subtree-size 
            (node-self-width n))])))
  
  
  
  ;; tree->list: tree -> list
  ;; For debugging: help visualize what the structure of the tree looks like.
  (define (tree->list a-tree)
    (let loop ([node (tree-root a-tree)])
      (cond
        [(nil? node)
         null]
        [else
         (list (format "~a:~a:~a" 
                       (node-data node)
                       (node-subtree-width node)
                       (if (black? node) "black" "red"))
               (loop (node-left node))
               (loop (node-right node)))])))
  
  
  ;; a little macro to help me measure how much time we spend in
  ;; the body of an expression.
  (define-syntax-rule (time-acc id body ...)
    (let ([start (current-inexact-milliseconds)])
      (call-with-values (lambda ()
                          (let () body ...))
                        (lambda vals
                          (let ([stop (current-inexact-milliseconds)])
                            (set! id (+ id (- stop start)))
                            (apply values vals))))))
  
  
  (define nil-tests
    (test-suite
     "check properties of nil"
     (printf "nil tests...\n")
     (test-case
      "nil tree should be consistent"
      (define t (new-tree))
      (check-rb-structure! t))))
  
  (define rotation-tests
    (test-suite 
     "Checking left and right rotation" 
     (printf "rotation tests...\n")
     (test-begin
      (define t (new-tree))
      
      (define alpha (node "alpha" 5 5 nil nil nil nil))
      (define beta (node "beta" 4 5 nil nil nil nil))
      (define gamma (node "gamma" 5 5 nil nil nil nil))
      
      (define x (node "x" 1 1 nil alpha beta nil))
      (define y (node "y" 1 1 nil nil gamma nil))
      (set-tree-root! t y)
      (set-node-left! y x)
      (set-node-parent! x y)
      
      (right-rotate! t y)
      (check-eq? (tree-root t) x)
      (check-eq? (node-left (tree-root t)) alpha)
      (check-eq? (node-right (tree-root t)) y)
      (check-eq? (node-left (node-right (tree-root t))) beta)
      (check-eq? (node-right (node-right (tree-root t))) gamma)
      
      (left-rotate! t x)
      (check-eq? (tree-root t) y)
      (check-eq? (node-right (tree-root t)) gamma)
      (check-eq? (node-left (tree-root t)) x)
      (check-eq? (node-left (node-left (tree-root t))) alpha)
      (check-eq? (node-right (node-left (tree-root t))) beta))))
  
  
  
  (define insertion-tests
    (test-suite
     "Insertion tests"
     (printf "insertion tests...\n")
     (test-case "small beginnings"
                (define t (new-tree))
                (insert-last/data! t "small world" 11)
                (check-rb-structure! t))
     
     (test-begin
      (define t (new-tree))
      (insert-last/data! t "foobar" 6)
      (insert-last/data! t "hello" 5)
      (insert-last/data! t "world" 5)
      (check-equal? (tree-items t)
                    '(("foobar" 6)
                      ("hello" 5)
                      ("world" 5)))
      (check-rb-structure! t))
     
     
     (test-begin 
      (define t (new-tree))
      (insert-first/data! t "a" 1)
      (insert-first/data! t "b" 1)
      (insert-first/data! t "c" 1)
      (check-equal? (tree-items t)
                    '(("c" 1) ("b" 1) ("a" 1)))
      (check-equal? (tree->list t)
                    '("b:3:black" ("c:1:red" () ()) ("a:1:red" () ())))
      (check-rb-structure! t))
     
     
     (test-begin 
      (define t (new-tree))
      (insert-first/data! t "alpha" 5)
      (insert-first/data! t "beta" 4)
      (insert-first/data! t "gamma" 5)
      (insert-first/data! t "delta" 5)
      (insert-first/data! t "omega" 5)
      (check-equal? (tree-items t)
                    '(("omega" 5) ("delta" 5)
                                  ("gamma" 5) ("beta" 4) ("alpha" 5)))
      (check-rb-structure! t))
     
     
     (test-begin 
      (define t (new-tree))
      (insert-last/data! t "hi" 2)
      (insert-last/data! t "bye" 3)
      (define the-root (tree-root t))
      (check-equal? (node-left the-root)
                    nil)
      (check-true (black? the-root))
      (check-equal? (node-subtree-width the-root) 5)
      (check-true (red? (node-right the-root)))
      
      (check-rb-structure! t))
     
     (test-begin 
      (define t (new-tree))
      (insert-last/data! t "hi" 2)
      (insert-last/data! t "bye" 3)
      (insert-last/data! t "again" 5)
      (define the-root (tree-root t))
      (check-equal? (node-data (node-left the-root))
                    "hi")
      (check-equal? (node-data the-root)
                    "bye")
      (check-equal? (node-data (node-right the-root))
                    "again")
      (check-true (black? the-root))
      (check-true (red? (node-left the-root)))
      (check-true (red? (node-right the-root)))
      (check-equal? (node-subtree-width the-root) 10)
      (check-rb-structure! t))))
  
  
  
  
  (define deletion-tests
    (test-suite
     "deletion-tests"
     (printf "deletion tests...\n")
     (test-case
      "Deleting the last node in a tree should set us back to the nil case"
      (define t (new-tree))
      (insert-first/data! t "hello" 5)
      (delete! t (tree-root t))
      (check-equal? (tree-root t) nil)
      (check-rb-structure! t))
     
     (test-case
      "Deleting the last node in a tree: first and last should be nil"
      (define t (new-tree))
      (insert-first/data! t "hello" 5)
      (delete! t (tree-root t))
      (check-equal? (tree-first t) nil)
      (check-equal? (tree-last t) nil)
      (check-rb-structure! t))
     
     (test-case
      "Delete the last node in a two-node tree: basic structure"
      (define t (new-tree))
      (insert-last/data! t "dresden" 6)
      (insert-last/data! t "files" 5)
      (delete! t (node-right (tree-root t)))
      (check-equal? (node-data (tree-root t)) "dresden")
      (check-equal? (node-left (tree-root t)) nil)
      (check-equal? (node-right (tree-root t)) nil)
      (check-rb-structure! t))
     
     (test-case
      "Delete the last node in a two-node tree: check the subtree-width has been updated"
      (define t (new-tree))
      (insert-last/data! t "dresden" 6)
      (insert-last/data! t "files" 5)
      (delete! t (node-right (tree-root t)))
      (check-equal? (node-subtree-width (tree-root t)) 6)
      (check-rb-structure! t))
     
     (test-case
      "Delete the last node in a two-node tree: check that tree-first and tree-last are correct"
      (define t (new-tree))
      (insert-last/data! t "dresden" 6)
      (insert-last/data! t "files" 5)
      (delete! t (node-right (tree-root t)))
      (check-true (node? (tree-root t)))
      (check-equal? (tree-first t) (tree-root t))
      (check-equal? (tree-last t) (tree-root t))
      (check-rb-structure! t))
     
     
     (test-case
      "bigger case identified by angry monkey"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (insert-last/data! t "d" 1)
      (insert-last/data! t "e" 1)
      (check-rb-structure! t)
      (delete! t (search t 1))
      (check-rb-structure! t)
      (delete! t (search t 1))
      (check-rb-structure! t)
      (delete! t (search t 0))
      (check-rb-structure! t))))
  
  
  
  (define mixed-tests
    (test-suite
     "other miscellaneous tests"
     (printf "mixed tests...\n")
     (test-case 
      "Another sequence identified by the random monkey"
      ;inserting "A" to front
      ;inserting "B" to front
      ;Inserting "C" after "A"
      ;Inserting "D" after "B"
      ;inserting "E" to back
      ;inserting "F" to front
      ;deleting "F"
      ;inserting "G" to back
      ;inserting "H" to back
      ;inserting "I" to front
      ;inserting "J" to back
      ;inserting "K" to front
      ;inserting "L" before "E"
      (define t (new-tree))
      
      (insert-first/data! t "A" 1)
      (check-equal? (map first (tree-items t)) '("A"))
      (insert-first/data! t "B" 1)
      (check-equal? (map first (tree-items t)) '("B" "A"))
      (insert-after/data! t (search t 1) "C" 1)
      (check-equal? (map first (tree-items t)) '("B" "A" "C"))
      (insert-after/data! t (search t 0) "D" 1)
      (check-equal? (map first (tree-items t)) '("B" "D" "A" "C"))
      (insert-last/data! t "E" 1)
      (check-equal? (map first (tree-items t)) '("B" "D" "A" "C" "E"))
      (insert-first/data! t "F" 1)
      (check-equal? (map first (tree-items t)) '("F" "B" "D" "A" "C" "E"))
      (delete! t (search t 0))
      (check-equal? (map first (tree-items t)) '("B" "D" "A" "C" "E"))
      (insert-last/data! t "G" 1)
      (check-equal? (map first (tree-items t)) '("B" "D" "A" "C" "E" "G"))
      (insert-last/data! t "H" 1)
      (check-equal? (map first (tree-items t)) '("B" "D" "A" "C" "E" "G" "H"))
      (insert-first/data! t "I" 1)
      (check-equal? (map first (tree-items t)) '("I" "B" "D" "A" "C" "E" "G" "H"))
      (insert-last/data! t "J" 1)
      (check-equal? (map first (tree-items t)) '("I" "B" "D" "A" "C" "E" "G" "H" "J"))
      (insert-first/data! t "K" 1)
      (check-equal? (map first (tree-items t)) '("K" "I" "B" "D" "A" "C" "E" "G" "H" "J"))
      (check-equal? (node-data (search t 6)) "E")
      (insert-before/data! t (search t 6) "L" 1)
      (check-equal? (map first (tree-items t)) '("K" "I" "B" "D" "A" "C" "L" "E" "G" "H" "J"))
      
      (for/fold ([n (minimum (tree-root t))]) ([w (in-list '("K" "I" "B" "D" "A" "C" "L" "E" "G" "H" "J"))])
        (check-equal? (node-data n) w)
        (successor n))
      
      (for/fold ([n (maximum (tree-root t))]) ([w (in-list (reverse '("K" "I" "B" "D" "A" "C" "L" "E" "G" "H" "J")))])
        (check-equal? (node-data n) w)
        (predecessor n)))))
  
  
  (define search-tests
    (test-suite 
     "search-tests"
     (printf "search tests...\n")
     (test-begin
      (define t (new-tree))
      (check-equal? (search t 0) nil)
      (check-equal? (search t 129348) nil))
     
     (test-begin
      (define t (new-tree))
      (insert-last/data! t "hello" 5)
      (check-equal? (node-data (search t 0)) "hello")
      (check-equal? (node-data (search t 1)) "hello")
      (check-equal? (node-data (search t 2)) "hello")
      (check-equal? (node-data (search t 3)) "hello")
      (check-equal? (node-data (search t 4)) "hello")
      ;; Edge case:
      (check-equal? (search t 5) nil)
      ;; Edge case:
      (check-equal? (search t -1) nil))
     
     
     ;; Empty nodes should get skipped over by search, though
     ;; the nodes are still there in the tree.
     (test-begin
      (define t (new-tree))
      (insert-last/data! t "hello" 5)
      (insert-last/data! t "" 0)
      (insert-last/data! t "" 0)
      (insert-last/data! t "" 0)
      (insert-last/data! t "world" 5)
      (insert-last/data! t "" 0)
      (insert-last/data! t "" 0)
      (insert-last/data! t "" 0)
      (insert-last/data! t "test!" 5)
      (check-equal? (tree-node-count t) 9)
      (check-equal? (node-data (search t 0)) "hello")
      (check-equal? (node-data (search t 1)) "hello")
      (check-equal? (node-data (search t 2)) "hello")
      (check-equal? (node-data (search t 3)) "hello")
      (check-equal? (node-data (search t 4)) "hello")
      (check-equal? (node-data (search t 5)) "world")
      (check-equal? (node-data (search t 6)) "world")
      (check-equal? (node-data (search t 7)) "world")
      (check-equal? (node-data (search t 8)) "world")
      (check-equal? (node-data (search t 9)) "world")
      (check-equal? (node-data (search t 10)) "test!"))
     
     
     
     
     (test-begin
      (define t (new-tree))
      (define words (string-split "This is a test of the emergency broadcast system"))
      (for ([word (in-list words)])
        (insert-last/data! t word (string-length word)))
      (check-equal? (node-data (search t 0)) "This")
      (check-equal? (node-data (search t 1)) "This")
      (check-equal? (node-data (search t 2)) "This")
      (check-equal? (node-data (search t 3)) "This")
      (check-equal? (node-data (search t 4)) "is")
      (check-equal? (node-data (search t 5)) "is")
      (check-equal? (node-data (search t 6)) "a")
      (check-equal? (node-data (search t 7)) "test")
      (check-equal? (node-data (search t 8)) "test")
      (check-equal? (node-data (search t 9)) "test")
      (check-equal? (node-data (search t 10)) "test")
      (check-equal? (node-data (search t 11)) "of")
      (check-equal? (node-data (search t 12)) "of")
      (check-equal? (node-data (search t 13)) "the")
      (check-equal? (node-data (search t 14)) "the")
      (check-equal? (node-data (search t 15)) "the")
      (check-equal? (node-data (search t 16)) "emergency")
      (check-equal? (node-data (search t 25)) "broadcast")
      (check-equal? (node-data (search t 34)) "system"))))
  
  
  (define position-tests
    (test-suite
     "position tests"
     (printf "position tests...\n")
     (test-case
      "empty case"
      (check-equal? (position nil) -1))
     
     (test-case
      "simple case"
      (define t (new-tree))
      (insert-last/data! t "foobar" 6)
      (check-equal? (position (tree-root t)) 0))
     
     (test-case
      "simple case of a few random words"
      (define t (new-tree))
      (insert-last/data! t "uc berkeley" 11)
      (insert-last/data! t "wpi" 3)
      (insert-last/data! t "brown" 5)
      (insert-last/data! t "university of utah" 18)
      (check-equal? (position (tree-first t)) 0)
      (check-equal? (position (successor (tree-first t))) 11)
      (check-equal? (position (successor (successor (tree-first t)))) 14)
      (check-equal? (position (successor (successor (successor (tree-first t))))) 19))
     
     (test-case
      "slightly larger example"
      (define t (new-tree))
      (define words 
        (string-split 
         "In a hole in the ground there lived a hobbit.  Not a
         nasty, dirty wet hole, filled with the ends of worms and an oozy
         smell, nor yet a dry, bare, sandy ole with nothing in it to sit down on
         or to eat: it was a hobbit-hole, and that means comfort."))
      (for ([w (in-list words)]) 
        (insert-last/data! t w (string-length w)))
      (for/fold ([pos 0]) ([w (in-list words)])
        (define n (search t pos))
        (check-equal? (node-data n) w)
        (check-equal? (position n) pos)
        (+ pos (string-length w))))))
  
  
  (define concat-tests
    (test-suite
     "concat tests"
     (printf "concat tests...\n")
     (test-case 
      "empty case"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (define t1+t2 (join! t1 t2))
      (check-true (nil? (tree-root t1+t2)))
      (check-rb-structure! t1+t2))
     
     (test-case 
      "left is empty"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t2 "hello" 5)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2))
                    '("hello"))
      (check-rb-structure! t1+t2))
     
     (test-case 
      "right is empty"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t1 "hello" 5)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2))
                    '("hello"))
      (check-rb-structure! t1+t2))
     
     (test-case
      "two single trees"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t1 "append" 5)
      (insert-last/data! t2 "this" 4)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2)) '("append" "this"))
      (check-rb-structure! t1+t2))
     
     
     (test-case
      "appending 2-1"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t1 "love" 4)
      (insert-last/data! t1 "and" 3)
      (insert-last/data! t2 "peace" 5)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2)) '("love" "and" "peace"))
      (check-rb-structure! t1+t2))
     
     (test-case
      "appending 1-2"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t1 "love" 4)
      (insert-last/data! t2 "and" 3)
      (insert-last/data! t2 "war" 3)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2)) '("love" "and" "war"))
      (check-rb-structure! t1+t2))
     
     
     (test-case
      "appending 3-3"
      (define t1 (new-tree))
      (define t2 (new-tree))
      (insert-last/data! t1 "four" 4)
      (insert-last/data! t1 "score" 5)
      (insert-last/data! t1 "and" 3)
      (insert-last/data! t2 "seven" 5)
      (insert-last/data! t2 "years" 5)
      (insert-last/data! t2 "ago" 3)
      (define t1+t2 (join! t1 t2))
      (check-equal? (map first (tree-items t1+t2)) '("four" "score" "and" "seven" "years" "ago"))
      (check-rb-structure! t1+t2))
     
     
     (test-case
      "a bigger concatenation example.  Gettysburg Address, November 19, 1863."
      (define t1 (new-tree))
      (define t2 (new-tree))
      (define t3 (new-tree))
      (define m1 "Four score and seven years ago our fathers
                  brought forth on this continent a new nation,
                  conceived in Liberty, and dedicated to the proposition
                  that all men are created equal.")
      (define m2 "Now we are engaged in a great civil war, testing
                  whether that nation, or any nation so conceived and so dedicated,
                  can long endure. We are met on a great battle-field of that war.
                  We have come to dedicate a portion of that field, as a final 
                  resting place for those who here gave their lives that that nation 
                  might live. It is altogether fitting and proper that we should do this.")
      (define m3 "But, in a larger sense, we can not dedicate -- we can not consecrate
                  -- we can not hallow -- this ground. The brave men, living and dead,
                  who struggled here, have consecrated it, far above our poor power to 
                  add or detract. The world will little note, nor long remember what we
                  say here, but it can never forget what they did here. It is for us the living,
                  rather, to be dedicated here to the unfinished work which they who fought here
                  have thus far so nobly advanced. It is rather for us to be here dedicated to
                  the great task remaining before us -- that from these honored dead we take 
                  increased devotion to that cause for which they gave the last full measure 
                  of devotion -- that we here highly resolve that these dead shall not have died 
                  in vain -- that this nation, under God, shall have a new birth of freedom --
                  and that government of the people, by the people, for the people, 
                  shall not perish from the earth.")
      (for ([word (in-list (string-split m1))])
        (insert-last/data! t1 word (string-length word)))      
      (for ([word (in-list (string-split m2))])
        (insert-last/data! t2 word (string-length word)))
      (for ([word (in-list (string-split m3))])
        (insert-last/data! t3 word (string-length word)))
      (define speech-tree (join! (join! t1 t2) t3))
      (check-equal? (map first (tree-items speech-tree))
                    (string-split (string-append m1 " " m2 " " m3)))
      (check-rb-structure! speech-tree))))
  
  (define split-tests
    (test-suite
     "splitting"
     (printf "split tests...\n")
     (test-case
      "(a) ---split-a--> () ()"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (define-values (l r) (split! t (search t 0)))
      (check-equal? (map first (tree-items l)) '())
      (check-equal? (map first (tree-items r)) '())
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a b) ---split-a--> () (b)"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (define-values (l r) (split! t (search t 0)))
      (check-equal? (map first (tree-items l)) '())
      (check-equal? (map first (tree-items r)) '("b"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a b) ---split-b--> (a) ()"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (define-values (l r) (split! t (search t 1)))
      (check-equal? (map first (tree-items l)) '("a"))
      (check-equal? (map first (tree-items r)) '())
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a b c) ---split-b--> (a) (c)"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (define-values (l r) (split! t (search t 1)))
      (check-equal? (map first (tree-items l)) '("a"))
      (check-equal? (map first (tree-items r)) '("c"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a b c d) ---split-a--> () (b c d)"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (insert-last/data! t "d" 1)
      (define-values (l r) (split! t (search t 0)))
      (check-equal? (map first (tree-items l)) '())
      (check-equal? (map first (tree-items r)) '("b" "c" "d"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     
     (test-case
      "(a b c d) ---split-b--> (a) (c d)"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (insert-last/data! t "d" 1)
      (define-values (l r) (split! t (search t 1)))
      (check-equal? (map first (tree-items l)) '("a"))
      (check-equal? (map first (tree-items r)) '("c" "d"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     
     (test-case
      "(a b c d) ---split-c--> (a b) (d)"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (insert-last/data! t "d" 1)
      (define-values (l r) (split! t (search t 2)))
      (check-equal? (map first (tree-items l)) '("a" "b"))
      (check-equal? (map first (tree-items r)) '("d"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a b c d) ---split-d--> (a b c) ()"
      (define t (new-tree))
      (insert-last/data! t "a" 1)
      (insert-last/data! t "b" 1)
      (insert-last/data! t "c" 1)
      (insert-last/data! t "d" 1)
      (define-values (l r) (split! t (search t 3)))
      (check-equal? (map first (tree-items l)) '("a" "b" "c"))
      (check-equal? (map first (tree-items r)) '())
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a ... z) ---split-m--> (a ... l) (n ...z)"
      (define t (new-tree))
      (for ([i (in-range 26)])
        (insert-last/data! t (string (integer->char (+ i (char->integer #\a))))
                           1))
      (define letter-m (search t 12))
      (define-values (l r) (split! t letter-m))
      (check-equal? (map first (tree-items l)) '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"))
      (check-equal? (map first (tree-items r)) '("n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
      (check-rb-structure! l)
      (check-rb-structure! r))
     
     (test-case
      "(a ... z) ---split-n--> (a ... l) (n ...z)"
      (define letters (for/list ([i (in-range 26)])
                        (string (integer->char (+ i (char->integer #\a))))))
      (for ([n (in-range 26)])
        (define t (new-tree))
        (for ([w (in-list letters)])
          (insert-last/data! t w 1))
        (define-values (l r) (split! t (search t n)))
        (define-values (expected-l 1+expected-r) (split-at letters n))
        (check-equal? (map first (tree-items l)) expected-l)
        (check-equal? (map first (tree-items r)) (rest 1+expected-r))
        (check-rb-structure! l)
        (check-rb-structure! r)))))
     
  

  (define predecessor-successor-min-max-tests
    (test-suite
     "predecesssor, successor, maximum, minimum tests"
     (printf "navigation tests\n")
     (test-case
      "simple predecessor and successor tests"
      (define known-model '("this" "is" "yet" "another" "test" "that" "makes" "sure" "we" "can" "walk"
                                   "the" "tree" "reliably" "using" "successor" "and" "predecessor"))
      ;; Make sure successor, predecessor are both doing the right thing on it:
      (define t (new-tree))
      (for ([w (in-list known-model)])
        (insert-last/data! t w (string-length w)))
      (check-equal? (node-data (minimum (tree-root t))) "this")
      (check-equal? (node-data (maximum (tree-root t))) "predecessor")
      (for/fold ([n (tree-first t)]) ([w (in-list known-model)])
        (check-equal? (node-data n) w)
        (successor n))
      (for/fold ([n (tree-last t)]) ([w (in-list (reverse known-model))])
        (check-equal? (node-data n) w)
        (predecessor n)))

     (test-case
      "one-element tree"
      (define t (new-tree))
      (insert-last/data! t "unary" 5)
      (check-eq? (predecessor (tree-root t)) nil)
      (check-eq? (successor (tree-root t)) nil)
      (check-eq? (maximum (tree-root t)) (tree-root t))
      (check-eq? (minimum (tree-root t)) (tree-root t)))
     
     (test-case
      "nil cases"
      (check-eq? (predecessor nil) nil)
      (check-eq? (successor nil) nil)
      (check-eq? (maximum nil) nil)
      (check-eq? (minimum nil) nil))))
  
     
  
  
  
  (define angry-monkey%
    (let ()
      (define-local-member-name catch-and-concat-at-front)
      (class object%
        (super-new)
        (define known-model '())
        (define t (new-tree))
        
        (define (random-word)
          (build-string (add1 (random 5))
                        (lambda (i) 
                          (integer->char (+ (char->integer #\a) (random 26))))))
        (define/public (get-tree) t)
        (define/public (get-model) known-model)
        
        (define/public (insert-front!)
          (define new-word (random-word))
          #;(printf "inserting ~s to front\n" new-word)
          (set! known-model (cons new-word known-model))
          (insert-first/data! t new-word (string-length new-word)))
        
        (define/public (insert-back!)
          (define new-word (random-word))
          #;(printf "inserting ~s to back\n" new-word)
          (set! known-model (append known-model (list new-word)))
          (insert-last/data! t new-word (string-length new-word)))
        
        (define/public (delete-kth! k)
          #;(printf "deleting ~s\n" (list-ref known-model k))
          (define offset (kth-offset k))
          (define node (search t offset))
          (delete! t node)
          (set! known-model (let-values ([(a b) (split-at known-model k)])
                              (append a (rest b)))))
        
        ;; kth-offset: natural -> natural
        ;; Returns the offset of the kth word in the model.
        (define (kth-offset k)
          (for/fold ([offset 0]) ([i (in-range k)]
                                  [word (in-list known-model)])
            (+ offset (string-length word))))
        
        
        (define/public (delete-random!)
          (when (not (empty? known-model))
            ;; Delete a random word if we can.
            (define k (random (length known-model)))
            (delete-kth! k)))
        
        (define/public (insert-before/random!)
          (when (not (empty? known-model))
            (define k (random (length known-model)))
            (define offset (kth-offset k))
            (define node (search t offset))
            (define new-word (random-word))
            #;(printf "Inserting ~s before ~s\n" new-word (node-data node))
            (insert-before/data! t node new-word (string-length new-word))
            (set! known-model (append (take known-model k)
                                      (list new-word)
                                      (drop known-model k)))))
        
        (define/public (insert-after/random!)
          (when (not (empty? known-model))
            (define k (random (length known-model)))
            (define offset (kth-offset k))
            (define node (search t offset))
            (define new-word (random-word))
            #;(printf "Inserting ~s after ~s\n" new-word (node-data node))
            (insert-after/data! t node new-word (string-length new-word))
            (set! known-model (append (take known-model (add1 k))
                                      (list new-word)
                                      (drop known-model (add1 k))))))
        
        
        ;; Concatenation.  Drop our existing tree and throw it at the
        ;; other m2 monkey.
        (define/public (throw-all-at-monkey m2)
          (send m2 catch-and-concat-at-front t known-model)
          (set! t (new-tree))
          (set! known-model '()))


        ;; Splitting/concatenation.  Split what we've got, keep the
        ;; left, and throw the right to our friend m2.
        (define/public (throw-some-at-monkey m2)
          (when (not (empty? known-model))
            (define k (random (length known-model)))
            (define offset (kth-offset k))
            (define node (search t offset))
            (define-values (l r) (split! t node))
            (set! t l)
            (send m2 catch-and-concat-at-front r (drop known-model (add1 k)))
            (set! known-model (take known-model k))))

        ;; private
        (define/public (catch-and-concat-at-front other-t other-known-model)
          (set! t (join! other-t t))
          (set! known-model (append other-known-model known-model)))

        
        (define/public (check-consistency!)
          ;; Check that the structure is consistent with our model.
          (check-equal? (map first (tree-items t)) known-model)
                    
          ;; And make sure it's still an rb-tree:
          (check-rb-structure! t)))))
  
  
  
  (define angry-monkey-test-1
    (test-suite
     "A simulation of an angry monkey bashing at the tree."
     (test-begin
      (printf "monkey tests 1...\n")
      (define number-of-operations 100)
      (define number-of-iterations 100)
      (for ([i (in-range number-of-iterations)])
        (define m (new angry-monkey%))
        (for ([i (in-range number-of-operations)])
          (case (random 12)
            [(0 1 2)
             (send m insert-front!)]
            [(3 4 5)
             (send m insert-back!)]
            [(6 7)
             (send m insert-after/random!)]
            [(8 9)
             (send m insert-before/random!)]
            [(10 11)
             (send m delete-random!)]))
        (send m check-consistency!)))))
  
  
  (define angry-monkey-test-2
    (test-suite
     "Another simulation of an angry monkey bashing at the tree.
     (more likely to delete)"
     (test-begin
      (printf "monkey tests 2...\n")
      (define number-of-operations 100)
      (define number-of-iterations 100)
      (for ([i (in-range number-of-iterations)])
        (define m (new angry-monkey%))
        (for ([i (in-range number-of-operations)])
          (case (random 12)
            [(0 1)
             (send m insert-front!)]
            [(2 3)
             (send m insert-back!)]
            [(4 5)
             (send m insert-after/random!)]
            [(6 7)
             (send m insert-before/random!)]
            [(8 9 10 11)
             (send m delete-random!)]))
        (send m check-consistency!)))))
  
  (define angry-monkey-pair-test
    (test-suite
     "Simulation of a pair of angry monkeys bashing at the tree.
      Occasionally they'll throw things at each other."
     (test-begin
      (printf "monkey tests paired...\n")
      (define number-of-operations 100)
      (define number-of-iterations 100)
      (for ([i (in-range number-of-iterations)])
        (define m1 (new angry-monkey%))
        (define m2 (new angry-monkey%))
        (for ([i (in-range number-of-operations)])
          (define random-monkey (if (= 0 (random 2)) m1 m2))
          (case (random 11)
            [(0 1 2)
             (send random-monkey insert-front!)]
            [(3 4 5)
             (send random-monkey insert-back!)]
            [(6)
             (send random-monkey delete-random!)]
            [(7)
             (send m1 throw-all-at-monkey m2)]
            [(8)
             (send m2 throw-all-at-monkey m1)]
            [(9)
             (send m1 throw-some-at-monkey m2)]
            [(10)
             (send m2 throw-some-at-monkey m1)]))
        (send m1 check-consistency!)
        (send m2 check-consistency!)))))


  (define angry-monkey-pair-test-parallel
    (test-suite
     ;; What do you call a group of monkeys?  A troop!
     ;; (http://www.npwrc.usgs.gov/about/faqs/animals/names.htm)
     "Simulation of a troop of angry monkeys bashing at the tree.
      They should not see each other."
     (test-begin
      (printf "monkey tests parallel...\n")
      (define number-of-operations 100)
      (define number-of-iterations 100)
      (define threads
        (for/list ([i (in-range 4)])
          (thread (lambda ()
                    (for ([i (in-range number-of-iterations)])
                      (define m (new angry-monkey%))
                      (for ([i (in-range number-of-operations)])
                        (case (random 11)
                          [(0)
                           (send m insert-front!)]
                          [(1)
                           (send m insert-back!)]
                          [(2)
                           (send m delete-random!)]
                          [(3)
                           (send m insert-after/random!)]
                          [(4)
                           (send m insert-before/random!)]))
                      (send m check-consistency!))))))
      (for ([t (in-list threads)])
        (thread-wait t)))))
  

  (define exhaustive-split-test
    (test-suite
     "exhaustive split test..."
     ;; Another exhaustive test.  Among N elements, split everywhere,
     ;; and make sure we get the expected values on the left and right.
     ;; Also print out how long it takes to do the actual splitting.
     (test-case
      "(1 ... n) ---split-k--> (1 ... k-1) (k+1 ...n)"
      (printf "exhaustive split test...\n")
      (define N 2000)
      (define elts (for/list ([i (in-range N)]) i))
      (define total-splitting-time 0)
      (for ([n (in-range N)])
        (define t (new-tree))
        (for ([w (in-list elts)])
          (insert-last/data! t w 1))
        (define-values (l r) 
          (let ([pivot (search t n)])
            (time-acc 
             total-splitting-time
             (split! t pivot))))
        (define-values (expected-l 1+expected-r) (split-at elts n))
        (check-equal? (map first (tree-items l)) expected-l)
        (check-equal? (map first (tree-items r)) (rest 1+expected-r)))
      (printf "time in split: ~a\n" total-splitting-time))))
  
  
  
  (define all-tests
    (test-suite "all-tests" 
                nil-tests 
                rotation-tests
                insertion-tests
                deletion-tests
                search-tests
                position-tests
                concat-tests
                predecessor-successor-min-max-tests
                split-tests
                mixed-tests

                ;; The following tests are a bit more expensive.  Wait a while.
                angry-monkey-test-1 
                angry-monkey-test-2 
                angry-monkey-pair-test
                angry-monkey-pair-test-parallel
                exhaustive-split-test))
  
  (void
   (printf "Running test suite.\nWarning: this suite can run very slowly under DrRacket when debugging is on.\n")
   (run-tests all-tests)))
