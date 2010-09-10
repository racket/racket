#lang racket/base
(require racket/match
         racket/dict
         racket/contract)

;; FIXME: need special handling of +/- inf.0 ! (otherwise, other keys get killed)
;; Idea: in traversal, just treat +/-inf.0 as 0 for key-adjustment.

;; ======== Raw splay tree ========

(struct node (key value left right) #:mutable #:transparent)

#|
Bottom-up, zero-allocation splay

The following notes sketch the derivation from the naive bottom-up
splay algorithm.

====

SplayPath = null | (cons (Side,Node) SplayPath)
In a SplayPath [...,(s1,n1),(s2,n2),...], then n1 = n2.s2.

find : ... -> (Node, SplayPath)
If find returns (s,x,[(s1,n1),...]), then x = n1.s1.

splay : (Node, SplayPath) -> Node
splayloop : (Node, SplayPath) -> (Node, SplayPath)

====

We always splay after find, so let's have find immediately call
isplay (incremental splay) with just the new part of the splay
path. But we can only splay when we have *two* splay path segments to
work with.

SplayPathBuf = Maybe (Side, Node)

find' : ... -> (Node, SplayPathBuf)
find' ... = ... isplay (find' ..., localSide, localNode) ...

isplay : ((Node, SplayPathBuf), Side, Node) -> (Node, SplayPathBuf)

And at the top there needs to be a finish function to handle
zigs (odd-length SplayPaths => non-None final SplayPathBufs).

finish : (Node, SplayPathBuf) -> Node

====

Actually, find returns Maybe Node. But we still want to splay the path
and produce a new root, even if find failed. So if find'' initially
returns None, isplay' takes the last node seen, sets that as the new
root, and continues splaying. We introduce a status result that
indicates whether the new root was actually the node sought (we also
distinguish found vs added.)

Status = Found | Added | Failed

find'' : ... -> (Status, Maybe Node, SplayPathBuf)
isplay : ((Status, Maybe Node, SplayPathBuf), Side, Node) -> (Status, Node, SplayPathBuf)

finish' : (Status, Maybe Node, SplayPathBuf) -> (Status, Maybe Node)

Note that isplay always returns a Node, never None (I'm taking some
type liberties here). Of course, if the initial tree is empty, isplay
is not called.

====

To avoid allocation, we flatten the types above and use multiple value
return.

<SPB> = (Maybe Side) (Maybe Node)
SP = (values Status (Maybe Node) <SPB>)
   = (values Status (Maybe Node) (Maybe Side) (Maybe Node))

In (values status nroot pside pnode):
  nroot is the new root (or #f)
  if pside and pnode are both non-#f,
    pnode is next node in splay path, overrides nroot as new root IF nroot = #f
  if pside and pnode are both #f,
    no pending rotation; add it and keep going...

|#

(define-syntax-rule (SPfinish expr)
  (let-values ([(tx ok? x p-side p) expr])
    (finish tx ok? x p-side p)))

(define-syntax-rule (SPisplay x-expr gp-side gp)
  (let-values ([(tx ok? x p-side p) x-expr])
    (isplay! tx ok? x p-side p gp-side gp)))

(define (SPunit tx x) (values tx 'found x #f #f))
(define (SPunit/add tx x) (values tx 'added x #f #f))
(define (SPfail tx) (values tx #f #f #f #f))

;; --------

;; find/root : ... -> (values boolean node/#f)
;; If ok?, then node returned is one sought.
(define (find/root cmp tx k x add-v)
  (SPfinish (find cmp tx k x #f #f add-v)))

;; find : ... -> SP
(define (find cmp tx k x p-side p add-v)
  (cond [x
         (let ([k* (if tx (- k (node-key x)) k)])
           (case (cmp k (node-key x))
             ((=) (SPunit tx x))
             ((<) (SPisplay (find cmp tx k* (node-left x)  'left  x add-v) 'left x))
             ((>) (SPisplay (find cmp tx k* (node-right x) 'right x add-v) 'right x))))]
        [add-v
         (let ([new-node (node k (car add-v) #f #f)])
           ;; link unnecessary? will be done in isplay/finish?
           (when p (set-node-side! p p-side new-node))
           (SPunit/add tx new-node))]
        [else (SPfail tx)]))

;; isplay! : ... -> node
;; incremental splay
(define (isplay! tx ok? x p-side p gp-side gp)
  ;; (printf "splay! ~s\n" (list x p-side p gp-side gp))
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         ;; Overwrite new root with gp
         (values tx ok? gp #f #f)]
        [p-side ;; we have two splay path segments; splay
         ;; First, link x as p.p-side
         (set-node-side! p p-side x)
         (cond [(eq? p-side gp-side)
                ;; zig-zig
                (rotate! tx gp gp-side)
                (rotate! tx p p-side)
                (values tx ok? x #f #f)]
               [else
                ;; zig-zag
                (rotate! tx p p-side)
                (rotate! tx gp gp-side)
                (values tx ok? x #f #f)])]
        [else
         (values tx ok? x gp-side gp)]))

(define (finish tx ok? x p-side p)
  (printf "run ~s\n" (list x p-side p))
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         (values ok? #f)]
        [p-side ;; one splay segment left; perform zig
         ;; First, link x as p.p-side
         (set-node-side! p p-side x)
         (rotate! tx p p-side)
         (values ok? x)]
        [else ;; no splay segments left
         (values ok? x)]))

(define (set-node-side! n side v)
  (case side
    ((left) (set-node-left! n v))
    ((right) (set-node-right! n v))))

(define (rotate! tx x side)
  (case side
    ((left) (right! tx x))
    ((right) (left! tx x))
    ((#f) (void))))

(define (right! tx p)
  (match p
    [(node Kp _ (and x (node Kx _ A B)) C)
     (set-node-left! p B)
     (set-node-right! x p)
     (when tx
       (set-node-key! p (- 0 Kx))
       (set-node-key! x (+ Kp Kx))
       (when B
         (set-node-key! B (+ (node-key B) Kx))))
     (sanity! tx 'right x)]))

(define (left! tx p)
  (match p
    [(node Kp _ A (and x (node Kx _ B C)))
     (set-node-right! p B)
     (set-node-left! x p)
     (when tx
       (set-node-key! p (- 0 Kx))
       (set-node-key! x (+ Kp Kx))
       (when B
         (set-node-key! B (+ (node-key B) Kx))))
     (sanity! tx 'left x)]))

(define (sanity! tx who x0)
  (when tx 
    (let loop ([x x0] [sign? void])
      (when (node? x)
        (unless (sign? (node-key x))
          (printf "x0 = ~s\n" x0)
          (error 'insane! "~s: insane sub-node ~s" who x))
        (loop (node-left x) negative?)
        (loop (node-right x) positive?)))))

;; --------

;; if left is node, new root is max(left)
(define (join-left tx left right)
  (cond [(and left right)
         (let-values ([(_ok? left*) (find-max tx left)])
           ;; left* is node, must have empty right branch
           (set-node-right! left* right)
           (when tx
             (set-node-key! right (- (node-key right) (node-key left*))))
           left*)]
        [left left]
        [else right]))

;; if right is node, new root is min(right)
(define (join-right tx left right)
  (cond [(and left right)
         (let-values ([(_ok? right*) (find-min tx right)])
           ;; right* is node, right*.left = #f
           (set-node-left! right* left)
           (when tx
             (set-node-key! left (- (node-key left) (node-key right*))))
           right*)]
        [right right]
        [else left]))

(define (split/drop-root tx root)
  (let ([left (node-left root)]
        [right (node-right root)])
    (when tx
      (when left
        (set-node-key! left (+ (node-key left) (node-key root))))
      (when right
        (set-node-key! right (+ (node-key right) (node-key root)))))
    (values left right)))

(define (split/root-to-left tx root)
  (let ([right (node-right root)])
    (when (and tx right)
      (set-node-key! right (+ (node-key right) (node-key root))))
    (set-node-right! root #f)
    (values root right)))

(define (split/root-to-right tx root)
  (let ([left (node-left root)])
    (when (and tx left)
      (set-node-key! left (+ (node-key left) (node-key root))))
    (set-node-left! root #f)
    (values left root)))

(define (delete-root tx root)
  (let-values ([(left right) (split/drop-root tx root)])
    (join-left tx left right)))

(define (find-min tx x)
  (define (find-min-loop x)
    (cond [(and x (node-left x))
           (SPisplay (find-min-loop (node-left x)) 'left x)]
          [x (SPunit tx x)]
          [else (SPfail tx)]))
  (SPfinish (find-min-loop x)))

(define (find-max tx x)
  (define (find-max-loop x)
    (cond [(and x (node-right x))
           (SPisplay (find-max-loop (node-right x)) 'right x)]
          [x (SPunit tx x)]
          [else (SPfail tx)]))
  (SPfinish (find-max-loop x)))

(define (contract! cmp tx root from to)
  ;; tx = #t... why pass as param?
  (let*-values ([(ok? from-node) (find/root cmp tx root from (list #f))]
                [(left-tree right-tree)
                 (if (eq? ok? 'added)
                     (split/drop-root tx from-node)
                     (split/root-to-right tx from-node))]
                [(ok? to-node) (find/root cmp tx right-tree to (list #f))]
                [(mid-tree right-tree)
                 (if (eq? ok? 'added)
                     (split/drop-root tx to-node)
                     (split/root-to-right tx to-node))])
    (when tx ;; ie, #t
      (when right-tree
        (set-node-key! right-tree (+ (node-key right-tree) (- from to)))))
    (join-left tx left-tree right-tree)))

(define (expand! cmp tx root from to)
  (let*-values ([(ok? from-node) (find/root cmp tx root from (list #f))]
                [(left-tree right-tree)
                 (if (eq? ok? 'added)
                     (split/drop-root tx from-node)
                     (split/root-to-right tx from-node))])
    (when tx ;; ie, #t
      (when right-tree
        (set-node-key! right-tree (+ (node-key right-tree) (- to from)))))
    (join-left tx left-tree right-tree)))

(define (find-prev tx root)
  ;; PRE: root is node and root.left is node; ie, has-prev?
  (let-values ([(left right) (split/root-to-right tx root)])
    ;; join-left does max(left)
    (join-left tx left right)))

(define (find-next tx root)
  ;; PRE: root is node and root.right is node; ie, has-next?
  (let-values ([(left right) (split/root-to-left tx root)])
    ;; join-right does min(right)
    (join-right tx left right)))

(define (has-prev? x) (and x (node-left x) #t))
(define (has-next? x) (and x (node-right x) #t))

;; ======== Splay tree ========

(define make-splay-tree*
  (let ([make-splay-tree
         (lambda (<? =?)
           (splay-tree #f
                       (lambda (x y) (if (=? x y) '= (if (<? x y) '< '>)))
                       #f))])
    make-splay-tree))

#|
In a numeric splay tree, keys can be stored relative to their parent nodes.
Only if requested, though; otherwise, lots of pointless arithmetic.
|#
(define (make-numeric-splay-tree [tx #f])
  (splay-tree #f 0 (lambda (x y) (if (= x y) '= (if (< x y) '< '>))) tx))

(define not-given (gensym 'not-given))

(define (splay-tree-ref s x [default not-given])
  (match s
    [(splay-tree root size cmp tx)
     (let-values ([(ok? root) (find/root cmp tx x root #f)])
       (set-splay-tree-root! s root)
       (if ok?
           (node-value root)
           (cond [(eq? default not-given)
                  (error 'splay-tree-ref "no value found for key: ~e" x)]
                 [(procedure? default)
                  (default)]
                 [else default])))]))

(define (splay-tree-set! s x v)
  (match s
    [(splay-tree root size cmp tx)
     (let-values ([(ok? root) (find/root cmp tx x root (list v))])
       (set-splay-tree-root! s root)
       (when (eq? ok? 'added) (set-splay-tree-size! s (add1 size)))
       (printf "root = ~s\n" root)
       (unless (eq? (node-value root) v)
         (set-node-value! root v)))]))

(define (splay-tree-remove! s x)
  (match s
    [(splay-tree root size cmp tx)
     (let-values ([(ok? root) (find/root cmp tx x root #f)])
       (when ok? ;; => root is node
         (set-splay-tree-root! s (delete-root tx root))
         (set-splay-tree-size! s (sub1 size))))]))

(define (splay-tree-count s)
  (splay-tree-size s))

#|
Iteration in splay-trees is problematic.
 - any access to the splay-tree disturbs most notions of "position"
   (other dictionaries, eg hashes, are only disturbed by *updates*)
 - parent-relative keys need parent chain to be interpreted
 - sequential iteration is worst for splaying (leaves as linear tree)

Options
 1) position = parent chain (very likely to get out of sync)
 2) position = key (re-lookup each time)
 3) snapshot as alist (more allocation than necessary, sometimes much more)

(1) is no good. (3) is not very iterator-like.

(2) seems to be the best compromise.
|#

(struct splay-tree-iter (key))

(define (splay-tree-iterate-first s)
  (match s
    [(splay-tree root size cmp tx)
     (let-values ([(ok? root) (find-min tx root)])
       (set-splay-tree-root! s root)
       (if ok? (splay-tree-iter (node-key root)) #f))]))

(define (splay-tree-iterate-next s pos)
  (match pos
    [(splay-tree-iter key)
     (let ([next (splay-tree-least-key/>? s key not-given)])
       (if (eq? next not-given)
           #f
           (splay-tree-iter next)))]))

(define (splay-tree-iterate-key s pos)
  (match pos
    [(splay-tree-iter key) key]))

(define (splay-tree-iterate-value s pos)
  (match pos
    [(splay-tree-iter key)
     (splay-tree-ref s key #f)]))


(struct splay-tree ([root #:mutable] [size #:mutable] cmp tx)
        #:transparent
        #:property prop:dict
        (vector splay-tree-ref
                splay-tree-set!
                #f ;; set
                splay-tree-remove!
                #f ;; remove
                splay-tree-count
                splay-tree-iterate-first
                splay-tree-iterate-next
                splay-tree-iterate-key
                splay-tree-iterate-value))

;; Order-based search

(define (extreme who s key cmp-result has-X? find-X default)
  (match s
    [(splay-tree root size cmp tx)
     (let*-values ([(_ok? root) (find/root cmp tx key root #f)]
                   [(ok? root)
                    (cond [(and root (memq (cmp (node-key root) key) cmp-result))
                           (values #t root)]
                          [(has-X? root)
                           (values #t (find-X tx root))]
                          [else
                           (values #f root)])])
       (set-splay-tree-root! s root)
       (if ok?
           (node-key root)
           (cond [(eq? default not-given)
                  (error who "no key found ~a than~a ~e"
                         (if (memq '< cmp-result) "less" "greater")
                         (if (memq '= cmp-result) " or equal to" "")
                         key)]
                 [(procedure? default) (default)]
                 [else default])))]))

(define (splay-tree-greatest-key/<=? s key [default not-given])
  (extreme 'splay-tree-greatest-key/<=? s key '(< =) has-prev? find-prev default))

(define (splay-tree-greatest-key/<? s key [default not-given])
  (extreme 'splay-tree-greatest-key/<? s key '(<) has-prev? find-prev default))

(define (splay-tree-least-key/>=? s key [default not-given])
  (extreme 'splay-tree-least-key/>=? s key '(> =) has-next? find-next default))

(define (splay-tree-least-key/>? s key [default not-given])
  (extreme 'splay-tree-least-key/>? s key '(>) has-next? find-next default))

;; ========

;; snapshot
(define (splay-tree->list s)
  (match s
    [(splay-tree root size cmp tx)
     (let loop ([x root] [onto null] [k* (if tx 0 #f)])
       (match x
         [(node key value left right)
          (let ([key (if tx (+ key k*) key)])
            (loop left
                  (cons (cons key value)
                        (loop right onto key))
                  key))]
         [#f onto]))]))

;; ========

(provide/contract
 [make-numeric-splay-tree (->* () (any/c) splay-tree?)]
 [splay-tree? (-> any/c boolean?)]
 [splay-tree-ref (->* (splay-tree? any/c) (any/c) any/c)]
 [splay-tree-set! (-> splay-tree? any/c any/c void?)]
 [splay-tree-remove! (-> splay-tree? any/c void?)]
 [splay-tree-count (-> splay-tree? exact-nonnegative-integer?)]
 [splay-tree->list (-> splay-tree? (listof (cons/c any/c any/c)))]

 [splay-tree-greatest-key/<=?
  (->* (splay-tree? any/c) (any/c) any/c)]
 [splay-tree-greatest-key/<?
  (->* (splay-tree? any/c) (any/c) any/c)]
 [splay-tree-least-key/>=?
  (->* (splay-tree? any/c) (any/c) any/c)]
 [splay-tree-least-key/>?
  (->* (splay-tree? any/c) (any/c) any/c)])
