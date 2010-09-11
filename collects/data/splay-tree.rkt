#lang racket/base
(require racket/match
         racket/dict
         racket/contract)

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
           ;; FIXME: link unnecessary? will be done in isplay/finish?
           (when p (set-node-side! p p-side new-node))
           (SPunit/add tx new-node))]
        [else (SPfail tx)]))

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

;; isplay! : ... -> SP
;; incremental splay
(define (isplay! tx ok? x p-side p gp-side gp)
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         ;; Overwrite new root with gp
         (values tx ok? gp #f #f)]
        [p-side ;; we have two splay path segments; splay
         (set-node-side! p p-side x)
         (cond [(eq? p-side gp-side)
                ;; zig-zig
                (rotate! tx p p-side)
                (set-node-side! gp gp-side x)
                (rotate! tx gp gp-side)
                (values tx ok? x #f #f)]
               [else
                ;; zig-zag
                (rotate! tx p p-side)
                (set-node-side! gp gp-side x)
                (rotate! tx gp gp-side)
                (values tx ok? x #f #f)])]
        [else
         (values tx ok? x gp-side gp)]))

(define (finish tx ok? x p-side p)
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         (values ok? #f)]
        [p-side ;; one splay path segment left; perform zig
         (set-node-side! p p-side x)
         (rotate! tx p p-side)
         (values ok? x)]
        [else ;; no splay path segments left
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
         (set-node-key! B (+ (node-key B) Kx))))]))

(define (left! tx p)
  (match p
    [(node Kp _ A (and x (node Kx _ B C)))
     (set-node-right! p B)
     (set-node-left! x p)
     (when tx
       (set-node-key! p (- 0 Kx))
       (set-node-key! x (+ Kp Kx))
       (when B
         (set-node-key! B (+ (node-key B) Kx))))]))

;; --------

;; if left is node, new root is max(left)
(define (join-left tx left right)
  (cond [(and left right)
         (let-values ([(_ok? left*) (find-max tx left)])
           ;; left* is node, left*.right = #f
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

(define (remove-range! cmp tx root from to contract?)
  ;; tx = #t... why pass as param?
  (let*-values ([(ok? from-node) (find/root cmp tx from root (list #f))]
                [(left-tree right-tree)
                 (if (eq? ok? 'added)
                     (split/drop-root tx from-node)
                     (split/root-to-right tx from-node))]
                [(ok? to-node) (find/root cmp tx to right-tree (list #f))]
                [(mid-tree right-tree)
                 (if (eq? ok? 'added)
                     (split/drop-root tx to-node)
                     (split/root-to-right tx to-node))])
    (when (and tx contract?)
      (when right-tree
        (set-node-key! right-tree (+ (node-key right-tree) (- from to)))))
    (join-left tx left-tree right-tree)))

(define (expand! cmp tx root from to)
  (let*-values ([(ok? from-node) (find/root cmp tx from root (list #f))]
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
       (when (and (eq? ok? 'added) size)
         (set-splay-tree-size! s (add1 size)))
       (unless (eq? (node-value root) v)
         (set-node-value! root v)))]))

(define (splay-tree-remove! s x)
  (match s
    [(splay-tree root size cmp tx)
     (let-values ([(ok? root) (find/root cmp tx x root #f)])
       (when ok? ;; => root is node
         (set-splay-tree-root! s (delete-root tx root))
         (when size (set-splay-tree-size! s (sub1 size)))))]))

(define (splay-tree-count s)
  (let ([size (splay-tree-size s)])
    (if size
        size
        (let ([size (let loop ([x (splay-tree-root s)] [n 0])
                      (if x
                          (loop (node-left x) (loop (node-right x) (add1 n)))
                          n))])
          (set-splay-tree-size! s size)
          size))))

(define (splay-tree-remove-range! s from to)
  (match s
    [(splay-tree root size cmp tx)
     (set-splay-tree-root! s (remove-range! cmp tx root from to #f))
     (set-splay-tree-size! s #f)]))

(define (splay-tree-contract! s from to)
  (match s
    [(splay-tree root size cmp tx)
     (set-splay-tree-root! s (remove-range! cmp tx root from to #t))
     (set-splay-tree-size! s #f)]))

(define (splay-tree-expand! s from to)
  (match s
    [(splay-tree root size cmp tx)
     (set-splay-tree-root! s (expand! cmp tx root from to))]))

;; ========

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
 4) position = node (doesn't work with position-relative keys)

(1,4) are no good. (3) is not very iterator-like.

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
     (splay-tree-iterate-least/>? s key)]))

(define (splay-tree-iterate-key s pos)
  (match pos
    [(splay-tree-iter key) key]))

(define (splay-tree-iterate-value s pos)
  (match pos
    [(splay-tree-iter key) (splay-tree-ref s key #f)]))

;; ========

(struct splay-tree ([root #:mutable] [size #:mutable] cmp tx)
        #:transparent
        #:property prop:dict/contract
        (list (vector-immutable splay-tree-ref
                                splay-tree-set!
                                #f ;; set
                                splay-tree-remove!
                                #f ;; remove
                                splay-tree-count
                                splay-tree-iterate-first
                                splay-tree-iterate-next
                                splay-tree-iterate-key
                                splay-tree-iterate-value)
              (vector-immutable any/c
                                any/c
                                splay-tree-iter?
                                #f #f #f)))

(struct splay-tree* splay-tree (key-c value-c)
        #:transparent
        #:property prop:dict/contract
        (list (vector-immutable splay-tree-ref
                                splay-tree-set!
                                #f ;; set
                                splay-tree-remove!
                                #f ;; remove
                                splay-tree-count
                                splay-tree-iterate-first
                                splay-tree-iterate-next
                                splay-tree-iterate-key
                                splay-tree-iterate-value)
              (vector-immutable any/c
                                any/c
                                splay-tree-iter?
                                (lambda (s) (splay-tree*-key-c s))
                                (lambda (s) (splay-tree*-value-c s))
                                #f)))

(define-syntax-rule (mkcmp <? =?)
  (lambda (x y) (cond [(=? x y) '=] [(<? x y) '<] [else '>])))

(define (make-splay-tree <? =?
                         #:key-contract [key-contract any/c]
                         #:value-contract [value-contract any/c])
  (cond [(and (eq? key-contract any/c) (eq? value-contract any/c))
         (splay-tree #f 0 (mkcmp <? =?) #f)]
        [else
         (splay-tree* #f 0 (mkcmp <? =?) #f key-contract value-contract)]))

#|
In an integer splay tree, keys can be stored relative to their parent nodes.
|#
(define (make-integer-splay-tree #:adjust? [adjust? #f]
                                 #:key-contract [key-contract any/c]
                                 #:value-contract [value-contract any/c])
  (splay-tree* #f 0 (mkcmp < =) (and adjust? #t)
               (if (eq? key-contract any/c)
                   exact-integer?
                   (and/c exact-integer? key-contract))
               value-contract))

(define (splay-tree-with-adjust? s)
  (splay-tree-tx s))

(define (key-c s)
  (if (splay-tree*? s) (splay-tree*-key-c s) any/c))
(define (val-c s)
  (if (splay-tree*? s) (splay-tree*-value-c s) any/c))

;; ========

;; Order-based search

(define (extreme who s key cmp-result has-X? find-X)
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
       (and ok? (splay-tree-iter root)))]))

(define (splay-tree-iterate-greatest/<=? s key)
  (extreme 'splay-tree-iterate-greatest/<=? s key '(< =) has-prev? find-prev))
(define (splay-tree-iterate-greatest/<? s key)
  (extreme 'splay-tree-iterate-greatest/<? s key '(<) has-prev? find-prev))
(define (splay-tree-iterate-least/>=? s key)
  (extreme 'splay-tree-iterate-least/>=? s key '(> =) has-next? find-next))
(define (splay-tree-iterate-least/>? s key)
  (extreme 'splay-tree-iterate-least/>? s key '(>) has-next? find-next))

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
 [make-splay-tree
  (->* ((-> any/c any/c any/c) (-> any/c any/c any/c))
       (#:key-contract contract? #:value-contract contract?)
       splay-tree?)]
 [make-integer-splay-tree
  (->* ()
       (#:adjust? any/c #:key-contract contract? #:value-contract contract?)
       splay-tree?)]

 [splay-tree? (-> any/c boolean?)]
 [splay-tree-with-adjust? (-> splay-tree? boolean?)]

 [splay-tree-ref
  (->i ([s splay-tree?] [key (s) (key-c s)])
       ([default any/c])
       [_ (s default) (or/c (key-c s) (lambda (x) (eq? x default)))])]
 [splay-tree-set!
  (->i ([s splay-tree?] [key (s) (key-c s)] [v (s) (val-c s)]) [_ void?])]
 [splay-tree-remove!
  (->i ([s splay-tree?] [key (s) (key-c s)]) [_ void?])]
 [splay-tree-remove-range!
  (->i ([s splay-tree?] [from (s) (key-c s)] [to (s) (key-c s)]) [_ void?])]
 [splay-tree-count
  (-> splay-tree? exact-nonnegative-integer?)]
 [splay-tree->list
  (->i ([s splay-tree?]) [_ (s) (listof (cons/c (key-c s) (val-c s)))])]

 [splay-tree-contract!
  (->i ([s (and/c splay-tree? splay-tree-with-adjust?)]
        [from (s) (key-c s)] [to (s) (key-c s)])
       [_ void?])]
 [splay-tree-expand!
  (->i ([s (and/c splay-tree? splay-tree-with-adjust?)]
        [from (s) (key-c s)] [to (s) (key-c s)])
       [_ void?])]

 [splay-tree-iterate-first
  (-> splay-tree? (or/c splay-tree-iter? #f))]
 [splay-tree-iterate-next
  (-> splay-tree? splay-tree-iter? (or/c splay-tree-iter? #f))]
 [splay-tree-iterate-key
  (-> splay-tree? splay-tree-iter? any/c)]
 [splay-tree-iterate-value
  (-> splay-tree? splay-tree-iter? any/c)]

 [splay-tree-iterate-greatest/<=?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_ (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-greatest/<?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_ (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-least/>=?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_ (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-least/>?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_ (or/c splay-tree-iter? #f)])])
