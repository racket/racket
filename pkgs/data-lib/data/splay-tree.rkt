#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/dict
         racket/contract/base
	 racket/generic
         "order.rkt")

#|
This library contains two implementations of splay trees.

node-splay-tree:
  - nodes are separate structures
  - bottom-up splay (no allocation)
  - fast expand!/contract!/remove-range! via parent-relative keys
  - specialized to integer keys

compact-splay-tree:
  - nodes packed in vector
  - top-down splay (constant preallocated scratch node)
  - 2-3x faster than *unspecialized* node-based splay-tree
    - from vector packing, not from top-down splay

If anyone wants to adapt the top-down splay algorithm to work with
parent-relative keys, we can get rid of node-splay-tree entirely.
|#



;; ============================================================
;; Common
;; ============================================================

(define not-given (gensym 'not-given))

(struct splay-tree-iter (key))

(define-syntax-rule (mkcmp <? =?)
  (lambda (x y) (cond [(=? x y) '=] [(<? x y) '<] [else '>])))

(define intcmp (mkcmp < =))


;; ============================================================
;; Node splay tree
;; ============================================================

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

<SPB> = Node/#f Node/#f
SP = (values Status Node/#f <SPB>)
   = (values Status Node/#f Side/#f Node/#f)
Status = 'found | 'added | #f
Side = 'left | 'right

In (values status nroot pside pnode):
  nroot is the new root (or #f)
  if pside and pnode are both non-#f,
    pnode is next node in splay path, overrides nroot as new root IF nroot = #f
  if pside and pnode are both #f,
    no pending rotation; add it and keep going...

|#

(define-syntax-rule (SPfinish expr)
  (let-values ([(ok? x p-side p) expr])
    (finish ok? x p-side p)))

(define-syntax-rule (SPisplay x-expr gp-side gp)
  (let-values ([(ok? x p-side p) x-expr])
    (isplay! ok? x p-side p gp-side gp)))

(define (SPunit x) (values 'found x #f #f))
(define (SPunit/add x) (values 'added x #f #f))
(define (SPfail) (values #f #f #f #f))

;; --------

;; find : ... -> (values status node/#f)
;; If ok?, then node returned is one sought.
(define (n:find k x add-v)
  (SPfinish (findb k x #f #f add-v)))

;; findb : ... -> SP
(define (findb k x p-side p add-v)
  (cond [x
         (let ([k* (- k (node-key x))])
           (cond [(= k (node-key x))
                  (SPunit x)]
                 [(< k (node-key x))
                  (SPisplay (findb k* (node-left x)  'left  x add-v) 'left x)]
                 [else
                  (SPisplay (findb k* (node-right x) 'right x add-v) 'right x)]))]
        [add-v
         (let ([new-node (node k (car add-v) #f #f)])
           ;; FIXME: link unnecessary? will be done in isplay/finish?
           (when p (set-node-side! p p-side new-node))
           (SPunit/add new-node))]
        [else (SPfail)]))

(define (n:find-min x)
  (define (find-min-loop x)
    (cond [(and x (node-left x))
           (SPisplay (find-min-loop (node-left x)) 'left x)]
          [x (SPunit x)]
          [else (SPfail)]))
  (SPfinish (find-min-loop x)))

(define (n:find-max x)
  (define (find-max-loop x)
    (cond [(and x (node-right x))
           (SPisplay (find-max-loop (node-right x)) 'right x)]
          [x (SPunit x)]
          [else (SPfail)]))
  (SPfinish (find-max-loop x)))

;; isplay! : ... -> SP
;; incremental splay
(define (isplay! ok? x p-side p gp-side gp)
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         ;; Overwrite new root with gp
         (values ok? gp #f #f)]
        [p-side ;; we have two splay path segments; splay
         (set-node-side! p p-side x)
         (cond [(eq? p-side gp-side)
                ;; zig-zig
                (rotate! p p-side)
                (set-node-side! gp gp-side x)
                (rotate! gp gp-side)
                (values ok? x #f #f)]
               [else
                ;; zig-zag
                (rotate! p p-side)
                (set-node-side! gp gp-side x)
                (rotate! gp gp-side)
                (values ok? x #f #f)])]
        [else
         (values ok? x gp-side gp)]))

(define (finish ok? x p-side p)
  (cond [(eq? x #f)
         ;; Then p-side = #f, p = #f
         (values ok? #f)]
        [p-side ;; one splay path segment left; perform zig
         (set-node-side! p p-side x)
         (rotate! p p-side)
         (values ok? x)]
        [else ;; no splay path segments left
         (values ok? x)]))

(define (set-node-side! n side v)
  (case side
    ((left) (set-node-left! n v))
    ((right) (set-node-right! n v))))

(define (rotate! x side)
  (case side
    ((left) (right! x))
    ((right) (left! x))
    ((#f) (void))))

(define (right! p)
  (match p
    [(node Kp _ (and x (node Kx _ A B)) C)
     (set-node-left! p B)
     (set-node-right! x p)
     (set-node-key! p (- 0 Kx))
     (set-node-key! x (+ Kp Kx))
     (when B
       (set-node-key! B (+ (node-key B) Kx)))]))

(define (left! p)
  (match p
    [(node Kp _ A (and x (node Kx _ B C)))
     (set-node-right! p B)
     (set-node-left! x p)
     (set-node-key! p (- 0 Kx))
     (set-node-key! x (+ Kp Kx))
     (when B
       (set-node-key! B (+ (node-key B) Kx)))]))

;; --------

;; if left is node, new root is max(left)
(define (n:join-left left right)
  (cond [(and left right)
         (let-values ([(_ok? left*) (n:find-max left)])
           ;; left* is node, left*.right = #f
           (set-node-right! left* right)
           (set-node-key! right (- (node-key right) (node-key left*)))
           left*)]
        [left left]
        [else right]))

;; if right is node, new root is min(right)
(define (n:join-right left right)
  (cond [(and left right)
         (let-values ([(_ok? right*) (n:find-min right)])
           ;; right* is node, right*.left = #f
           (set-node-left! right* left)
           (set-node-key! left (- (node-key left) (node-key right*)))
           right*)]
        [right right]
        [else left]))

(define (n:split/drop-root root)
  (let ([left (node-left root)]
        [right (node-right root)])
    (when left
      (set-node-key! left (+ (node-key left) (node-key root))))
    (when right
      (set-node-key! right (+ (node-key right) (node-key root))))
    (values left right)))

(define (n:split/root-to-left root)
  (let ([right (node-right root)])
    (when right
      (set-node-key! right (+ (node-key right) (node-key root))))
    (set-node-right! root #f)
    (values root right)))

(define (n:split/root-to-right root)
  (let ([left (node-left root)])
    (when left
      (set-node-key! left (+ (node-key left) (node-key root))))
    (set-node-left! root #f)
    (values left root)))

(define (n:delete-root root)
  (let-values ([(left right) (n:split/drop-root root)])
    (n:join-left left right)))

(define (n:remove-range! root from to contract!?)
  (let*-values ([(ok? from-node) (n:find from root (list #f))]
                [(left-tree right-tree)
                 (if (eq? ok? 'added)
                     (n:split/drop-root from-node)
                     (n:split/root-to-right from-node))]
                [(ok? to-node) (n:find to right-tree (list #f))]
                [(mid-tree right-tree)
                 (if (eq? ok? 'added)
                     (n:split/drop-root to-node)
                     (n:split/root-to-right to-node))])
    (when contract!?
      (when right-tree
        (set-node-key! right-tree (+ (node-key right-tree) (- from to)))))
    (n:join-left left-tree right-tree)))

(define (n:expand! root from to)
  (let*-values ([(ok? from-node) (n:find from root (list #f))]
                [(left-tree right-tree)
                 (if (eq? ok? 'added)
                     (n:split/drop-root from-node)
                     (n:split/root-to-right from-node))])
    (when right-tree
      (set-node-key! right-tree (+ (node-key right-tree) (- to from))))
    (n:join-left left-tree right-tree)))

(define (n:find-prev root)
  ;; PRE: root is node and root.left is node; ie, has-prev?
  (let-values ([(left right) (n:split/root-to-right root)])
    ;; join-left does max(left)
    (n:join-left left right)))

(define (n:find-next root)
  ;; PRE: root is node and root.right is node; ie, has-next?
  (let-values ([(left right) (n:split/root-to-left root)])
    ;; join-right does min(right)
    (n:join-right left right)))

(define (n:has-prev? x) (and x (node-left x) #t))
(define (n:has-next? x) (and x (node-right x) #t))

;; ------------------------------------------------------------
;; Splay tree operations
;; ------------------------------------------------------------

(define (n:splay-tree-ref s x [default not-given])
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root) (n:find x root #f)])
       (set-node-splay-tree-root! s root)
       (if ok?
           (node-value root)
           (cond [(eq? default not-given)
                  (error 'splay-tree-ref "no value found for key: ~e" x)]
                 [(procedure? default)
                  (default)]
                 [else default])))]))

(define (n:splay-tree-set! s x v)
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root) (n:find x root (list v))])
       (set-node-splay-tree-root! s root)
       (when (and (eq? ok? 'added) size)
         (set-node-splay-tree-size! s (add1 size)))
       (unless (eq? (node-value root) v)
         (set-node-value! root v)))]))

(define (n:splay-tree-remove! s x)
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root) (n:find x root #f)])
       (cond [ok? ;; => root is node to remove
              (set-node-splay-tree-root! s (n:delete-root root))
              (when size (set-node-splay-tree-size! s (sub1 size)))]
             [else
              (set-node-splay-tree-root! s root)]))]))

(define (n:splay-tree-count s)
  (let ([size (node-splay-tree-size s)])
    (if size
        size
        (let ([size (let loop ([x (node-splay-tree-root s)] [n 0])
                      (if x
                          (loop (node-left x) (loop (node-right x) (add1 n)))
                          n))])
          (set-node-splay-tree-size! s size)
          size))))

(define (n:splay-tree-remove-range! s from to)
  (match s
    [(node-splay-tree root size)
     (when (< from to)
       (set-node-splay-tree-root! s (n:remove-range! root from to #f))
       (set-node-splay-tree-size! s #f))]))

(define (splay-tree-contract! s from to)
  (match s
    [(node-splay-tree root size)
     (unless (< from to)
       (error 'splay-tree-contract!
              "bad range: ~s to ~s" from to))
     (set-node-splay-tree-root! s (n:remove-range! root from to #t))
     (set-node-splay-tree-size! s #f)]))

(define (splay-tree-expand! s from to)
  (match s
    [(node-splay-tree root size)
     (unless (< from to)
       (error 'splay-tree-expand!
              "bad range: ~s to ~s" from to))
     (set-node-splay-tree-root! s (n:expand! root from to))]))

;; ========

#|
Iteration in splay-trees is problematic.
 - any access to the splay-tree disturbs most notions of "position"
   (other dictionaries, eg hashes, are only disturbed by *updates*)
 - parent-relative keys need parent chain to be interpreted

Options
 1) position = parent chain (very likely to get out of sync)
 2) position = key (re-lookup each time)
 3) snapshot as alist (more allocation than necessary, sometimes much more)
 4) position = node (doesn't work with position-relative keys)

(1,4) are no good. (3) is not very iterator-like.

(2) seems to be the best compromise.
|#

(define (n:splay-tree-iterate-first s)
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root) (n:find-min root)])
       (set-node-splay-tree-root! s root)
       (if ok? (splay-tree-iter (node-key root)) #f))]))

(define (n:splay-tree-iterate-next s pos)
  (match pos
    [(splay-tree-iter key)
     (n:splay-tree-iterate-least/>? s key)]))

(define (n:splay-tree-iterate-key s pos)
  (match pos
    [(splay-tree-iter key) key]))

(define (n:splay-tree-iterate-value s pos)
  (match pos
    [(splay-tree-iter key) (n:splay-tree-ref s key #f)]))

;; Order-based search

(define (n:extreme s key cmp-result has-X? find-X)
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root)
                   (n:extreme* root key cmp-result has-X? find-X)])
       (set-node-splay-tree-root! s root)
       (and ok? (splay-tree-iter (node-key root))))]))

(define (n:extreme* root key cmp-result has-X? find-X)
  (let-values ([(_ok? root) (n:find key root #f)])
    ;; ok? is true if returned root satisfies search criteria
    (cond [(and root (memq (intcmp (node-key root) key) cmp-result))
           (values #t root)]
          [(has-X? root)
           (values #t (find-X root))]
          [else
           (values #f root)])))

(define (n:splay-tree-iterate-greatest/<=? s key)
  (n:extreme s key '(< =) n:has-prev? n:find-prev))
(define (n:splay-tree-iterate-greatest/<? s key)
  (n:extreme s key '(<) n:has-prev? n:find-prev))
(define (n:splay-tree-iterate-least/>=? s key)
  (n:extreme s key '(> =) n:has-next? n:find-next))
(define (n:splay-tree-iterate-least/>? s key)
  (n:extreme s key '(>) n:has-next? n:find-next))

(define (n:splay-tree-iterate-least s)
  (n:splay-tree-iterate-first s))
(define (n:splay-tree-iterate-greatest s)
  (match s
    [(node-splay-tree root size)
     (let-values ([(ok? root) (n:find-max root)])
       (set-node-splay-tree-root! s root)
       (if ok? (splay-tree-iter (node-key root)) #f))]))

;; ========

;; snapshot
(define (n:splay-tree->list s)
  (match s
    [(node-splay-tree root size)
     (let loop ([x root] [onto null] [k* 0])
       (match x
         [(node key value left right)
          (let ([key (+ key k*)])
            (loop left
                  (cons (cons key value)
                        (loop right onto key))
                  key))]
         [#f onto]))]))

;; ------------------------------------------------------------
;; Struct
;; ------------------------------------------------------------

(define n:dict-methods
  (vector-immutable n:splay-tree-ref
                    n:splay-tree-set!
                    #f ;; set
                    n:splay-tree-remove!
                    #f ;; remove
                    n:splay-tree-count
                    n:splay-tree-iterate-first
                    n:splay-tree-iterate-next
                    n:splay-tree-iterate-key
                    n:splay-tree-iterate-value))

(struct node-splay-tree ([root #:mutable] [size #:mutable])
        #:property prop:dict/contract
        (list n:dict-methods
              (vector-immutable exact-integer?
                                any/c
                                splay-tree-iter?
                                #f #f #f))
        #:methods gen:ordered-dict
	[(define dict-iterate-least n:splay-tree-iterate-least)
	 (define dict-iterate-greatest n:splay-tree-iterate-greatest)
	 (define dict-iterate-least/>? n:splay-tree-iterate-least/>?)
	 (define dict-iterate-least/>=? n:splay-tree-iterate-least/>=?)
	 (define dict-iterate-greatest/<? n:splay-tree-iterate-greatest/<?)
	 (define dict-iterate-greatest/<=? n:splay-tree-iterate-greatest/<=?)])

(struct node-splay-tree* node-splay-tree (key-c value-c)
        #:property prop:dict/contract
        (list n:dict-methods
              (vector-immutable any/c
                                any/c
                                splay-tree-iter?
                                (lambda (s) (node-splay-tree*-key-c s))
                                (lambda (s) (node-splay-tree*-value-c s))
                                #f))
        #:methods gen:ordered-dict
        [(define dict-iterate-least n:splay-tree-iterate-least)
	 (define dict-iterate-greatest n:splay-tree-iterate-greatest)
	 (define dict-iterate-least/>? n:splay-tree-iterate-least/>?)
	 (define dict-iterate-least/>=? n:splay-tree-iterate-least/>=?)
	 (define dict-iterate-greatest/<? n:splay-tree-iterate-greatest/<?)
	 (define dict-iterate-greatest/<=? n:splay-tree-iterate-greatest/<=?)])



;; ============================================================
;; Compact splay tree
;; ============================================================

;; Mem = vector: [key, value, left, right] ...
;; Node = nat, multiple of NODE-SIZE

;; First "node" of Mem is always Scratch
;; (node-key Scratch) = size = next *fresh* node
;; (node-value Scratch) = #f or free-list head (Node)

;; If N in free-list:
;;   (node-key N) = number of nodes in free-list here on (self included)
;;   (node-value N) = #f or next node in free-list

(define NODE-SIZE 4)

;; min number of vector slots
;; (ie, (MIN-SIZE / NODE-SIZE) - 1 is min number of data nodes
(define MIN-SIZE 16)

(define-syntax-rule (dvf [ref set offset] ...)
  (begin (define (ref mem node)
           (vector-ref mem (+ node offset))) ...
         (define (set mem node v)
           (vector-set! mem (+ node offset) v)) ...))
(dvf [vnode-key set-vnode-key! 0]
     [vnode-value set-vnode-value! 1]
     [vnode-left set-vnode-left! 2]
     [vnode-right set-vnode-right! 3])

(define scratch 0)
(define (v:next mem)
  (vnode-key mem scratch))
(define (v:set-next! mem v)
  (set-vnode-key! mem scratch v))

(define (v:free-list mem)
  (vnode-value mem scratch))

(define (v:push-free! mem n)
  (let ([head (v:free-list mem)])
    (set-vnode-value! mem n head)
    (set-vnode-key! mem n
                    (if head
                        (add1 (vnode-key mem head))
                        1)))
  (set-vnode-value! mem scratch n))
(define (v:pop-free! mem)
  (let* ([head (v:free-list mem)]
         [next (vnode-value mem head)])
    (set-vnode-value! mem scratch next)
    head))

;; number of nodes (not including scratch)
(define (v:size mem)
  (let ([free (v:free-list mem)])
    (- (sub1 (quotient (v:next mem) NODE-SIZE))
       (if free
           (vnode-key mem free) ;; size of free list
           0))))

(define (valloc! mem)
  (if (vnode-value mem scratch) ;; free-list head
      (v:pop-free! mem)
      (let ([next (v:next mem)])
        (v:set-next! mem (+ NODE-SIZE next))
        next)))

(define (vnode! mem key value left right)
  (let ([node (valloc! mem)])
    (set-vnode-key! mem node key)
    (set-vnode-value! mem node value)
    (set-vnode-left! mem node left)
    (set-vnode-right! mem node right)
    node))

;; find : ... -> (values boolean node/#f)
;; If ok?, then node returned is one sought.
;; PRE: if add-v, then (size mem) + NODE-SIZE <= (vector-length mem)
;;   that is, room for at least one node
(define (v:find cmp k mem x add-v)
  (v:findt cmp k mem x add-v))

#|
Top-down splay
|#
(define (v:findt cmp k mem x add-v)
  (cond [x
         (set-vnode-left! mem scratch #f)
         (set-vnode-right! mem scratch #f)
         (v:findt* cmp k mem x scratch scratch add-v)]
        [add-v
         (values 'added (vnode! mem k (car add-v) #f #f))]
        [else
         (values #f #f)]))

(define (v:find-min mem x)
  (if x
      (let-values ([(_ok? root) (v:findt (lambda (x y) '<) 'dummy mem x #f)])
        (values 'found root))
      (values #f #f)))

(define (v:find-max mem x)
  (if x
      (let-values ([(_ok? root) (v:findt (lambda (x y) '>) 'dummy mem x #f)])
        (values 'found root))
      (values #f #f)))

(define (v:findt* cmp k mem t l r add-v)
  (define-syntax-rule (finish! status t l r)
    (assemble! status mem t scratch l r))
  (define-syntax-rule (continue t l r)
    (v:findt* cmp k mem t l r add-v))
  (define-syntax-rule (rotate&link cmpresult rl l r
                                   (vnode-A set-vnode-A!)
                                   (vnode-B set-vnode-B!))
    (let ([tA (vnode-A mem t)])
      (cond [tA
             (let ([c (cmp k (vnode-key mem tA))])
               (case c
                 ((cmpresult) ;; k should be on A-side of tA
                  (set-vnode-A! mem t (vnode-B mem tA))
                  (set-vnode-B! mem tA t)
                  (let ([tAA (vnode-A mem tA)])
                    (cond [tAA
                           (set-vnode-A! mem rl tA)
                           (let ([rl tA]) ;; shadows either l or r
                             (continue tAA l r))]
                          [add-v
                           (let ([tAA (vnode! mem k (car add-v) #f #f)])
                             (set-vnode-A! mem tA tAA)
                             (set-vnode-A! mem rl tA)
                             (let ([rl tA]) ;; shadows either l or r
                               (finish! 'added tAA l r)))]
                          [else
                           (finish! #f tA l r)])))
                 (else
                  (set-vnode-A! mem rl t)
                  (let ([rl t]) ;; shadows either l or r
                    (continue tA l r)))))]
            [add-v
             (let ([tA (vnode! mem k (car add-v) #f #f)])
               (set-vnode-A! mem t tA)
               (set-vnode-A! mem rl t)
               (let ([rl t]) ;; shadows either l or r
                 (finish! 'added tA l r)))]
            [else
             (finish! #f t l r)])))
  (case (cmp k (vnode-key mem t))
    ((<)
     (rotate&link < r l r (vnode-left set-vnode-left!) (vnode-right set-vnode-right!)))
    ((>)
     (rotate&link > l l r (vnode-right set-vnode-right!) (vnode-left set-vnode-left!)))
    (else
     (finish! 'found t l r))))

(define (assemble! status mem t scratch l r)
  (set-vnode-right! mem l (vnode-left mem t))
  (set-vnode-left! mem r (vnode-right mem t))
  (set-vnode-left! mem t (vnode-right mem scratch))
  (set-vnode-right! mem t (vnode-left mem scratch))
  (values status t))

;; --------

;; if left is node, new root is max(left)
(define (v:join-left mem left right)
  (cond [(and left right)
         (let-values ([(_ok? left*) (v:find-max mem left)])
           ;; left* is node, left*.right = #f
           (set-vnode-right! mem left* right)
           left*)]
        [left left]
        [else right]))

;; if right is node, new root is min(right)
(define (v:join-right mem left right)
  (cond [(and left right)
         (let-values ([(_ok? right*) (v:find-min mem right)])
           ;; right* is node, right*.left = #f
           (set-vnode-left! mem right* left)
           right*)]
        [right right]
        [else left]))

(define (v:split/drop-root mem root cmp)
  (let ([root-key (vnode-key mem root)]
        [left (vnode-left mem root)]
        [right (vnode-right mem root)])
    (v:push-free! mem root)
    (values left right)))

(define (v:split/root-to-left mem root)
  (let ([right (vnode-right mem root)])
    (set-vnode-right! mem root #f)
    (values root right)))

(define (v:split/root-to-right mem root)
  (let ([left (vnode-left mem root)])
    (set-vnode-left! mem root #f)
    (values left root)))

(define (v:delete-root mem root cmp)
  (let-values ([(left right) (v:split/drop-root mem root cmp)])
    (v:join-left mem left right)))

#|
(define (v:remove-range! mem root cmp from to)
  (let loop ([root root])
    (let-values ([(ok? root)
                  (v:extreme* mem root cmp from '(> =) v:has-next? v:find-next)])
      (if (and ok? (eq? (cmp (vnode-key mem root) to) '<))
          (loop (v:delete-root mem root cmp))
          root))))
|#

(define (v:remove-range! mem root cmp from to)
  (let*-values ([(ok? from-node) ;; least >= from
                 (v:extreme* mem root cmp from
                             '(> =) v:has-next? v:find-next)]
                [(left-tree mid+right-tree)
                 (v:split/root-to-right mem from-node)]
                [(ok? to-node) ;; least >= to
                 (v:extreme* mem mid+right-tree cmp to
                             '(> =) v:has-next? v:find-next)]
                [(mid-tree right-tree)
                 (v:split/root-to-right mem to-node)])
    ;; Remove everything rooted at mid-tree.
    (let loop ([n mid-tree])
      (when n
        (loop (vnode-left mem n))
        (loop (vnode-right mem n))
        (set-vnode-left! mem n #f) ;; not strictly necessary
        (set-vnode-right! mem n #f)
        (v:push-free! mem n))) ;; overwrites key, value
    ;; Join left and right trees.
    (v:join-left mem left-tree right-tree)))

(define (v:find-prev mem root)
  ;; PRE: root is node and root.left is node; ie, has-prev?
  (let-values ([(left right) (v:split/root-to-right mem root)])
    ;; join-left does max(left)
    (v:join-left mem left right)))

(define (v:find-next mem root)
  ;; PRE: root is node and root.right is node; ie, has-next?
  (let-values ([(left right) (v:split/root-to-left mem root)])
    ;; join-right does min(right)
    (v:join-right mem left right)))

(define (v:has-prev? mem x) (and x (vnode-left mem x) #t))
(define (v:has-next? mem x) (and x (vnode-right mem x) #t))

;; ------------------------------------------------------------
;; Splay tree operations
;; ------------------------------------------------------------

(define (v:splay-tree-ref s x [default not-given])
  (match s
    [(compact-splay-tree mem root cmp)
     (let-values ([(ok? root) (v:find cmp x mem root #f)])
       (set-compact-splay-tree-root! s root)
       (if ok?
           (vnode-value mem root)
           (cond [(eq? default not-given)
                  (error 'splay-tree-ref "no value found for key: ~e" x)]
                 [(procedure? default)
                  (default)]
                 [else default])))]))

(define (v:splay-tree-set! s x v)
  (match s
    [(compact-splay-tree mem root cmp)
     (let ([mem
            ;; ensure at least one free node
            (cond [(v:free-list mem) mem]
                  [(<= (+ NODE-SIZE (v:next mem)) (vector-length mem)) mem]
                  [else ;; no free, can make simple copy
                   (let ([mem* (make-vector (* (vector-length mem) 2) #f)])
                     (vector-copy! mem* 0 mem)
                     (set-compact-splay-tree-mem! s mem*)
                     mem*)])])
       (let-values ([(ok? root) (v:find cmp x mem root (list v))])
         (set-compact-splay-tree-root! s root)
         (unless (eq? (vnode-value mem root) v)
           (set-vnode-value! mem root v))))]))

(define (v:splay-tree-remove! s x)
  (match s
    [(compact-splay-tree mem root cmp)
     (let-values ([(ok? root) (v:find cmp x mem root #f)])
       (cond [ok? ;; => root is node to remove
              (let ([root (v:delete-root mem root cmp)])
                (set-compact-splay-tree-root! s root)
                (v:check-size s mem root))]
             [else
              (set-compact-splay-tree-root! s root)]))]))

(define (v:splay-tree-count s)
  (match s
    [(compact-splay-tree mem root cmp)
     (v:size mem)]))

(define (v:splay-tree-remove-range! s from to)
  (match s
    [(compact-splay-tree mem root cmp)
     (when (eq? (cmp from to) '<)
       (let ([root (v:remove-range! mem root cmp from to)])
         (set-compact-splay-tree-root! s root)
         (v:check-size s mem root)))]))

(define (v:check-size s mem root)
  (when (and (< (* 2 (v:size mem)) (quotient (vector-length mem) NODE-SIZE))
             (>= (quotient (vector-length mem) 2) MIN-SIZE))
    (let ([mem* (make-vector (quotient (vector-length mem) 2) #f)])
      ;; condensing copy
      (v:set-next! mem* NODE-SIZE)
      (let loop ([n root])
        (cond [n
               (let ([n* (vnode! mem* (vnode-key mem n) (vnode-value mem n) #f #f)])
                 (set-vnode-left! mem* n* (loop (vnode-left mem n)))
                 (set-vnode-right! mem* n* (loop (vnode-right mem n)))
                 n*)]
              [else #f]))
      (set-compact-splay-tree-root! s NODE-SIZE)
      (set-compact-splay-tree-mem! s mem*))))

;; ========

(define (v:splay-tree-iterate-first s)
  (match s
    [(compact-splay-tree mem root cmp)
     (let-values ([(ok? root) (v:find-min mem root)])
       (set-compact-splay-tree-root! s root)
       (if ok? (splay-tree-iter (vnode-key mem root)) #f))]))

(define (v:splay-tree-iterate-next s pos)
  (match pos
    [(splay-tree-iter key)
     (v:splay-tree-iterate-least/>? s key)]))

(define (v:splay-tree-iterate-key s pos)
  (match pos
    [(splay-tree-iter key) key]))

(define (v:splay-tree-iterate-value s pos)
  (match pos
    [(splay-tree-iter key) (v:splay-tree-ref s key #f)]))

;; Order-based search

(define (v:extreme s key cmp-result has-X? find-X)
  (match s
    [(compact-splay-tree mem root cmp)
     (let-values ([(ok? root)
                   (v:extreme* mem root cmp key cmp-result has-X? find-X)])
       (set-compact-splay-tree-root! s root)
       (and ok? (splay-tree-iter (vnode-key mem root))))]))

(define (v:extreme* mem root cmp key cmp-result has-X? find-X)
  (let*-values ([(_ok? root) (v:find cmp key mem root #f)])
    ;; ok? is true when root returned satisfies search criteria
    (cond [(and root (memq (cmp (vnode-key mem root) key) cmp-result))
           (values #t root)]
          [(has-X? mem root)
           (values #t (find-X mem root))]
          [else
           (values #f root)])))

(define (v:splay-tree-iterate-greatest/<=? s key)
  (v:extreme s key '(< =) v:has-prev? v:find-prev))
(define (v:splay-tree-iterate-greatest/<? s key)
  (v:extreme s key '(<) v:has-prev? v:find-prev))
(define (v:splay-tree-iterate-least/>=? s key)
  (v:extreme s key '(> =) v:has-next? v:find-next))
(define (v:splay-tree-iterate-least/>? s key)
  (v:extreme s key '(>) v:has-next? v:find-next))

(define (v:splay-tree-iterate-least s)
  (v:splay-tree-iterate-first s))
(define (v:splay-tree-iterate-greatest s)
  (match s
    [(compact-splay-tree mem root cmp)
     (let-values ([(ok? root) (v:find-max mem root)])
       (set-compact-splay-tree-root! s root)
       (if ok? (splay-tree-iter (vnode-key mem root)) #f))]))

;; ========

;; snapshot
(define (v:splay-tree->list s)
  (match s
    [(compact-splay-tree mem root cmp)
     (let loop ([x root] [onto null])
       (cond [x (loop (vnode-left mem x)
                      (cons (cons (vnode-key mem x) (vnode-value mem x))
                            (loop (vnode-right mem x) onto)))]
             [else onto]))]))

;; ------------------------------------------------------------
;; Struct
;; ------------------------------------------------------------

(define v:dict-methods
  (vector-immutable v:splay-tree-ref
                    v:splay-tree-set!
                    #f ;; set
                    v:splay-tree-remove!
                    #f ;; remove
                    v:splay-tree-count
                    v:splay-tree-iterate-first
                    v:splay-tree-iterate-next
                    v:splay-tree-iterate-key
                    v:splay-tree-iterate-value))

(define v:ordered-dict-methods
  (vector-immutable v:splay-tree-iterate-least
                    v:splay-tree-iterate-greatest
                    v:splay-tree-iterate-least/>?
                    v:splay-tree-iterate-least/>=?
                    v:splay-tree-iterate-greatest/<?
                    v:splay-tree-iterate-greatest/<=?))

(struct compact-splay-tree ([mem #:mutable] [root #:mutable] cmp)
        #:property prop:dict/contract
        (list v:dict-methods
              (vector-immutable any/c
                                any/c
                                splay-tree-iter?
                                #f #f #f))
        #:property prop:ordered-dict
        v:ordered-dict-methods)

(struct compact-splay-tree* compact-splay-tree (key-c value-c)
        #:property prop:dict/contract
        (list v:dict-methods
              (vector-immutable any/c
                                any/c
                                splay-tree-iter?
                                (lambda (s) (compact-splay-tree*-key-c s))
                                (lambda (s) (compact-splay-tree*-value-c s))
                                #f))
        #:property prop:ordered-dict
        v:ordered-dict-methods)



;; ============================================================
;; Constructors, predicates
;; ============================================================

(define (make-splay-tree [ord datum-order]
                         #:key-contract [key-contract any/c]
                         #:value-contract [value-contract any/c])
  (*make-splay-tree (order-comparator ord)
                    (and/c* (order-domain-contract ord) key-contract)
                    value-contract))

(define (make-adjustable-splay-tree #:key-contract [key-contract any/c]
                                    #:value-contract [value-contract any/c])
  (cond [(and (eq? key-contract any/c) (eq? value-contract any/c))
         (node-splay-tree #f 0)]
        [else
         (node-splay-tree* #f 0 key-contract value-contract)]))

(define (*make-splay-tree cmp key-contract value-contract)
  (let ([mem (make-vector (* NODE-SIZE 4) #f)])
    (set-vnode-key! mem scratch 4)
    (cond [(and (eq? key-contract any/c) (eq? value-contract any/c))
           (compact-splay-tree mem #f cmp)]
          [else
           (compact-splay-tree* mem #f cmp key-contract value-contract)])))

(define (splay-tree? x)
  (or (node-splay-tree? x) (compact-splay-tree? x)))

(define (adjustable-splay-tree? s)
  (node-splay-tree? s))

(define (and/c* x y)
  (cond [(eq? x any/c) y]
        [(eq? y any/c) x]
        [else (and/c x y)]))

;; ============================================================
;; Splay trees
;; ============================================================

(define (splay-tree-ref s x [default not-given])
  (if (compact-splay-tree? s)
      (v:splay-tree-ref s x default)
      (n:splay-tree-ref s x default)))

(define-syntax (defboth stx)
  (syntax-case stx ()
    [(defboth (f p0 p ...) ...)
     (with-syntax ([(v:f ...) (map (lambda (f) (format-id f "v:~a" f))
                                   (syntax->list #'(f ...)))]
                   [(n:f ...) (map (lambda (f) (format-id f "n:~a" f))
                                   (syntax->list #'(f ...)))])
       #'(begin (define (f p0 p ...)
                  (if (compact-splay-tree? p0)
                      (v:f p0 p ...)
                      (n:f p0 p ...)))
                ...))]))

(defboth
  (splay-tree-set! s x v)
  (splay-tree-remove! s x)
  (splay-tree-count s)
  (splay-tree-remove-range! s from to)
  (splay-tree-iterate-first s)
  (splay-tree-iterate-next s pos)
  (splay-tree-iterate-key s pos)
  (splay-tree-iterate-value s pos)
  (splay-tree-iterate-greatest/<=? s key)
  (splay-tree-iterate-greatest/<? s key)
  (splay-tree-iterate-least/>=? s key)
  (splay-tree-iterate-least/>? s key)
  (splay-tree-iterate-least s)
  (splay-tree-iterate-greatest s)
  (splay-tree->list s))


;; ============================================================
;; provide/contract
;; ============================================================

(define (key-c s)
  (cond [(compact-splay-tree*? s) (compact-splay-tree*-key-c s)]
        [(node-splay-tree*? s)
         (and/c* exact-integer? (node-splay-tree*-key-c s))]
        [(node-splay-tree? s) exact-integer?]
        [else any/c]))
(define (val-c s)
  (cond [(compact-splay-tree*? s) (compact-splay-tree*-value-c s)]
        [(node-splay-tree*? s) (node-splay-tree*-value-c s)]
        [else any/c]))

(provide/contract
 [make-splay-tree
  (->* ()
       (order? #:key-contract contract? #:value-contract contract?)
       splay-tree?)]
 [make-adjustable-splay-tree
  (->* ()
       (#:key-contract contract? #:value-contract contract?)
       splay-tree?)]
 #|
 [make-datum-splay-tree
  (->* ()
       (#:key-contract contract? #:value-contract contract?)
       splay-tree?)]
 |#

 [splay-tree? (-> any/c boolean?)]
 [adjustable-splay-tree? (-> any/c boolean?)]

 [splay-tree-ref
  (->i ([s splay-tree?] [key (s) (key-c s)])
       ([default any/c])
       any)]
 [splay-tree-set!
  (->i ([s splay-tree?] [key (s) (key-c s)] [v (s) (val-c s)]) [_r void?])]
 [splay-tree-remove!
  (->i ([s splay-tree?] [key (s) (key-c s)]) [_r void?])]
 [splay-tree-remove-range!
  (->i ([s splay-tree?] [from (s) (key-c s)] [to (s) (key-c s)]) [_r void?])]
 [splay-tree-count
  (-> splay-tree? exact-nonnegative-integer?)]
 [splay-tree->list
  (->i ([s splay-tree?]) [_r (s) (listof (cons/c (key-c s) (val-c s)))])]

 [splay-tree-contract!
  (->i ([s adjustable-splay-tree?]
        [from (s) (key-c s)] [to (s) (key-c s)])
       [_r void?])]
 [splay-tree-expand!
  (->i ([s adjustable-splay-tree?]
        [from (s) (key-c s)] [to (s) (key-c s)])
       [_r void?])]

 [splay-tree-iterate-first
  (-> splay-tree? (or/c splay-tree-iter? #f))]
 [splay-tree-iterate-next
  (-> splay-tree? splay-tree-iter? (or/c splay-tree-iter? #f))]
 [splay-tree-iterate-key
  (->i ([s splay-tree?] [i splay-tree-iter?]) [_r (s) (key-c s)])]
 [splay-tree-iterate-value
  (->i ([s splay-tree?] [i splay-tree-iter?]) [_r (s) (val-c s)])]

 [splay-tree-iterate-greatest/<=?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_r (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-greatest/<?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_r (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-least/>=?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_r (or/c splay-tree-iter? #f)])]
 [splay-tree-iterate-least/>?
  (->i ([s splay-tree?] [k (s) (key-c s)]) [_r (or/c splay-tree-iter? #f)])]

 [splay-tree-iterate-least
  (-> splay-tree? (or/c splay-tree-iter? #f))]
 [splay-tree-iterate-greatest
  (-> splay-tree? (or/c splay-tree-iter? #f))]

 [splay-tree-iter? (-> any/c boolean?)])
