#lang racket/base
(require (for-syntax racket/base
                     syntax/for-body)
         (only-in '#%kernel
                  vector-copy
                  vector*-copy vector*-append vector*-set/copy)
         '#%flfxnum
         racket/hash-code
         "private/sort.rkt"
         (only-in "private/for.rkt" prop:stream)
         "private/serialize-structs.rkt")

(#%declare #:unsafe)

;; RRB tree implementation
;;
;; Based on
;;
;;  Phil Bagwell and Tiark Rompf, "RRB-Trees: Efficient Immutable Vectors"
;;
;;  Nicolas Stucki, Tiark Rompf, Vlad Ureche, Phil Bagwell, "RRB Vector: A Practical General Purpose Immutable Sequence"
;;
;;  Jean Niklas L'orange, "Improving RRB-Tree Performance through Transience"
;;
;; and ported from an initial implementation in Rhombus by Alec Mills.

(provide (rename-out [build-treelist treelist])
         make-treelist
         treelist?
         treelist-ref
         treelist-first
         treelist-last
         treelist-set
         treelist-add
         treelist-cons
         treelist-append
         treelist-insert
         treelist-delete
         treelist-take
         treelist-drop
         treelist-take-right
         treelist-drop-right
         treelist-sublist
         treelist-reverse
         treelist-rest
         treelist-length
         treelist-empty?
         empty-treelist
         treelist->vector
         vector->treelist
         treelist->list
         list->treelist
         treelist-map
         treelist-for-each
         treelist-filter
         treelist-member?
         treelist-find
         treelist-index-of
         treelist-flatten
         treelist-append*
         treelist-sort
         in-treelist
         sequence->treelist
         for/treelist
         for*/treelist
         chaperone-treelist
         treelist-chaperone-state)

(module+ unsafe
  (provide
   ;; For constructing a treelist without intermediate wrapper
   ;; structs, instead directly working with the root node,
   ;; size, and depth as separate values:
   unsafe-root-add
   (rename-out [empty-node unsafe-empty-root]
               [treelist unsafe-treelist])
   ;; For implementing mutable-treelist:
   treelist-copy-for-mutable
   treelist-set!
   ;; To perform the same checks, but for mutable:
   check-treelist-index
   check-treelist-end-index
   check-treelist-bound-index
   check-sort-arguments
   check-chaperone-properties
   ;; For printing, comparing, etc., mutable treelists:
   treelist-print
   treelist-equal?
   treelist-hash-code
   impersonate-treelist
   unimpersonate-treelist
   (for-syntax make-for/treelist)))

(define BITS 5)
(define MAX_WIDTH (expt 2 BITS))
(define MASK (- MAX_WIDTH 1))
(define MAX_ERROR 2)

(define (radix index height)
  (bitwise-and (fxrshift index (fx* BITS height)) MASK))

;; a node in the RRB Tree
;;
;;  - a node is fully dense if it has exactly `m` children where `m` is the branching factor of the overall Tree
;;    and each child is also fully dense
;;  - a node is leftwise dense if its first `n - 1` children, where `n` is its total number of children,
;;    are fully dense, and its `n`th child is leftwise dense or fully dense. `n` is allowed to be < `m`
;;  - dense implies leftwise dense, and leaves are always at least leftwise dense
;;  - a node that is not leftwise dense contain a size array `sizes`; leftwise dense node usually don't,
;;    but a transition into leftwise dense is not always detected

(define-syntax Node
  (syntax-rules ()
    [(Node) empty-node]
    [(Node children) children]
    [(Node children sizes) (let ([cs children]
                                 [szs sizes])
                             (unless (variable-reference-from-unsafe? (#%variable-reference))
                               #;(unless (vector? cs) (error 'Node "bad children vector: ~v" cs))
                               (unless (or (not szs) (vector? szs)) (error 'Node "bad sizes vector: ~v" szs)))
                             (if szs (cons cs szs) cs))]))

(define (vector*-take vec n) (vector*-copy vec 0 n))
(define (vector*-drop vec n) (vector*-copy vec n (vector*-length vec)))
(define (vector*-drop-right vec n) (vector*-copy vec 0 (- (vector*-length vec) n)))
(define (vector*-add-left val a) (vector*-append (vector val) a))
(define (vector*-add-right a val) (vector*-append a (vector val)))

(define (assert-node n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (or (vector? n) (and (pair? n) (vector? (car n)) (vector? (cdr n))))
      (error 'node "expected a node: ~v" n))))

(define (node-leftwise-dense? n) (assert-node n) (not (pair? n)))
(define (node-children n) (assert-node n) (if (pair? n) (car n) n))
(define (node-sizes n) (assert-node n) (and (pair? n) (cdr n)))
(define (node-size n) (assert-node n) (vector*-length (node-children n)))
(define (node-first n) (assert-node n) (vector*-ref (node-children n) 0))
(define (node-last n) (assert-node n) (let ([cs (node-children n)])
                                        (vector*-ref cs (fx- (vector*-length cs) 1))))
(define (node-ref n i) (assert-node n) (vector*-ref (node-children n) i))
(define (node-child-set n i v) (assert-node n) (vector*-set/copy (node-children n) i v))
(define (node-length n) (assert-node n) (vector*-length (node-children n)))

;; `node*` refers to a leftwise dense node
(define (assert-node* n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (vector? n)
      (error 'node* "expected a node*: ~v" n))))

(define (node*-children n) (assert-node* n) n)
(define (node*-ref n i) (assert-node* n) (vector*-ref (node*-children n) i))
(define (node*-set n i v) (assert-node* n) (vector*-set/copy (node*-children n) i v))
(define (node*-length n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-size n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-last n) (assert-node* n) (let ([cs (node*-children n)])
                                          (vector*-ref cs (fx- (vector*-length cs) 1))))

(define empty-node (Node (vector)))

(define (leaf v) (Node (vector v)))

(struct treelist (root size height)
  #:sealed
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (lambda (tl port mode)
                                 (treelist-print "treelist" tl port mode))
  #:property prop:equal+hash (list
                              (lambda (tl other-tl recur)
                                (treelist-equal? tl other-tl recur))
                              (lambda (v recur)
                                (treelist-hash-code v recur))
                              (lambda (v recur)
                                (treelist-size v)))
  #:property prop:sequence (lambda (tl)
                             (in-treelist/proc tl))
  #:property prop:stream (vector
                          (lambda (tl) (treelist-empty? tl))
                          (lambda (tl) (treelist-first tl))
                          (lambda (tl) (treelist-rest tl)))
  #:property prop:serializable (make-serialize-info
                                (lambda (tl) (vector (treelist->vector tl)))
                                (cons 'deserialize-treelist
                                      (module-path-index-join '(submod "." deserialize)
                                                              (variable-reference->module-path-index
                                                               (#%variable-reference))))
                                #f
                                (or (current-load-relative-directory)
                                    (current-directory))))

(module+ deserialize
  (provide deserialize-treelist)
  (define deserialize-treelist
    (make-deserialize-info (lambda (vec) (if (vector? vec)
                                             (vector->treelist vec)
                                             (error 'treelist "invalid deserialization")))
                           (lambda () (error "should not get here; cycles not supported"))))
  (module declare-preserve-for-embedding racket/kernel))

(define empty-treelist (treelist empty-node 0 0))

(define build-treelist
  (let ([treelist
         (case-lambda
           [() empty-treelist]
           [(a) (treelist (leaf a) 1 0)]
           [(a b) (treelist (Node (vector a b)) 2 0)]
           [(a b c) (treelist (Node (vector a b c)) 3 0)]
           [ds (list->treelist ds)])])
    treelist))

(define (check-treelist who tl)
  (unless (treelist? tl)
    (raise-argument-error* who 'racket/primitive "treelist?" tl)))

(define (check-treelist-index who tl size index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-range-error* who 'racket/primitive "treelist" "" index tl 0 (fx- size 1))
        (raise-argument-error* who 'racket/primitive "exact-nonnegative-integer?" index)))
  (when (index . fx< . 0)
    (raise-argument-error* who 'racket/primitive "exact-nonnegative-integer?" index))
  (when (index . fx>= . size)
    (raise-range-error* who 'racket/primitive "treelist" "" index tl 0 (fx- size 1))))

(define (check-treelist-bound-index who tl size index at-least what)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-range-error* who 'racket/primitive "treelist" what index tl at-least size)
        (raise-argument-error* who 'racket/primitive "exact-nonnegative-integer?" index)))
  (when (index . fx< . at-least)
    (if (index . fx>= . 0)
        (raise-range-error* who 'racket/primitive "treelist" what index tl at-least size)
        (raise-argument-error* who 'racket/primitive "exact-nonnegative-integer?" index)))
  (when (index . fx> . size)
    (raise-range-error* who 'racket/primitive "treelist" what index tl at-least size)))

(define (check-treelist-end-index who tl size index)
  (check-treelist-bound-index who tl size index 0 ""))

(define (treelist-empty? v)
  (or (eq? v empty-treelist)
      (if (impersonator? v)
          (or (equal? v empty-treelist)
              (if (treelist? v)
                  #f
                  (check-treelist 'treelist-empty? v)))
          ;; compiler will avoid a redundant impersonator check in this `treelist?`:
          (if (treelist? v)
              #f
              (check-treelist 'treelist-empty? v)))))

(define-sequence-syntax in-treelist
  (lambda () #'in-treelist/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ tl-expr)]
       #'[(d)
          (:do-in
           ([(tl) tl-expr])
           (begin
             (unless (variable-reference-from-unsafe? (#%variable-reference))
               (check-treelist 'in-treelist tl))
             (define size (treelist-size tl)))
           ([pos 0]
            [node empty-node]
            [node-pos 0])
           (pos . fx< . size)
           ([(d next-node next-node-pos)
             (if (node-pos . fx< . (node-size node))
                 (values (node-ref node node-pos) node (fx+ node-pos 1))
                 (let-values ([(node node-pos) (treelist-node-for tl pos)])
                   (values (node-ref node node-pos) node (fx+ node-pos 1))))])
           #t
           #t
           ((fx+ pos 1) next-node next-node-pos))]]
      [_ #f])))

(define-for-syntax (make-for/treelist for/fold/derived-id wrap-result-id)
  (lambda (stx)
    (syntax-case stx ()
      [(_ binds body0 body ...)
       (with-syntax ([((pre-body ...) (post-body ...)) (split-for-body stx #'(body0 body ...))]
                     [for/fold/derived for/fold/derived-id]
                     [wrap-result wrap-result-id])
         #`(for/fold/derived #,stx ([root empty-node] [size 0] [height 0]
                                                      #:result (wrap-result
                                                                (if (fx= size 0)
                                                                    empty-treelist
                                                                    (treelist root size height))))
                             binds
             pre-body ...
             (unsafe-root-add root size height
                              (let ()
                                post-body ...))))])))

(define-syntax for/treelist
  (make-for/treelist #'for/fold/derived #'values))
(define-syntax for*/treelist
  (make-for/treelist #'for*/fold/derived #'values))

(define in-treelist/proc
  ;; Slower strategy than the inline version, but the
  ;; `make-do-sequence` protocol requires a single value for the
  ;; position, and allocating a value to track index plus node defeats
  ;; the benefit of threading the current node
  (let ([in-treelist (lambda (tl)
                       (check-treelist 'in-treelist tl)
                       (make-do-sequence
                        (lambda ()
                          (values
                           (lambda (i) (treelist-ref tl i))
                           (lambda (i) (fx+ i 1))
                           0
                           (lambda (i) (i . fx< . (treelist-size tl)))
                           #f
                           #f))))])
    in-treelist))

(define (sequence->treelist s)
  (cond
    [(treelist? s) s]
    [(vector? s) (vector->treelist s)]
    [(list? s) (list->treelist s)]
    [(sequence? s) (for/treelist ([el s]) el)]
    [else
     (raise-argument-error* 'sequence->treelist 'racket/primitive "sequence?" s)]))

(define (treelist-print type-str tl port mode)
  (case mode
    [(#t #f)
     (display "#<" port)
     (display type-str port)
     (unless (zero? (treelist-size tl))
       (display ":" port))]
    [else
     (display "(" port)
     (display type-str port)])
  (for ([e (in-treelist tl)])
    (display " " port)
    (case mode
      [(#t) (write e port)]
      [(#f) (display e port)]
      [else (print e port)]))
  (case mode
    [(#t #f) (display ">" port)]
    [else (display ")" port)]))

(define (treelist-ref tl index)
  (cond
    [(impersonator? tl) (treelist-ref/slow tl index)]
    [else
     (check-treelist 'treelist-ref tl)
     (check-treelist-index 'treelist-ref tl (treelist-size tl) index)
     (define-values (node pos) (treelist-node-for tl index))
     (node-ref node pos)]))

(define (treelist-first tl)
  (cond
    [(impersonator? tl) (treelist-first/slow tl)]
    [else
     (check-treelist 'treelist-first tl)
     (cond
       [(eq? tl empty-treelist)
        (raise-arguments-error* 'treelist-first 'racket/primitive "treelist is empty")]
       [else
        (define-values (node pos) (treelist-node-for tl 0))
        (node-ref node 0)])]))

(define (treelist-last tl)
  (cond
    [(impersonator? tl) (treelist-last/slow tl)]
    [else
     (check-treelist 'treelist-last tl)
     (cond
       [(eq? tl empty-treelist)
        (raise-arguments-error* 'treelist-last 'racket/primitive "treelist is empty")]
       [else
        (define-values (node pos) (treelist-node-for tl (fx- (treelist-size tl) 1)))
        (node-ref node pos)])]))

(define (treelist-node-for tl index)
  (cond
    [(impersonator? tl) (treelist-node-for/slow tl index)]
    [else
     (let walk ([node (treelist-root tl)]
                [index index]
                [height (treelist-height tl)])
       (cond
         [(fx= height 0)
          (values node (bitwise-and index MASK))]
         [(node-leftwise-dense? node)
          (walk (node*-ref node (radix index height)) index (fx- height 1))]
         [else
          (define-values (bi si) (step node index height))
          (walk (node-ref node bi) si (fx- height 1))]))]))

(define (treelist-equal? tl other-tl recur)
  (cond
    [(or (impersonator? tl)
         (impersonator? other-tl))
     (treelist-equal?/slow tl other-tl recur)]
    [else
     (define len (treelist-size tl))
     (cond
       [(not (fx= len (treelist-size other-tl)))
        #f]
       [else
        ;; we could use `for` to iterate through the trees, but
        ;; we implement the traversal manually so that we can detect
        ;; a shared subtree and skip it
        (let loop ([i 0]
                   [a-node empty-node]
                   [a-pos 0]
                   [b-node empty-node]
                   [b-pos 0])
          (cond
            [(fx= i len) #t]
            [(a-pos . >= . (node-size a-node))
             (cond
               [(and (b-pos . >= . (node-size b-node))
                     (shared-subtree-size tl other-tl i))
                => (lambda (len)
                     (loop (fx+ i len) empty-node 0 empty-node 0))]
               [else
                (define-values (a-node a-pos) (treelist-node-for tl i))
                (loop i a-node a-pos b-node b-pos)])]
            [(b-pos . >= . (node-size b-node))
             (define-values (b-node b-pos) (treelist-node-for other-tl i))
             (loop i a-node a-pos b-node b-pos)]
            [else
             (and (recur (node-ref a-node a-pos) (node-ref b-node b-pos))
                  (loop (fx+ i 1) a-node (fx+ a-pos 1) b-node (fx+ b-pos 1)))]))])]))

;; same traversal as `treelist-node-for`, but for two trees at the
;; same time to try to find a shared subtree and return its size
(define (shared-subtree-size tl other-tl index)
  (let walk ([node (treelist-root tl)]
             [other-node (treelist-root other-tl)]
             [index index]
             [other-index index]
             [height (treelist-height tl)]
             [other-height (treelist-height other-tl)])
    (cond
      [(fx= height 0)
       (and (eq? node other-node)
            (node*-size node))]
      [(eq? node other-node)
       (size-subtree node height)]
      [(not (fx= other-height height))
       (cond
         [(fx< other-height height)
          (define-values (bi si) (if (node-leftwise-dense? node)
                                     (values (radix index height) index)
                                     (step node index height)))
          (walk (node-ref node bi)
                other-node
                si
                other-index
                (fx- height 1)
                other-height)]
         [else (walk other-node
                     node
                     other-index
                     index
                     other-height
                     height)])]
      [else
       (define-values (bi si) (if (node-leftwise-dense? node)
                                  (values (radix index height) index)
                                  (step node index height)))
       (define-values (other-bi other-si) (if (node-leftwise-dense? other-node)
                                              (values (radix other-index height) other-index)
                                              (step other-node other-index height)))
       (walk (node-ref node bi)
             (node-ref other-node other-bi)
             si
             other-si
             (fx- height 1)
             (fx- height 1))])))

(define (treelist-hash-code tl recur)
  ;; limit the number of elements that we inspect to 48
  (define len (treelist-size tl))
  (cond
    [(len . fx< . 48)
     (for/fold ([hc 0]) ([elem (in-treelist tl)])
       (hash-code-combine hc (recur elem)))]
    [else
     (hash-code-combine
      ;; first 16
      (for/fold ([hc 0]) ([i (in-range 0 16)])
        (hash-code-combine hc (recur (treelist-ref tl i))))
      ;; sparse middle 16
      (let* ([n (fx- len 32)]
             [skip (quotient n 16)])
        (for/fold ([hc 0]) ([i (in-range 16 (fx- len 16) skip)])
          (hash-code-combine hc (recur (treelist-ref tl i)))))
      ;; last 16
      (for/fold ([hc 0]) ([i (in-range (fx- len 16) len)])
        (hash-code-combine hc (recur (treelist-ref tl i)))))]))

;; functionally update the slot at `index` to `el`
(define (treelist-set tl index el)
  (cond
    [(impersonator? tl)
     (treelist-set/slow tl index el)]
    [else
     (check-treelist 'treelist-set tl)
     (define size (treelist-size tl))
     (check-treelist-index 'treelist-set tl size index)
     (define height (treelist-height tl))
     (define new-node
       (let set ([node (treelist-root tl)]
                 [index index]
                 [el el]
                 [height height])
         (cond
           [(fx= height 0)
            (node-child-set node (radix index height) el)]
           [(node-leftwise-dense? node)
            (define branch-index (radix index height))
            (node*-set node branch-index (set (node*-ref node branch-index) index el (fx- height 1)))]
           [else
            (define-values (branch-index subindex) (step node index height))
            (Node (node-child-set node branch-index (set (node-ref node branch-index) subindex el (fx- height 1)))
                  (node-sizes node))])))
     (treelist new-node size height)]))

;; add `el` to end of vector
(define (treelist-add tl el)
  (cond
    [(impersonator? tl)
     (treelist-add/slow tl el)]
    [else
     (check-treelist 'treelist-add tl)
     (define size (treelist-size tl))
     (cond
       [(fx= size 0)
        (treelist (leaf el) 1 0)]
       [else
        (define root (treelist-root tl))
        (define height (treelist-height tl))
        (define new-root (build root height el))
        (if new-root
            ;; enough space in original tree
            (treelist new-root (fx+ size 1) height)
            ;; not enough space in original tree
            (treelist (Node (vector root
                                    (new-branch el height)))
                      (fx+ size 1)
                      (fx+ height 1)))])]))

(define (unsafe-root-add root size height el)
  ;; similar to `treelist-add`, used for `for/treelist`
  (define new-root (build root height el))
  (if new-root
      (values new-root (fx+ size 1) height)
      (values (Node (vector root
                            (new-branch el height)))
              (fx+ size 1)
              (fx+ height 1))))

(define (treelist->vector tl)
  (check-treelist 'treelist->vector tl)
  (for/vector #:length (treelist-size tl) ([el (in-treelist tl)])
    el))

(define (vector->treelist vec-in)
  ;; build a dense tree of vectors directly
  (define vec (if (impersonator? vec-in)
                  (and (vector? vec-in)
                       (vector-copy vec-in))
                  (and (vector? vec-in) ; amounts to `vector*?`
                       vec-in)))
  (unless vec (raise-argument-error* 'vector->treelist 'racket/primitive "vector?" vec-in))
  (define size (vector*-length vec))
  (cond
    [(fx= size 0) empty-treelist]
    [else
     (define-values (root height)
       (cond
         [(size . fx<= . MAX_WIDTH) (values (Node vec) 0)]
         [else
          (define height (fx- (fxquotient (fx+ (integer-length (fx- size 1)) BITS -1) BITS) 1))
          (values
           (let loop ([start 0] [height height])
             (cond
               [(fx= height 0)
                (Node (vector*-copy vec start (fxmin size (fx+ start MAX_WIDTH))))]
               [else
                (define width (fxlshift MAX_WIDTH (fx* height BITS)))
                (define end (fxmin size (fx+ width start)))
                (define step (fxrshift width BITS))
                (define len (fxquotient (fx+ (fx- end start) (fx- step 1)) step))
                (Node (for/vector #:length len ([start (in-range start end step)])
                                  (loop start (fx- height 1))))]))
           height)]))
     (treelist root size height)]))

(define (make-treelist size element)

#| This function follows the same recursive structure as
`vector->treelist`, but instead of using `vector*-copy` to copy
different elements into different vectors inside the treelist, it
tries to reuse as many vectors as possible, creating vectors
whose sizes sum up to O(log(size)). For example, if size is
MAX_WIDTH*2, then this code will end up creating this vector:
(let ([v (make-vector MAX_WIDTH element)]) (vector v v))

The strategy this code uses is to keep track of the first element in
any vector it builds as it works its way across and, if any other
elements of a vector have the same size as that first (inner) vector
then they must also have the same elements, so we can just reuse the
same vector. This happens in the loop whose result is bound to `root`,
below. For example, when `size` is MAX_WIDTH*2 we'll get `first-one`
bound to the vector in the first slot and then we'll reuse it in the
second iteration of the loop.

This strategy only most of the sharing, but not all of the it. To
mitigate this somewhat we ensure that all leaves are shared (when they
have size MAX_WIDTH), thanks to `max-width-leaf`.

Thus, since all nodes candidate at height 1 will be shared (thanks to
`max-width-leaf`), the smallest node that won't be shared has size
MAX_WIDTH*MAX_WIDTH and thanks to the structure of loop that
implements the sharing, we would need those nodes to have two
different parents, meaning we need at least MAX_WIDTH^3+MAX_WIDTH^2
nodes to miss some sharing. In that case, the sharing we miss is one
vector of size MAX_WITH. When MAX_WIDTH=32, the smallest tree that
misses any sharing has 33,792 nodes and an extra 1/1055th of the
minimum required storage. |#

  (unless (exact-nonnegative-integer? size)
    (raise-argument-error* 'make-treelist 'racket/primitive "natural?" size))
  (unless (fixnum? size)
    (raise (make-exn:fail:out-of-memory
            (format "out of memory making treelist\n  size: ~s"
                    size)
            (current-continuation-marks))))
  (cond
    [(fx= size 0) empty-treelist]
    [(size . fx<= . MAX_WIDTH) (treelist (Node (make-vector size element)) size 0)]
    [else
     (define max-width-leaf (make-vector MAX_WIDTH element))
     (define height (fx- (fxquotient (fx+ (integer-length (fx- size 1)) BITS -1) BITS) 1))
     (define available-to-reuse (make-hash))
     (define root
       (let loop ([first-one-at-this-level #f]
                  [first-one-size #f]
                  [start 0]
                  [end (fxmin size (fxlshift MAX_WIDTH (fx* height BITS)))]
                  [height height])
         (cond
           [(and first-one-at-this-level
                 (fx= (fx- end start) first-one-size))
            first-one-at-this-level]
           [(fx= height 0)
            (define size (fx- end start))
            (if (fx= size MAX_WIDTH)
                max-width-leaf
                (Node (make-vector size element)))]
           [else
            (define width (fxlshift MAX_WIDTH (fx* height BITS)))
            (define step (fxrshift width BITS))
            (define len (fxquotient (fx+ (fx- end start) (fx- step 1)) step))
            (define first-one (loop #f #f start (fxmin end (fx+ start step)) (fx- height 1)))
            (hash-set! available-to-reuse height #t)
            (Node (for/vector #:length len ([inner-start (in-range start end step)])
                    (if (fx= inner-start start)
                        first-one
                        (loop first-one
                              step
                              inner-start
                              (fxmin end (fx+ inner-start step))
                              (fx- height 1)))))])))
     (treelist root size height)]))

(define (treelist->list tl)
  (check-treelist 'treelist->list tl)
  (for/list ([el (in-treelist tl)])
    el))

(define (list->treelist lst)
  (unless (list? lst) (raise-argument-error* 'list->treelist 'racket/primitive "list?" lst))
  (if (null? lst)
      empty-treelist
      (vector->treelist (list->vector lst))))

(define (treelist-length tl)
  (check-treelist 'treelist-length tl)
  (treelist-size tl))

;; trees that are a result of this method may not meet invariants, but rebalancing is costly
;; and future concatenations would restore the invariants due to rebalancing being done on concats.
;; TODO write some tests showing this
(define (treelist-take tl pos)
  (cond
    [(impersonator? tl)
     (treelist-take/slow tl pos)]
    [else
     (check-treelist 'treelist-take tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-take tl size pos)
     (cond
       [(fx= pos 0)
        empty-treelist]
       [(fx= pos size)
        tl]
       [else
        (define height (treelist-height tl))
        (define new-root
          (let take ([node (treelist-root tl)]
                     [index (fx- pos 1)]
                     [height height])
            (cond
              [(fx= height 0)
               (Node (vector*-take (node-children node) (fx+ (radix index 0) 1)))]
              [(node-leftwise-dense? node)
               (define branch-index (radix index height))
               (define children (node*-children node))
               (define new-child (take (vector*-ref children branch-index) index (fx- height 1)))
               (define new-children (vector*-take children (fx+ branch-index 1)))
               (vector*-set! new-children branch-index new-child)
               (Node new-children)]
              [else
               (define-values (branch-index subindex) (step node index height))
               (define children (node-children node))
               (define new-child (take (vector*-ref children branch-index) subindex (fx- height 1)))
               (define new-children (vector*-take children (fx+ branch-index 1)))
               (vector*-set! new-children branch-index new-child)
               (cond
                 [(fx= 1 (vector*-length new-children))
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  ;; it's possible that we drop off a non-dense part and end up leftwise dense, but we don't try to check
                  (define new-sizes (vector*-take (node-sizes node) (fx+ branch-index 1)))
                  (vector*-set! new-sizes branch-index (fx+ index 1))
                  (Node new-children new-sizes)])])))
        (squash new-root pos height)])]))

(define (treelist-drop tl pos)
  (cond
    [(impersonator? tl)
     (treelist-drop/slow tl pos)]
    [else
     (check-treelist 'treelist-drop tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-drop tl size pos)
     (cond
       [(fx= pos 0)
        tl]
       [(fx= pos size)
        empty-treelist]
       [else
        (define height (treelist-height tl))
        (define new-root
          (let drop ([node (treelist-root tl)]
                     [index pos]
                     [height height])
            (cond
              [(fx= height 0)
               (Node (vector*-drop (node-children node) (radix index 0)))]
              [(node-leftwise-dense? node)
               (define branch-index (radix index height))
               (define children (node*-children node))
               (define new-child (drop (vector*-ref children branch-index) index (fx- height 1)))
               (define new-children (vector*-drop children branch-index))
               (vector*-set! new-children 0 new-child)

               (define new-len (fx- (node-size node) branch-index))
               (cond
                 [(fx= new-len 1)
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  (define size0 (size-subtree new-child (fx- height 1)))
                  (define step (fxlshift 1 (fx* height BITS)))
                  (cond
                    [(fx= size0 step)
                     ;; we dropped complete subtress to stay leftwise dense
                     (Node new-children)]
                    [else
                     (define new-sizes (make-vector new-len size0))
                     (for ([i (in-range 0 (fx- new-len 1))])
                       (vector*-set! new-sizes i (fx+ size0 (fx* i step))))
                     (define sizeN (size-subtree (vector*-ref new-children (fx- new-len 1)) (fx- height 1)))
                     (vector*-set! new-sizes (fx- new-len 1) (fx+ size0 (fx* (fx- new-len 2) step) sizeN))
                     (Node new-children new-sizes)])])]
              [else
               (define-values (branch-index subindex) (step node index height))
               (define children (node-children node))
               (define new-child (drop (vector*-ref children branch-index) subindex (fx- height 1)))
               (define new-children (vector*-drop children branch-index))
               (vector*-set! new-children 0 new-child)
               (define old-len (vector*-length (node-sizes node)))
               (define new-len (fx- old-len branch-index))
               (cond
                 [(fx= new-len 1)
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  ;; it's possible that the result is leftwise dense, but we don't try to check
                  (define new-sizes (for/vector #:length new-len
                                                ([i (in-range branch-index old-len)])
                                                (fx- (vector*-ref (node-sizes node) i) index)))
                  (Node new-children new-sizes)])])))
        (squash new-root (fx- size pos) height)])]))

(define (treelist-take-right tl pos)
  (check-treelist 'treelist-take-right tl)
  (define size (treelist-size tl))
  (check-treelist-end-index 'treelist-take-right tl size pos)
  (treelist-drop tl (fx- size pos)))

(define (treelist-drop-right tl pos)
  (check-treelist 'treelist-drop-right tl)
  (define size (treelist-size tl))
  (check-treelist-end-index 'treelist-drop-right tl size pos)
  (treelist-take tl (fx- size pos)))

(define (treelist-sublist tl pos end-pos)
  (check-treelist 'treelist-sublist tl)
  (define size (treelist-size tl))
  (check-treelist-bound-index 'treelist-sublist tl size pos 0 "starting ")
  (check-treelist-bound-index 'treelist-sublist tl size end-pos pos "ending ")
  (define len (fx- end-pos pos))
  (cond
    [(and (fx= len 0) (not (impersonator? tl)))
     empty-treelist]
    [(and (fx= len 1) (not (impersonator? tl)))
     (build-treelist (treelist-ref tl pos))]
    [else (treelist-take (treelist-drop tl pos) len)]))

(define (treelist-reverse tl)
  (cond
    [(impersonator? tl) (treelist-reverse/slow tl)]
    [else
     (check-treelist 'treelist-reverse tl)
     (define len (treelist-size tl))
     (define vec (make-vector len))
     (for ([el (in-treelist tl)]
           [i (in-range (sub1 len) -1 -1)])
       (vector-set! vec i el))
     (vector->treelist vec)]))

(define (treelist-rest tl)
  (cond
    [(impersonator? tl) (treelist-rest/slow tl)]
    [else
     (check-treelist 'treelist-rest tl)
     (cond
       [(eq? tl empty-treelist)
        (raise-arguments-error* 'treelist-rest 'racket/primitive "treelist is empty")]
       [else
        (treelist-drop tl 1)])]))

(define (treelist-split tl at)
  (check-treelist 'treelist-split tl)
  (check-treelist-end-index 'treelist-split tl (treelist-size tl) at)
  (cond
    [(fx= at 0) (values empty-treelist tl)]
    [(fx= at (treelist-size tl)) (values tl empty-treelist)]
    [else (values (treelist-take tl at) (treelist-drop tl at))]))

(define (treelist-insert tl at el)
  (cond
    [(impersonator? tl)
     (treelist-insert/slow tl at el)]
    [else
     (check-treelist 'treelist-insert tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-insert tl size at)
     (cond
       [(fx= at 0) (treelist-cons tl el)]
       [(fx= at size) (treelist-add tl el)]
       [else (treelist-append (treelist-add (treelist-take tl at) el)
                              (treelist-drop tl at))])]))

(define (treelist-delete tl at)
  (cond
    [(impersonator? tl)
     (treelist-delete/slow tl at)]
    [else
     (check-treelist 'treelist-delete tl)
     (define size (treelist-size tl))
     (check-treelist-index 'treelist-delete tl size at)
     (cond
       [(fx= at 0) (treelist-drop tl 1)]
       [(fx= at (fx- size 1))
        (if (fx= at 0)
            empty-treelist
            (treelist-take tl at))]
       [else (treelist-append (treelist-take tl at)
                              (treelist-drop tl (fx+ at 1)))])]))

(define (treelist-cons tl el)
  (cond
    [(impersonator? tl)
     (treelist-cons/slow tl el)]
    [else
     (check-treelist 'treelist-cons tl)
     (define size (treelist-size tl))
     (cond
       [(fx= 0 size)
        (treelist (leaf el) 1 0)]
       [else
        ;; insert in leftmost node, if it has space; this
        ;; will always work for small lists
        (define height (treelist-height tl))
        (define new-root
          (let insert-left ([a (treelist-root tl)]
                            [height height])
            (cond
              [(fx= height 0)
               (and ((node-size a) . < . MAX_WIDTH)
                    (Node (vector*-add-left el (node-children a))))]
              [else
               (define left (insert-left (vector*-ref (node-children a) 0) (fx- height 1)))
               (and left
                    (Node (vector*-set/copy (node-children a) 0 left)
                          (let ([sizes (node-sizes a)])
                            (and sizes
                                 (for/vector #:length (vector*-length sizes) ([n (in-vector sizes)])
                                             (fx+ n 1))))))])))
        (cond
          [new-root
           (treelist new-root (fx+ size 1) height)]
          [else
           (treelist-append (treelist (leaf el) 1 0) tl)])])]))

(define treelist-append
  (case-lambda
    [(tl rhs)
     (cond
       [(or (impersonator? tl)
            (impersonator? rhs))
        (treelist-append/slow tl rhs)]
       [else
        (check-treelist 'treelist-append tl)
        (check-treelist 'treelist-append rhs)
        (cond
          [(fx= 0 (treelist-size tl)) rhs]
          [(fx= 0 (treelist-size rhs)) tl]
          [else
           (define-values (new-children new-height)
             (concat-subtree (treelist-root tl)
                             (treelist-height tl)
                             (treelist-root rhs)
                             (treelist-height rhs)))
           (treelist new-children
                     (fx+ (treelist-size tl)
                          (treelist-size rhs))
                     new-height)])])]
    [() empty-treelist]
    [(tl)
     (check-treelist 'treelist-append tl)
     tl]
    [(tl . rhss)
     (for/fold ([tl tl]) ([rhs (in-list rhss)])
       (treelist-append tl rhs))]))

;; after take or drop, squash tree if it can be shorter:
(define (squash node new-size new-height)
  (cond
    [(and (fx= (node-size node) 1)
          (fx> new-height 0))
     (squash (node-first node) new-size (fx- new-height 1))]
    [else
     (treelist node new-size new-height)]))

;; result height is either max of two heights or one more
;; than the max of the heights
(define (concat-subtree left
                        height-l
                        right
                        height-r)
  ;; only trees of the same height can be concatenated
  (cond
    [(fx> height-l height-r)
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       right
                       height-r))
     (rebalance left
                mid
                #f
                height-l
                height-m)]
    [(fx< height-l height-r)
     (define-values (mid height-m)
       (concat-subtree left
                       height-l
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance #f
                mid
                right
                height-r
                height-m)]
    [(fx= height-l 0)
     (cond
       [(fx<= (fx+ (node-size left) (node-size right)) MAX_WIDTH)
        (values (Node (vector*-append (node-children left) (node-children right)))
                0)]
       [else
        (values (Node (vector (node-children left) (node-children right))
                      (vector (node-size left) (fx+ (node-size left) (node-size right))))
                1)])]
    [else
     ;; two internal nodes with same height
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance left
                mid
                right
                height-l
                height-m)]))

;; keeps all but last of `left`, all but first of `right`,
;; and all of `center`; height is the same for `left` and
;; `right`, which `center` height might be one less; height
;; is at least 1; the resulting height grows by either 0 or 1
(define (rebalance left
                   center
                   right
                   height
                   height_c)
  (define all-slots (merge-nodes left
                                 (if (fx< height_c height)
                                     (Node (vector center))
                                     center)
                                 right))
  (define plan (concat-plan all-slots))
  (define rebalanced-slots (exec-concat-plan all-slots plan height))

  (cond
    [(fx<= (vector*-length rebalanced-slots) MAX_WIDTH)
     (values (set-sizes rebalanced-slots height)
             height)]
    [else
     (define new-left (vector*-take rebalanced-slots MAX_WIDTH))
     (define new-right (vector*-drop rebalanced-slots MAX_WIDTH))
     (values (set-sizes (vector (set-sizes new-left height)
                                (set-sizes new-right height))
                        (fx+ height 1))
             (fx+ height 1))]))

;; merge all children except for the rightmost in `left` and leftmost in `right`
(define (merge-nodes left center right)
  (vector*-append (if (not left) (vector) (vector*-drop-right (node-children left) 1))
                  (node-children center)
                  (if (not right) (vector) (vector*-drop (node-children right) 1))))

;; TODO how to avoid setting sizes when the tree is leftwise dense?
(define (set-sizes children height)
  (cond
    [(fx= height 0)
     (Node children)]
    [else
     (define sizes (make-vector (vector*-length children)))
     (define mask (fx- (fxlshift 1 (fx* height BITS)) 1))
     (define-values (sum leftwise-dense?)
       (for/fold ([sum 0] [leftwise-dense? #t]) ([i (in-range 0 (vector*-length children))])
         (define new-sum (fx+ sum (size-subtree (vector*-ref children i) (fx- height 1))))
         (vector*-set! sizes i new-sum)
         (values new-sum (and leftwise-dense? (fx= 0 (fxand sum mask))))))
     (if leftwise-dense?
         (Node children)
         (Node children sizes))]))

;; TODO redesign this to be less imperative?
;; receives a node that is temporarily allowed to have > max_width children, redistributes it to conform to invariant
(define (concat-plan slots)
  (define plan (make-vector (vector*-length slots)))
  (define child-count
    (for/fold ([count 0]) ([i (in-range 0 (vector*-length slots))])
      (define sz (node-size (vector*-ref slots i)))
      (vector*-set! plan i sz)
      (fx+ count sz)))

  (define optimal-node-len (fxquotient (fx+ child-count MAX_WIDTH -1) MAX_WIDTH))
  (define target-node-len (fx+ optimal-node-len MAX_ERROR))
  
  (if (fx>= target-node-len (vector*-length plan))
      #false
      (distribute plan target-node-len (vector*-length plan))))

(define (distribute plan target count [node-idx 0])
  (cond
    [(fx>= target count)
     (vector*-take plan count)]
    [else
     (define init-i (short-node plan node-idx))
     (define-values (i r)
       (let loop ([i init-i]
                  [r (vector*-ref plan init-i)])
         (cond
           [(fx= r 0)
            (values i r)]
           [else
            (define min-size (min (fx+ r (vector*-ref plan (fx+ i 1))) MAX_WIDTH))
            (vector*-set! plan i min-size)
            (loop (fx+ i 1) (fx- (fx+ r (vector*-ref plan (fx+ i 1))) min-size))])))

     ;; we've removed a node (conceptually) at this point,
     ;; so move nodes to the right of current node left by one
     (for ([j (in-range i (fx- count 1))])
       (vector*-set! plan j (vector*-ref plan (fx+ j 1))))

     (distribute plan target (fx- count 1) (fx- i 1))]))

(define (short-node plan i)
  (if (fx< (vector*-ref plan i) (fx- MAX_WIDTH 1))
      i
      (short-node plan (fx+ i 1))))

(define (exec-concat-plan slots plan height)
  (cond
    [(not plan) slots]
    [else
     (define flattened-size
       (for/fold ([sum 0]) ([node (in-vector slots)])
         (fx+ sum (node-size node))))
     (define flattened
       (for*/vector #:length flattened-size ([node (in-vector slots)]
                                             [child (in-vector (node-children node))])
         child))

     (define new-slots (make-vector (vector*-length plan)))
     (for/fold ([sum 0]) ([i (in-range 0 (vector*-length plan))])
       (define new-sum (fx+ sum (vector*-ref plan i)))
       (define new-node
         (for/vector #:length (fx- new-sum sum)
                     ([j (in-range sum new-sum)])
                     (vector*-ref flattened j)))
       (vector*-set! new-slots i (set-sizes new-node (fx- height 1)))
       new-sum)
     
     new-slots]))

(define (size-subtree node height)
  (cond
    [(fx= height 0)
     (vector*-length (node-children node))]
    [(node-sizes node)
     => (lambda (sizes)
          (vector*-ref sizes (fx- (vector*-length sizes) 1)))]
    [else
     ;; if sizes is #false, then we know we have a leftwise dense subtree
     (fx+ (fxlshift (fx- (node*-size node) 1) (fx* height BITS))
          (size-subtree (node*-last node) (fx- height 1)))]))

;; helper functions

;; calculate next branch to take and subindex of `index` along that path;
;; the returned subindex is always in range for the subtree (i.e., no bits
;; set at `height` radix or above)
(define (step node index height)
  (define sizes (node-sizes node))
  (define target-index (bitwise-and index (fx- (fxlshift 1 (fx* (fx+ height 1) BITS)) 1)))
  (define branch (let loop ([i 0])
                   (if (fx<= (vector*-ref sizes i) target-index)
                       (loop (fx+ i 1))
                       i)))
  (values branch
          (if (fx= branch 0)
              target-index
              (fx- target-index (vector*-ref sizes (fx- branch 1))))))

;; add if there's room, return #false otherwise
(define (build n height el)
  (cond
    [(fx= height 0)
     (if (fx< (node-size n) MAX_WIDTH)
         (Node (vector*-add-right (node-children n) el))
         #false)]
    [else
     (define size (node-size n))
     (define child (and (fx> size 0)
                        (build (node-ref n (fx- size 1)) (fx- height 1) el)))
     (cond
       [child
        (Node (vector*-set/copy (node-children n) (fx- size 1) child)
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-set/copy sizes
                                       (fx- (vector*-length sizes) 1)
                                       (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [(fx< (node-size n) MAX_WIDTH)
        (Node (vector*-add-right (node-children n)
                           (new-branch el (fx- height 1)))
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-add-right sizes
                                        (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [else
        #false])]))

;; create a branch of height `height` terminating in a unary leaf node containing `el`
(define (new-branch el height)
  (if (fx= height 0)
      (leaf el)
      (Node (vector (new-branch el (fx- height 1))))))

(define (treelist-map tl proc)
  (check-treelist 'treelist-map tl)
  (unless (and (procedure? proc) (procedure-arity-includes? proc 1))
    (raise-argument-error* 'treelist-map 'racket/primitive "(procedure-arity-includes/c 1)" proc))
  (for/treelist ([v (in-treelist tl)])
    (proc v)))

(define (treelist-for-each tl proc)
  (check-treelist 'treelist-for-each tl)
  (unless (and (procedure? proc) (procedure-arity-includes? proc 1))
    (raise-argument-error* 'treelist-for-each 'racket/primitive "(procedure-arity-includes/c 1)" proc))
  (for ([v (in-treelist tl)])
    (proc v)))

(define (treelist-filter keep tl)
  (unless (and (procedure? keep) (procedure-arity-includes? keep 1))
    (raise-argument-error* 'treelist-filter 'racket/primitive "(procedure-arity-includes/c 1)" keep))
  (check-treelist 'treelist-filter tl)
  (for/treelist ([el (in-treelist tl)]
                 #:when (keep el))
    el))

(define (treelist-member? tl v [eql? equal?])
  (check-treelist 'treelist-member? tl)
  (unless (and (procedure? eql?) (procedure-arity-includes? eql? 2))
    (raise-argument-error* 'treelist-member? 'racket/primitive "(procedure-arity-includes/c 2)" eql?))
  (for/fold ([r #f]) ([el (in-treelist tl)]
                      #:when (eql? v el)
                      #:final #t)
    #t))

(define (treelist-find tl match?)
  (check-treelist 'treelist-find tl)
  (unless (and (procedure? match?) (procedure-arity-includes? match? 1))
    (raise-argument-error* 'treelist-find 'racket/primitive "(procedure-arity-includes/c 1)" match?))
  (for/fold ([r #f]) ([el (in-treelist tl)]
                      #:when (match? el)
                      #:final #t)
    el))

(define (treelist-index-of tl v [eql? equal?])
  (check-treelist 'treelist-index-of tl)
  (unless (and (procedure? eql?) (procedure-arity-includes? eql? 2))
    (raise-argument-error* 'treelist-index-of 'racket/primitive "(procedure-arity-includes/c 2)" eql?))
  (for/first ([el (in-treelist tl)]
              [i (in-naturals)]
              #:when (eql? v el))
    i))

;; input does not have to be a treelist: if so make a singleton
(define (treelist-flatten v)
  (cond
    [(treelist? v) (treelist-append* (treelist-map v treelist-flatten))]
    [else (build-treelist v)]))

;; append*, concat, treelist of treelists into a single treelist
(define (treelist-append* tlotl)
  (check-treelist 'treelist-append* tlotl)
  (for/fold ([acc empty-treelist]) ([tl (in-treelist tlotl)])
    (treelist-append acc tl)))

(define (check-sort-arguments who less-than? get-key)
  (unless (and (procedure? less-than?) (procedure-arity-includes? less-than? 2))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 2)" less-than?))
  (unless (or (not get-key)
              (and (procedure? get-key) (procedure-arity-includes? get-key 1)))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 1)" get-key)))

(define (treelist-sort tl less-than?
                       #:key [get-key #f]
                       #:cache-keys? [cache-keys? #f])
  (check-treelist 'treelist-sort tl)
  (check-sort-arguments 'treelist-sort less-than? get-key)
  (define vec (treelist->vector tl))
  (vector-sort! vec less-than? 0 (vector*-length vec) get-key cache-keys?)
  (vector->treelist vec))

(define-values (prop:treelist-chaperone
                treelist-chaperone?
                treelist-chaperone-ref)
  (make-impersonator-property 'treelist))

(struct treelist-wrapper (chaperone? prev props state procs)
  #:authentic
  #:sealed)

(struct procs (ref set insert prepend append append2 delete take drop state-key)
  #:authentic
  #:sealed)

(define (check-chaperone-arguments who
                                   ref-proc
                                   set-proc
                                   insert-proc
                                   prepend-proc
                                   append-proc
                                   append2-proc
                                   delete-proc
                                   take-proc
                                   drop-proc
                                   props)
  (unless (and (procedure? ref-proc)
               (procedure-arity-includes? ref-proc 4))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 4)" ref-proc)) 
  (unless (and (procedure? set-proc)
               (procedure-arity-includes? set-proc 4))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 4)" set-proc))
  (unless (and (procedure? insert-proc)
               (procedure-arity-includes? insert-proc 4))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 4)" insert-proc))
  (unless (and (procedure? prepend-proc)
               (procedure-arity-includes? prepend-proc 3))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 3)" prepend-proc))
  (unless (and (procedure? append-proc)
               (procedure-arity-includes? append-proc 3))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 3)" append-proc))
  (unless (or (not append2-proc)
              (and (procedure? append2-proc)
                   (procedure-arity-includes? append2-proc 4)))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 4)" append2-proc))
  (unless (and (procedure? delete-proc)
               (procedure-arity-includes? delete-proc 3))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 3)" delete-proc))
  (unless (and (procedure? take-proc)
               (procedure-arity-includes? take-proc 3))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 3)" take-proc))
  (unless (and (procedure? drop-proc)
               (procedure-arity-includes? drop-proc 3))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 3)" drop-proc))
  (check-chaperone-properties who props))

(define (check-chaperone-properties who props)
  (let loop ([ps props])
    (unless (null? ps)
      (define prop (car ps))
      (unless (impersonator-property? prop)
        (raise-argument-error* who 'racket/primitive "impersonator-property?" prop))
      (when (null? (cdr ps))
        (raise-arguments-error* who 'racket/primitive "missing value after impersonator-property argument"
                                "impersonator property" prop))
      (loop (cddr ps)))))

(define (chaperone-treelist tl
                            #:state state
                            #:state-key [state-key (list 'fresh)]
                            #:ref ref-proc
                            #:set set-proc
                            #:insert insert-proc
                            #:prepend prepend-proc
                            #:append append-proc
                            #:append2 [append2-proc #f]
                            #:delete delete-proc
                            #:take take-proc
                            #:drop drop-proc
                            . props)
  (check-treelist 'chaperone-treelist tl)
  (check-chaperone-arguments 'chaperone-treelist
                             ref-proc
                             set-proc
                             insert-proc
                             prepend-proc
                             append-proc
                             append2-proc
                             delete-proc
                             take-proc
                             drop-proc
                             props)
  (apply chaperone-struct tl
         struct:treelist
         prop:treelist-chaperone
         (treelist-wrapper #t
                           tl
                           props
                           state
                           (procs
                            ref-proc
                            set-proc
                            insert-proc
                            prepend-proc
                            append-proc
                            append2-proc
                            delete-proc
                            take-proc
                            drop-proc
                            state-key))
         props))

(define (impersonate-treelist tl
                              #:state state
                              #:state-key state-key
                              #:ref ref-proc
                              #:set set-proc
                              #:insert insert-proc
                              #:prepend prepend-proc
                              #:append append-proc
                              #:append2 append2-proc
                              #:delete delete-proc
                              #:take take-proc
                              #:drop drop-proc
                              . props)
  (apply impersonate-struct tl
         struct:treelist
         prop:treelist-chaperone
         (treelist-wrapper #f
                           tl
                           props
                           state
                           (procs
                            ref-proc
                            set-proc
                            insert-proc
                            prepend-proc
                            append-proc
                            append2-proc
                            delete-proc
                            take-proc
                            drop-proc
                            state-key))
         props))

(define (unimpersonate-treelist tl)
  (define w (treelist-chaperone-ref tl))
  (treelist-wrapper-prev w))

(define (treelist-chaperone-state tl key [fail-k (make-state-key-fail tl key)])
  (define who 'treelist-chaperone-state)
  (check-treelist who tl)
  (unless (and (procedure? fail-k) (procedure-arity-includes? fail-k 0))
    (raise-argument-error* who 'racket/primitive "(procedure-arity-includes/c 0)" fail-k))
  (let loop ([tl tl])
    (define w (and tl (treelist-chaperone-ref tl #f)))
    (cond
      [(not w)
       (fail-k)]
      [(eq? key (procs-state-key (treelist-wrapper-procs w)))
       (treelist-wrapper-state w)]
      [else
       (loop (treelist-wrapper-prev w))])))

;; removes a chaperone layer whose state is keyed by `key`
(define (extract-keyed-treelist tl key)
  (let loop ([tl tl])
    (define w (and tl (treelist-chaperone-ref tl #f)))
    (cond
      [(not w) (values #f #f)]
      [(eq? key (procs-state-key (treelist-wrapper-procs w)))
       (values (treelist-wrapper-prev w) (treelist-wrapper-state w))]
      [else
       (define-values (new-prev state) (loop (treelist-wrapper-prev w)))
       (if new-prev
           (values (re-chaperone new-prev w (treelist-wrapper-state w))
                   state)
           (values #f #f))])))

(define (make-state-key-fail tl key)
  (lambda ()
    (raise-arguments-error* 'treelist-chaperone-state 'racket/primitive
                            "no state found for the given key"
                            "treelist" tl
                            "key" key)))

(define (check-chaperone who check? v old-v)
  (when check?
    (unless (chaperone-of? v old-v)
      (error who "result is not a chaperone")))
  v)

(define (re-chaperone new-prev w-in new-state)
  (define w (struct-copy treelist-wrapper w-in
                         [prev new-prev]
                         [state new-state]))
  (if (treelist-wrapper-chaperone? w)
      (apply chaperone-struct
             new-prev
             struct:treelist
             prop:treelist-chaperone
             w
             (treelist-wrapper-props w))
      (apply impersonate-struct
             new-prev
             struct:treelist
             prop:treelist-chaperone
             w
             (treelist-wrapper-props w))))

(define (treelist-ref/slow tl index)
  (define who 'treelist-ref)
  (check-treelist who tl)
  (check-treelist-index who tl (treelist-size tl) index)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define v (treelist-ref prev index))
  (define ref (procs-ref (treelist-wrapper-procs w)))
  (check-chaperone who
                   (treelist-wrapper-chaperone? w)
                   (ref prev index v (treelist-wrapper-state w))
                   v))

(define (treelist-first/slow tl)
  (define who 'treelist-first)
  (check-treelist who tl)
  (cond
    [(treelist-empty? tl)
     (raise-arguments-error* who 'racket/primitive "treelist is empty")]
    [else
     (treelist-ref/slow tl 0)]))

(define (treelist-last/slow tl)
  (define who 'treelist-last)
  (check-treelist who tl)
  (cond
    [(treelist-empty? tl)
     (raise-arguments-error* who 'racket/primitive "treelist is empty")]
    [else
     (treelist-ref/slow tl (fx- (treelist-size tl) 1))]))

(define (treelist-reverse/slow tl)
  (define who 'treelist-reverse)
  (check-treelist who tl)
  (define len (treelist-size tl))
  (for/fold ([new-tl (treelist-take/slow tl 0)]) ([i (in-range len)])
    (treelist-add/slow new-tl (treelist-ref/slow tl (fx- len i 1)))))

(define (treelist-rest/slow tl)
  (define who 'treelist-rest)
  (check-treelist who tl)
  (cond
    [(treelist-empty? tl)
     (raise-arguments-error* who 'racket/primitive "treelist is empty")]
    [else
     (treelist-drop/slow tl 1)]))

(define (treelist-node-for/slow tl index)
  (values (Node (vector (treelist-ref tl index)))
          0))

(define (treelist-equal?/slow tl other-tl recur)
  (define len (treelist-size tl))
  (and (fx= len (treelist-size other-tl))
       (for/and ([i (in-range len)])
         (recur (treelist-ref tl i) (treelist-ref other-tl i)))))

(define (call-with-chaperone-values who tl w el producer receiver)
  (define chaperone? (treelist-wrapper-chaperone? w))
  (call-with-values
   producer
   (case-lambda
     [(new-el new-state)
      (check-chaperone who chaperone? new-el el)
      (receiver new-el new-state)]
     [args
      (raise
       (|#%app|
        exn:fail:contract:arity
        (error-message->adjusted-string
         who 'racket/primitive	 
         (string-append "arity mismatch;\n"
                        " received wrong number of values from a chaperone's replacement procedure\n"
                        "  expected: 2\n"
                        "  received: " (number->string (length args)) "\n"
                        "  chaperone: " (format "~e" tl))
         'racket/primitive)))])))

(define (treelist-set/slow tl index el)
  (define who 'treelist-set)
  (check-treelist who tl)
  (check-treelist-index who tl (treelist-size tl) index)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (call-with-chaperone-values
   who tl w
   el
   (lambda ()
     ((procs-set (treelist-wrapper-procs w)) prev index el (treelist-wrapper-state w)))
   (lambda (new-el new-state)
     (define new-prev (treelist-set prev index new-el))
     (re-chaperone new-prev w new-state))))

(define (treelist-insert/slow tl at el)
  (define who 'treelist-insert)
  (check-treelist who tl)
  (define size (treelist-size tl))
  (check-treelist-end-index who tl size at)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (call-with-chaperone-values
   who tl w
   el
   (lambda ()
     ((procs-insert (treelist-wrapper-procs w)) prev at el (treelist-wrapper-state w)))
   (lambda (new-el new-state)
     (define new-prev (treelist-insert prev at new-el))
     (re-chaperone new-prev w new-state))))

(define (treelist-add/slow tl el)
  (check-treelist 'treelist-add tl)
  (treelist-insert/slow tl (treelist-size tl) el))

(define (treelist-cons/slow tl el)
  (check-treelist 'treelist-cons tl)
  (treelist-insert/slow tl 0 el))

(define (treelist-do-slow who op procs-op tl at)
  (check-treelist who tl)
  (define size (treelist-size tl))
  (check-treelist-end-index who tl size at)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define op-tl (op prev at))
  (define new-state
    ((procs-op (treelist-wrapper-procs w)) prev at (treelist-wrapper-state w)))
  (re-chaperone op-tl w new-state))

(define (treelist-delete/slow tl at)
  (treelist-do-slow 'treelist-delete treelist-delete procs-delete tl at))

(define (treelist-take/slow tl pos)
  (treelist-do-slow 'treelist-take treelist-take procs-take tl pos))

(define (treelist-drop/slow tl pos)
  (treelist-do-slow 'treelist-drop treelist-drop procs-drop tl pos))

(define (treelist-append/slow tl rhs)
  (define who 'treelist-append)
  (cond
    [(impersonator? tl)
     (check-treelist who tl)
     (check-treelist who rhs)
     (define w (treelist-chaperone-ref tl))
     (define prev (treelist-wrapper-prev w))
     (define-values (rhs2 state2) (if (procs-append2 (treelist-wrapper-procs w))
                                      (extract-keyed-treelist rhs (procs-state-key
                                                                   (treelist-wrapper-procs w)))
                                      (values #f #f)))
     (cond
       [rhs2
        (call-with-chaperone-values
         who tl w
         rhs2
         (lambda ()
           ((procs-append2 (treelist-wrapper-procs w)) prev rhs2 (treelist-wrapper-state w) state2))
         (lambda (new-rhs new-state)
           (define new-tl (treelist-append prev new-rhs))
           (re-chaperone new-tl w new-state)))]
       [else
        (call-with-chaperone-values
         who tl w
         rhs
         (lambda ()
           ((procs-append (treelist-wrapper-procs w)) prev rhs (treelist-wrapper-state w)))
         (lambda (new-rhs new-state)
           (define new-tl (treelist-append prev new-rhs))
           (re-chaperone new-tl w new-state)))])]
    [else
     (check-treelist who tl)
     (check-treelist who rhs)
     ;; `rhs` must be an impersonator to get here; if it has `#f` for `ref-proc`,
     ;; we can strip it away to keep treelist-add` faster:     
     (define w (treelist-chaperone-ref rhs))
     (define prev (treelist-wrapper-prev w))
     (call-with-chaperone-values
      who rhs w
      tl
      (lambda ()
        ((procs-prepend (treelist-wrapper-procs w)) tl prev (treelist-wrapper-state w)))
      (lambda (new-tl new-state)
        (define new-rhs (treelist-append new-tl prev))
        (re-chaperone new-rhs w new-state)))]))

(define (treelist-copy-for-mutable tl)
  (cond
    [(eq? tl empty-treelist)
     tl]
    [(impersonator? tl)
     ;; copy the slow but general way
     (for/treelist ([i (in-treelist tl)])
       i)]
    [else
     (struct-copy treelist tl
                  [root(let copy-node ([n (treelist-root tl)] [height (treelist-height tl)])
                         (cond
                           [(fx= height 0) (vector*-copy n)]
                           [else
                            (for/vector #:length (vector-length n) ([e (in-vector n)])
                                        (copy-node e (fx- height 1)))]))])]))

(define (treelist-set! tl index el)
  (cond
    [(impersonator? tl)
     (error "cannot mutate a chaperoned treelist")]
    [else
     (define-values (node node-pos) (treelist-node-for tl index))
     (vector-set! node node-pos el)]))
