#lang racket/base
(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/math
         racket/match
         racket/promise
         racket/stream
         racket/vector

         data/gvector
         
         math/flonum
         (only-in math/number-theory
                  binomial
                  integer-root)

         "error.rkt")

(provide enum
         enum?
         size
         encode
         decode
         empty/e
         const/e
         from-list/e
         fin/e
         disj-sum/e
         disj-append/e
         cons/e
         elegant-cons/e
         dep/e
         dep2/e ;; requires size (eventually should replace dep/e with this)
         map/e
         filter/e ;; very bad, only use for small enums
         except/e 
         thunk/e
         fix/e
         many/e
         many1/e
         list/e
         vec/e

         cantor-vec/e
         cantor-list/e

         box-vec/e
         box-list/e
         
         traverse/e
         hash-traverse/e
         
         fail/e
         
         approximate
         to-list
         to-stream
         take/e
         fold-enum

         nat/e
         range/e
         slice/e
         nat+/e

         ;; Base type enumerators
         any/e
         var/e
         var-prefix/e
         num/e
         integer/e
         bool/e
         real/e
         string/e)

;; an enum a is a struct of < Nat or +Inf, Nat -> a, a -> Nat >
(struct enum
        (size from to)
        #:prefab)

;; size : enum a -> Nat or +Inf
(define (size e)
  (enum-size e))

;; decode : enum a, Nat -> a
(define/contract (decode e n)
  (-> enum? exact-nonnegative-integer? any/c)
  (if (and (< n (enum-size e))
           (>= n 0))
      ((enum-from e) n)
      (redex-error 'decode "Index into enumerator out of range. Tried to decode ~s in an enum of size ~s" n (size e))))

;; encode : enum a, a -> Nat
(define/contract (encode e a)
  (-> enum? any/c exact-nonnegative-integer?)
  ((enum-to e) a))

;; Helper functions
;; map/e : (a -> b), (b -> a), enum a -> enum b
(define (map/e f inv-f e . es)
  (cond [(empty? es)
         (enum (size e)
               (λ (x) 
                  (f (decode e x)))
               (λ (n)
                  (encode e (inv-f n))))]
        [else
         (define es/e (apply list/e (cons e es)))
         (map/e
          (λ (xs)
             (apply f xs))
          (λ (ys)
             (call-with-values (λ () (inv-f ys)) list))   
          es/e)]))

;; filter/e : enum a, (a -> bool) -> enum a
;; size won't be accurate!
;; encode is not accurate right now!
(define (filter/e e p)
  (enum (size e)
        (λ (n)
           (let loop ([i 0]
                      [seen 0])
             (let ([a (decode e i)])
               (if (p a)
                   (if (= seen n)
                       a
                       (loop (+ i 1) (+ seen 1)))
                   (loop (+ i 1) seen)))))
        (λ (x) (encode e x))))

;; except/e : (enum a) a* -> (enum a)
;; Everything inside e MUST be in the enumerator or you will get a redex-error
(define (except/e e . excepts)
  (define (except1/e x e)
    (cond [(= (size e) 0) e]
          [else
           (define xi (encode e x))
           (define (from-nat n)
             (cond [(< n xi) (decode e n)]
                   [else (decode e (add1 n))]))
           (define (to-nat y)
             (define yi (encode e y))
             (cond [(< yi xi) yi]
                   [(> yi xi) (sub1 yi)]
                   [else (redex-error 'encode "attempted to encode an excepted value")]))
           (enum (max 0 (sub1 (size e))) from-nat to-nat)]))
  (foldr except1/e
         e
         excepts))

(define (to-stream e)
  (define (loop n)
    (cond [(n . >= . (size e))
           empty-stream]
          [else
           (stream-cons (decode e n)
                        (loop (add1 n)))]))
  (loop 0))

(define (approximate e n)
  (for/list ([i (in-range n)])
    (decode e i)))

;; to-list : enum a -> listof a
;; better be finite
(define (to-list e)
  (when (infinite? (size e))
    (redex-error 'to-list "cannot encode an infinite list"))
  (map (enum-from e)
       (build-list (size e)
                   identity)))

;; take/e : enum a, Nat -> enum a
;; returns an enum of the first n parts of e
;; n must be less than or equal to size e
(define (take/e e n)
  (unless (<= n (size e))
    (redex-error 'take/e "there aren't ~s elements in ~s" n e))
  (enum n
        (λ (k)
           (decode e k))
        (λ (x)
           (let ([k (encode e x)])
             (unless (< k n)
               (redex-error 'take/e "attempted to encode an element not in an enumerator"))
             k))))

(define (slice/e e lo hi)
  (unless (and (lo . >= . 0)
               (hi . <= . (size e))
               (hi . >= . lo))
    (redex-error 'slice/e "bad range in slice/e: size: ~s, lo: ~s, hi: ~s" (size e) lo hi))
  (enum (hi . - . lo)
        (λ (n)
           (decode e (n . + . lo)))
        (λ (x)
           (define n (encode e x))
           (unless (and (n . >= . lo)
                        (n . <  . hi))
             (redex-error 'slice/e "attempted to encode an element removed by slice/e: ~s was excepted, originally ~s, but sliced between ~s and ~s" x n lo hi))
           (n . - . lo))))

;; below/e
(define (below/e n)
  (take/e nat/e n))

;; display-enum : enum a, Nat -> void
(define (display-enum e n)
  (for ([i (range n)])
    (display (decode e i))
    (newline) (newline)))

(define empty/e
  (enum 0
        (λ (n)
           (redex-error 'decode "absurd"))
        (λ (x)
           (redex-error 'encode "no elements in the enumerator"))))

(define (const/e c)
  (enum 1
        (λ (n)
           c)
        (λ (x)
           (if (equal? c x)
               0
               (redex-error 'encode "value not in enumerator")))))

;; from-list/e :: Listof a -> Gen a
;; input list should not contain duplicates
;; equality on eq?
(define (from-list/e l)
  (define rev-map
    (for/hash ([i (in-naturals)]
               [x (in-list l)])
      (values x i)))
  (if (empty? l)
      empty/e
      (enum (length l)
            (λ (n)
               (list-ref l n))
            (λ (x)
               (hash-ref rev-map x)))))

(define (fin/e . args) (from-list/e (remove-duplicates args)))

(define nat/e
  (enum +inf.0
        identity
        identity))


(define int/e
  (enum +inf.0
        (λ (n)
           (if (even? n)
               (* -1 (/ n 2))
               (/ (+ n 1) 2)))
        (λ (n)
           (if (> n 0)
               (- (* 2 n) 1)
               (* 2 (abs n))))))

(define (empty/e? e)
  (= 0 (size e)))

(define (exact-min . xs)
  (define (exact-min-2 x y)
    (if (x . <= . y)
        x
        y))
  (foldl exact-min-2 +inf.0 xs))

(struct fin-layer
  (bound  ;; nat
   enums) ;; Vectorof (Enum a, list-index)
  #:transparent)

(struct upper-bound
  (total-bound      ;; Nat
   individual-bound ;; Nat
   enumerators      ;; Vectorof (Enum a, Nat)
   )
  #:transparent)

(struct list-layer
  (max-index ;; Nat = layer-size + prev-layer-max-index: This is the maximum index into decode for this layer
   inexhausted-bound ;; Nat, = min (map size inexhausteds): This is the maximum index in the tuple for encode
   exhausteds   ;; Vectorof (Enum a, Nat)
   inexhausteds ;; Vectorof (Enum a, Nat)
   )
  #:transparent)

(define/contract (mk-fin-layers es)
  ((listof enum?) . -> . (listof fin-layer?))
  (define (loop eis prev)
    (define non-emptys (filter (negate (compose empty/e? car)) eis))
    (match non-emptys
      ['() '()]
      [_
       (define min-size
         (apply exact-min (map (compose size car) non-emptys)))
       (define (not-min-size? e)
         (not (= (size (car e)) min-size)))
       (define leftover
         (filter not-min-size? non-emptys))
       (define veis
         (apply vector non-emptys))
       (define cur-layer (fin-layer min-size veis))
       (define remaining-layers
         (loop leftover cur-layer))
       (cons cur-layer
             remaining-layers)]))
  (define eis
    (for/list [(i (in-naturals))
               (e (in-list es))]
      (cons e i)))
  (loop eis (fin-layer 0 eis)))

;; layers : Listof Enum -> Listof Upper-Bound
(define/contract (disj-sum-layers es)
  ((listof enum?) . -> . (vectorof upper-bound?))
  (define fin-layers (mk-fin-layers es))
  (define/contract (loop fin-layers prev)
    (-> (listof fin-layer?)
        upper-bound?
        (listof upper-bound?))
    (match fin-layers
      ['() '()]
      [(cons (fin-layer cur-bound eis) rest-fin-layers)
       (match-define (upper-bound prev-tb
                                  prev-ib
                                  _)
                     prev)
       (define min-size cur-bound)
       (define diff-min-size
         (min-size . - . prev-ib))
       (define total-bound
         (prev-tb . + . (diff-min-size . * . (vector-length eis))))
       (define cur-layer
         (upper-bound total-bound
                      cur-bound
                      eis))
       (define rest-layers (loop rest-fin-layers cur-layer))
       (cons cur-layer
             rest-layers)]))
  (define eis
    (for/list [(i (in-naturals))
               (e (in-list es))]
      (cons e i)))
  (apply vector (loop fin-layers (upper-bound 0 0 eis))))

(define (mk-list-layers es)
  (define eis
    (for/list [(i (in-naturals))
               (e (in-list es))]
      (cons e i)))
  (define/contract (loop fin-layers prev-layer)
    (-> (listof fin-layer?)
        list-layer?
        (listof list-layer?))
    (match fin-layers
      ['() '()]
      [(cons (fin-layer cur-bound vec-cur-inexhausteds) rest-fins)
       (match-define (list-layer prev-max-index
                                 prev-bound
                                 prev-exhausteds
                                 prev-inexhausteds)
                     prev-layer)
       (define cur-inexhausteds (vector->list vec-cur-inexhausteds))
       (define cur-exhausteds
         (append (remove* cur-inexhausteds prev-inexhausteds)
                 prev-exhausteds))
       (define num-inexhausted
         (length cur-inexhausteds))
       (define max-index
         (prev-max-index . + . (apply *
                                      ((expt cur-bound num-inexhausted) . - . (expt prev-bound num-inexhausted))
                                      (map (compose size car) cur-exhausteds))))
       (define cur-layer
         (list-layer max-index
                     cur-bound
                     cur-exhausteds
                     cur-inexhausteds))
       (define rest-layers (loop rest-fins cur-layer))
       (cons cur-layer rest-layers)]))
  (loop (mk-fin-layers es)
        (list-layer 0 0 '() eis)))

;; find-dec-layer : Nat, Nonempty-Listof Upper-bound -> Upper-bound, Upper-bound
;; Given an index, find the first layer 
(define (find-dec-layer i layers)
  (find-layer-by-size i
                      upper-bound-total-bound
                      (upper-bound 0 0 (vector))
                      layers))

(define (find-index x e-ps [cur-index 0])
  (match e-ps
    ['() (redex-error 'encode "invalid term")]
    [(cons (cons e in-e?)
           more-e-ps)
     (cond [(in-e? x)
            (values (encode e x)
                    cur-index)]
           [else
            (find-index x more-e-ps (add1 cur-index))])]))

(define (find-enc-layer i e-i layers)
  (define-values (prev cur)
    (find-layer-by-size i
                        upper-bound-individual-bound
                        (upper-bound 0 0 (vector))
                        layers))
  (define/match (find-e-index l e-i)
    [((upper-bound tb ib eis) e-i)
     (define (loop low hi)
       (when (> low hi)
         (redex-error 'encode "internal bin search bug"))
       (define mid
         (quotient (low . + . hi) 2))
       (define cur
         (cdr (vector-ref eis mid)))
       (cond [(low . = . mid)
              (unless (cur . = . e-i)
                (redex-error 'encode "internal binary search bug"))
              mid]
             [(cur . = . e-i) mid]
             [(cur . < . e-i) (loop (add1 mid) hi)]
             [else            (loop low mid)]))
     (loop 0 (vector-length eis))])
  (values prev
          cur
          (find-e-index cur e-i)))

(define/contract (find-layer-by-size i get-size zeroth ls)
  (-> (or/c infinite? exact-nonnegative-integer?)
      (any/c . -> . (or/c infinite? exact-nonnegative-integer?))
      any/c
      (vectorof any/c)
      (values any/c any/c))
  ;; Find the lowest indexed elt that is still greater than i
  (define (loop lo hi)
    (define mid (quotient (lo . + . hi) 2))
    (define cur (get-size (vector-ref ls mid)))
    (cond [(i  . = . cur) (add1 mid)]
          [(i  . > . cur) (loop (add1 mid) hi)]
          [(lo . = . mid) lo]
          [else           (loop lo         mid)]))
  (define n (loop 0 (vector-length ls)))
  (define prev
    (cond [(n . > . 0) (vector-ref ls (sub1 n))]
          [else        zeroth]))
  (values prev (vector-ref ls n)))

(define (find-list-dec-layer layers n eis)
  (define-values (prev cur)
    (find-layer-by-size n
                        list-layer-max-index
                        (list-layer 0 0 '() eis)
                        (list->vector layers)))
  (match-define (list-layer prev-max-index _ _ _) prev)
  (match-define (list-layer _  tuple-bound exhs inexhs) prev)
  (values prev-max-index tuple-bound exhs inexhs))

;; fairly interleave a list of enumerations
(define (disj-sum/e . e-ps)
  (define layers
    (disj-sum-layers (map car e-ps)))
  (define (empty-e-p? e-p)
    (= 0 (size (car e-p))))
  (match (filter (negate empty-e-p?) e-ps)
    ['() empty/e]
    [`(,e-p) (car e-p)]
    [_
     (define (dec i)
       (define-values (prev-up-bound cur-up-bound)
         (find-dec-layer i layers))
       (match-define (upper-bound so-far prev-ib _)  prev-up-bound)
       (match-define (upper-bound ctb    cib     es) cur-up-bound)
       (define this-i (i . - . so-far))
       (define len (vector-length es))
       (define-values (q r) (quotient/remainder this-i len))
       (define this-e (car (vector-ref es r)))
       (decode this-e (+ q prev-ib)))
     (define (enc x)
       (define-values (index which-e)
         (find-index x e-ps))
       (define-values (prev-up-bound cur-up-bound cur-e-index)
         (find-enc-layer index which-e layers))
       (match-define (upper-bound ptb pib pes) prev-up-bound)
       (match-define (upper-bound ctb cib ces) cur-up-bound)
       (+ ptb
          cur-e-index
          ((vector-length ces) . * . (index . - . pib))))
     (enum (apply + (map (compose size car) e-ps))
           dec
           enc)]))

;; Like disj-sum/e, but sequences the enumerations instead of interleaving
(define (disj-append/e e-p . e-ps)
  (define/match (disj-append2/e e-p1 e-p2)
    [((cons e1 1?) (cons e2 2?))
     (define s1 (size e1))
     (define s2 (size e2))
     (when (infinite? s1)
       (error "Only the last enum in a call to disj-append/e can be infinite."))
     (define (from-nat n)
       (cond [(< n s1) (decode e1 n)]
             [else (decode e2 (- n s1))]))
     (define (to-nat x)
       (cond [(1? x) (encode e1 x)]
             [(2? x) (+ (encode e2 x) s1)]
             [else (redex-error 'encode "bad term")]))
     (enum (+ s1 s2) from-nat to-nat)])
  (car
   (foldr1 (λ (e-p1 e-p2)
             (match* (e-p1 e-p2)
                     [((cons e1 1?) (cons e2 2?))
                      (cons (disj-append2/e e-p1
                                            (cons e2 (negate 1?)))
                            (λ (x)
                               (or (1? x)
                                   (2? x))))]))
           (cons e-p e-ps))))

(define (foldr1 f l)
  (match l
    [(cons x '()) x]
    [(cons x  xs) (f x (foldr1 f xs))]))

(define (fin-cons/e e1 e2)
  (define s1 (enum-size e1))
  (define s2 (enum-size e2))
  (define size (* s1 s2))
  (cond [(zero? size) empty/e]
        [(or (not (infinite? s1))
             (not (infinite? s2)))
         (define-values (fst-smaller? min-size)
           (cond [(s1 . <= . s2) (values #t s1)]
                 [else          (values #f s2)]))
         (define (dec n)
           (define-values (q r)
             (quotient/remainder n min-size))
           (define-values (n1 n2)
             (if fst-smaller?
                 (values r q)
                 (values q r)))
           (cons (decode e1 n1)
                 (decode e2 n2)))
         (define/match (enc p)
           [((cons x1 x2))
            (define n1 (encode e1 x1))
            (define n2 (encode e2 x2))
            (define-values (q r)
              (if fst-smaller?
                  (values n2 n1)
                  (values n1 n2)))
            (+ (* min-size q)
               r)])           
         (enum size dec enc)]
        [else
         (redex-error 'internal "fin-cons/e should only be called on finite enumerations")]))

;; cons/e : enum a, enum b ... -> enum (cons a b ...)
(define (cons/e e1 e2)
  (map/e (λ (x)
            (cons (first  x)
                  (second x)))
         (λ (x-y)
            (list (car x-y) (cdr x-y)))
         (list/e e1 e2)))

(define (elegant-cons/e e1 e2)
  (define s1 (size e1))
  (define s2 (size e2))
  (cond [(not (and (infinite? s1)
                   (infinite? s2)))
         (error 'finite-sets-are-inelegant)]
        [else
         (define (dec n)
           (define fl-root (integer-sqrt n))
           (define root-sq (fl-root . * . fl-root))
           (define-values
             (n1 n2)
             (cond [((n . - . root-sq) . < . fl-root)
                  (define n1 (n . - . root-sq))
                  (define n2 fl-root)
                  (values n1 n2)]
                 [else
                  (define n1 fl-root)
                  (define n2 (- n root-sq fl-root))
                  (values n1 n2)]))
           (cons (decode e1 n1)
                 (decode e2 n2)))
         (define/match (enc p)
           [((cons x1 x2))
            (define n1 (encode e1 x1))
            (define n2 (encode e2 x2))
            (cond [(n1 . < . n2)
                   ((n2 . * . n2) . + . n1)]
                  [else
                   (+ (n1 . * . n1) n1 n2)])])
         (enum +inf.0 dec enc)]))

;; Traversal (maybe come up with a better name
;; traverse/e : (a -> enum b), (listof a) -> enum (listof b)
(define (traverse/e f xs)
  (apply list/e (map f xs)))

;; Hash Traversal
;; hash-traverse/e : (a -> enum b), (hash[k] -o> a) -> enum (hash[k] -o> b)
(define (hash-traverse/e f ht)
  ;; as-list : listof (cons k a)
  (define as-list (hash->list ht))
  ;; on-cdr : (cons k a) -> enum (cons k b)
  (define/match (on-cdr pr)
    [((cons k v))
     (map/e (λ (x) (cons k x))
            cdr
            (f v))])
  ;; enum (listof (cons k b))
  (define assoc/e
    (traverse/e on-cdr as-list))
  (map/e make-immutable-hash
         hash->list
         assoc/e))

;; the nth triangle number
(define (tri n)
  (/ (* n (+ n 1))
     2))

;; the floor of the inverse of tri
;; returns the largest triangle number less than k
;; always returns an integer
(define (floor-untri k)
  (let ([n (integer-sqrt (+ 1 (* 8 k)))])
    (/ (- n 
          (if (even? n)
              2
              1))
       2)))

;; dep/e : enum a (a -> enum b) -> enum (a, b)
(define (dep/e e f)
  (define (search-size sizes n)
    (define (loop cur)
      (let* ([lastSize (gvector-ref sizes (- cur 1))]
             [e2 (f (decode e cur))]
             [s  (+ lastSize (size e2))])
        (gvector-add! sizes s)
        (if (> s n)
            cur
            (loop (+ cur 1)))))
    (loop (gvector-count sizes)))
  ;; fill-table - find sizes[n], filling the table as it goes
  ;; assumption: n >= (gvector-count sizes)
  (define (fill-table sizes n)
    (let loop ([cur (gvector-count sizes)])
      (let* ([prevSize (gvector-ref sizes (- cur 1))]
             [curE (f (decode e cur))]
             [s (+ prevSize (size curE))])
        (gvector-add! sizes s)
        (if (= cur n)
            s
            (loop (+ cur 1))))))
  (if (= 0 (size e))
      empty/e
      (let ([first (size (f (decode e 0)))])
        (cond
         [(not (infinite? first))
          ;; memo table caches the size of the dependent enumerators
          ;; sizes[n] = # of terms with left side index <= n
          ;; sizes : gvector int
          (let ([sizes (gvector first)])
            (enum (if (infinite? (size e))
                      +inf.0
                      (foldl
                       (λ (curSize acc)
                          (let ([sum (+ curSize acc)])
                            (gvector-add! sizes sum)
                            sum))
                       first (map (compose size f) (cdr (to-list e)))))
                  (λ (n)
                     (let* ([ind (or (find-size sizes n)
                                     (search-size sizes n))]
                            [l (if (= ind 0)
                                   0
                                   (gvector-ref sizes (- ind 1)))]
                            [m (- n l)]
                            [x (decode e ind)]
                            [e2 (f x)]
                            [y (decode e2 m)])
                       (cons x y)))
                  (λ (ab)
                     (let* ([a (car ab)]
                            [b (cdr ab)]
                            [ai (encode e a)]
                            [ei (f a)]
                            [nextSize (size ei)]
                            [sizeUpTo (if (= ai 0)
                                          0
                                          (or (gvector-ref sizes (- ai 1) #f)
                                              (let ([sizeUp
                                                     (fill-table sizes (- ai 1))])
                                                (begin0
                                                    sizeUp
                                                  (gvector-add! sizes
                                                                (+ nextSize
                                                                   sizeUp))))))])
                       (+ sizeUpTo
                          (encode ei b))))))]
         [(not (infinite? (size e)))
          (enum +inf.0
                (λ (n)
                  (define-values (q r) (quotient/remainder n (size e)))
                  (cons (decode e r)
                        (decode (f (decode e r)) q)))
                (λ (ab)
                   (+ (* (size e) (encode (f (car ab)) (cdr ab)))
                      (encode e (car ab)))))]
         [else ;; both infinite, same as cons/e
          (enum +inf.0               
                (λ (n)
                   (let* ([k (floor-untri n)]
                          [t (tri k)]
                          [l (- n t)]
                          [m (- k l)]
                          [a (decode e l)])
                     (cons a
                           (decode (f a) m))))
                (λ (xs) ;; bijection from nxn -> n, inverse of previous
                   ;; (n,m) -> (n+m)(n+m+1)/2 + n
                   (unless (pair? xs)
                     (redex-error 'encode "not a pair"))
                   (let ([l (encode e (car xs))]
                         [m (encode (f (car xs)) (cdr xs))])
                     (+ (/ (* (+ l m) (+ l m 1))
                           2)
                        l))))]))))

;; find-size : gvector int, int -> either int #f
;; binary search for the index of the smallest element of vec greater
;; than n or #f if no such element exists
(define (find-size vec n)
  (define (bin-search min max)
    (cond [(= min max) min]
          [(= (- max min) 1)
           (cond [(> (gvector-ref vec min) n)
                  min]
                 [else max])]
          [else
           (let ([mid (quotient (+ max min)
                                2)])
             (cond [(> (gvector-ref vec mid) n)
                    (bin-search min mid)]
                   [else
                    (bin-search mid max)]))]))
  (let ([size (gvector-count vec)])
    (cond [(or (= size 0)
               (<= (gvector-ref vec (- size 1))
                   n))
           #f]
          [else (bin-search 0 (- size 1))])))

;; dep2 : natural (enum a) (a -> enum b) -> enum (a,b)
(define (dep2/e n e f)
  (define (search-size sizes n)
    (define (loop cur)
      (let* ([lastSize (gvector-ref sizes (- cur 1))]
             [e2 (f (decode e cur))]
             [s  (+ lastSize (size e2))])
        (gvector-add! sizes s)
        (if (> s n)
            cur
            (loop (+ cur 1)))))
    (loop (gvector-count sizes)))
  ;; fill-table - find sizes[n], filling the table as it goes
  ;; assumption: n >= (gvector-count sizes)
  (define (fill-table sizes n)
    (let loop ([cur (gvector-count sizes)])
      (let* ([prevSize (gvector-ref sizes (- cur 1))]
             [curE (f (decode e cur))]
             [s (+ prevSize (size curE))])
        (gvector-add! sizes s)
        (if (= cur n)
            s
            (loop (+ cur 1))))))
  (if (= 0 (size e))
      empty/e
      (cond
       [(not (infinite? n))
        ;; memo table caches the size of the dependent enumerators
        ;; sizes[n] = # of terms with left side index <= n
        ;; sizes : gvector int
        (let* ([first (size (f (decode e 0)))]
               [sizes (gvector first)])
          (enum (if (infinite? (size e))
                    +inf.0
                    (foldl
                     (λ (curSize acc)
                        (let ([sum (+ curSize acc)])
                          (gvector-add! sizes sum)
                          sum))
                     first (map (compose size f) (cdr (to-list e)))))
                (λ (n)
                   (let* ([ind (or (find-size sizes n)
                                   (search-size sizes n))]
                          [l (if (= ind 0)
                                 0
                                 (gvector-ref sizes (- ind 1)))]
                          [m (- n l)]
                          [x (decode e ind)]
                          [e2 (f x)]
                          [y (decode e2 m)])
                     (cons x y)))
                (λ (ab)
                   (let* ([a (car ab)]
                          [b (cdr ab)]
                          [ai (encode e a)]
                          [ei (f a)]
                          [nextSize (size ei)]
                          [sizeUpTo (if (= ai 0)
                                        0
                                        (or (gvector-ref sizes (- ai 1) #f)
                                            (let ([sizeUp
                                                   (fill-table sizes (- ai 1))])
                                              (begin0
                                                  sizeUp
                                                (gvector-add! sizes
                                                              (+ nextSize
                                                                 sizeUp))))))])
                     (+ sizeUpTo
                        (encode ei b))))))]
       [(not (infinite? (size e)))
        (enum +inf.0
              (λ (n)
                (define-values (q r) (quotient/remainder n (size e)))
                (cons (decode e r)
                      (decode (f (decode e r)) q)))
              (λ (ab)
                 (+ (* (size e) (encode (f (car ab)) (cdr ab)))
                    (encode e (car ab)))))]
       [else ;; both infinite, same as cons/e
        (enum +inf.0               
              (λ (n)
                 (let* ([k (floor-untri n)]
                        [t (tri k)]
                        [l (- n t)]
                        [m (- k l)]
                        [a (decode e l)])
                   (cons a
                         (decode (f a) m))))
              (λ (xs) ;; bijection from nxn -> n, inverse of previous
                 ;; (n,m) -> (n+m)(n+m+1)/2 + n
                 (unless (pair? xs)
                   (redex-error 'encode "not a pair"))
                 (let ([l (encode e (car xs))]
                       [m (encode (f (car xs)) (cdr xs))])
                   (+ (/ (* (+ l m) (+ l m 1))
                         2)
                      l))))])))

;; fold-enum : ((listof a), b -> enum a), (listof b) -> enum (listof a)
(define (fold-enum f l)
  (map/e
   reverse
   reverse
   (let loop ([l l]
              [acc (const/e '())])
     (cond [(empty? l) acc]
           [else
            (loop
             (cdr l)
             (flip-dep/e
              acc
              (λ (xs)
                 (f xs (car l)))))]))))

;; flip-dep/e : enum a (a -> enum b) -> enum (b,a)
(define (flip-dep/e e f)
  (map/e
   (λ (ab)
      (cons (cdr ab)
            (car ab)))
   (λ (ba)
      (cons (cdr ba)
            (car ba)))
   (dep/e e f)))

;; more utility enums
;; nats of course
(define (range/e low high)
  (cond [(> low high) (redex-error 'range/e "invalid range: ~s, ~s" low high)]
        [(infinite? high)
         (if (infinite? low)
             int/e
             (map/e
              (λ (n)
                 (+ n low))
              (λ (n)
                 (- n low))
              nat/e))]
        [(infinite? low)
         (map/e
          (λ (n)
             (- high n))
          (λ (n)
             (+ high n))
          nat/e)]
        [else
         (map/e (λ (n) (+ n low))
                (λ (n) (- n low))
                (take/e nat/e (+ 1 (- high low))))]))

;; thunk/e : Nat or +-Inf, ( -> enum a) -> enum a
(define (thunk/e s thunk)
  (define promise/e (delay (thunk)))
  (enum s
        (λ (n)
           (decode (force promise/e) n))
        (λ (x)
           (encode (force promise/e) x))))

;; fix/e : [size] (enum a -> enum a) -> enum a
(define fix/e
  (case-lambda
    [(f/e) (fix/e +inf.0 f/e)]
    [(size f/e)
     (define self (delay (f/e (fix/e size f/e))))
     (enum size
           (λ (n)
             (decode (force self) n))
           (λ (x)
             (encode (force self) x)))]))

;; many/e : enum a -> enum (listof a)
;;       or : enum a, #:length natural -> enum (listof a)
(define many/e
  (case-lambda
    [(e)
     (define fix-size
       (if (= 0 (size e))
           1
           +inf.0))
     (fix/e fix-size
            (λ (self)
               (disj-sum/e (cons (fin/e '()) null?)
                           (cons (cons/e e self) pair?))))]
    [(e n)
     (apply list/e (build-list n (const e)))]))

;; many1/e : enum a -> enum (nonempty listof a)
(define (many1/e e)
  (except/e (many/e e) '()))

(define (cantor-vec/e . es)
  (map/e list->vector
         vector->list
         (apply cantor-list/e es)))

;; vec/e : listof (enum any) -> enum (vectorof any)
(define vec/e cantor-vec/e)

(define (box-vec/e . es)
  (map/e list->vector
         vector->list
         (apply box-list/e es)))

(define (cantor-untuple k)
  ;; Paul Tarau Deriving a Fast Inverse of the Generalized Cantor N-tupling Bijection
  (λ (n)
     (inc-set->list (combinatorial-number-decode k n))))

(define (combinatorial-number-decode k n)
  (define (loop k n acc)
    (cond [(k . < . 0) (error 'combinatorial-number-decode-bug)]
          [(k . = . 0) acc]
          [else
           (define k2 (sub1 k))
           (define i  (k . + . n))
           (define d
             (let/ec kont
               (for ([j (in-range k2 (add1 i))])
                 (define b (binomial j k))
                 (when (b . > . n)
                   (kont (sub1 j))))
               (error 'ididntfindit)))
           (define n2 (n . - . (binomial d k)))
           (loop k2 n2 (cons d acc))]))
  (loop k n '()))

(define (cantor-tuple k)
  (λ (xs)
     (unless ((length xs) . = . k)
       (error 'bad-length-cantor-tuple))
     ;; Section 6 of Tarau Cantor n-tupling inverse
     (define sums
       (list->inc-set xs))
     (for/sum ([sum_i (in-list sums)]
               [n     (in-naturals)])
       (binomial sum_i (add1 n)))))

(define (list->inc-set xs)
  (define (loop xs count acc)
    (match xs
      ['() (reverse acc)]
      [(cons hd tl)
       (define acc-hd
         (+ hd count 1))
       (loop tl
             acc-hd
             (cons acc-hd acc))]))
  (loop xs -1 '()))

(define (inc-set->list xs)
  (define (loop xs count acc)
    (match xs
      ['() (reverse acc)]
      [(cons hd tl)
       (define acc-hd
         (- hd count 1))
       (loop tl
             hd
             (cons acc-hd acc))]))
  (loop xs -1 '()))

(define (tuple-constructors infs fins)
  (define inf?s (inf-slots (map cdr infs)
                           (map cdr fins)))
  (define (decon xs)
    (let loop ([xs xs]
               [inf-acc '()]
               [fin-acc '()]
               [inf?s   inf?s])
      (match* (xs inf?s)
              [('() '()) (cons (reverse inf-acc)
                               (reverse fin-acc))]
              [((cons x rest-xs) (cons inf? rest-inf?s))
               (cond [inf?
                      (loop rest-xs
                            (cons x inf-acc)
                            fin-acc
                            rest-inf?s)]
                     [else
                      (loop rest-xs
                            inf-acc
                            (cons x fin-acc)
                            rest-inf?s)])])))
  (define (recon infs-fins)
    (match-define (cons infs fins) infs-fins)
    (let loop ([infs infs]
               [fins fins]
               [inf?s inf?s]
               [acc '()])
      (match inf?s
        ['() (reverse acc)]
        [(cons inf? rest)
         (cond [inf?
                (loop (cdr infs)
                      fins
                      rest
                      (cons (car infs) acc))]
               [else
                (loop infs
                      (cdr fins)
                      rest
                      (cons (car fins) acc))])])))
  (values decon recon))

;; list/e : listof (enum any) -> enum (listof any)
(define (list/e . es)
  (define nat/es
    (for/list ([e (in-list es)])
      (take/e nat/e (size e))))
            
  (map/e
   (curry map decode es)
   (curry map encode es)
   (mixed-box-tuples/e nat/es)))

(define (mixed-box-tuples/e es)
  (match es
    ['()  (const/e '())]
    [(list e) (map/e list car (car es))]
    [_
     (cond [(for/or ([e (in-list es)])
              (zero? (size e)))
            empty/e]
           [else
            (define layers (mk-list-layers es))

            (define eis
              (for/list ([i (in-naturals)]
                         [e (in-list es)])
                (cons e i)))

            (define prev-cur-layers (map cons
                                         (cons (list-layer 0 0 '() eis) (reverse (rest (reverse layers))))
                                         layers))

            (define layer/es
              (for/list ([prev-cur (in-list prev-cur-layers)])
                (match-define (cons (list-layer prev-max
                                                prev-tuple-max
                                                prev-exhs
                                                prev-inexhs)
                                    (list-layer cur-max
                                                cur-tuple-max
                                                cur-exhs
                                                cur-inexhs))
                              prev-cur)

                (define-values (decon recon)
                  (tuple-constructors cur-inexhs cur-exhs))

                (define k (length cur-inexhs))
                (define inexhs-lo (expt prev-tuple-max k))
                (define inexhs-hi (expt cur-tuple-max  k))

                (define inxh-tups
                  (for/list ([_ cur-inexhs])
                    nat/e))

                (define layer/e
                  (map/e
                   recon
                   decon
                   (fin-cons/e
                    (slice/e (apply box-list/e inxh-tups)
                             inexhs-lo
                             inexhs-hi)
                    (mixed-box-tuples/e (map car (sort cur-exhs < #:key cdr))))))
                (list layer/e
                      cur-max
                      prev-max
                      cur-tuple-max)))

            (define (dec n)
              (let/ec return
                (for ([layer (in-list layer/es)])
                  (match layer
                    [(list e
                           max-index
                           min-index
                           _)
                     (when (n . < . max-index)
                       (return (decode e (n . - . min-index))))]))))

            (define (enc tup)
              (define m (apply max tup))
              (let/ec return
                (for ([layer (in-list layer/es)])
                  (match layer
                    [(list e
                           _
                           min-index
                           max-max)
                     (when (m . < . max-max)
                       (return (+ min-index (encode e tup))))]))))

            (enum (apply * (map size es))
                  dec
                  enc)])]))

(define/contract (inf-slots infs fins)
  (-> (listof number?)
      (listof number?)
      any/c)
  (define sorted-infs (sort infs <))
  (define sorted-fins (sort fins <))
  (reverse
   (let loop ([inf-is sorted-infs]
              [fin-is sorted-fins]
              [acc    '()])
     (match* (inf-is fin-is)
             [('() '()) acc]
             [((cons _ _) '())
              (append (for/list ([_ (in-list inf-is)]) #t) acc)]
             [('() (cons _ _))
              (append (for/list ([_ (in-list fin-is)]) #f) acc)]
             [((cons ii rest-iis) (cons fi rest-fis))
              (cond [(ii . < . fi)
                     (loop rest-iis
                           fin-is
                           (cons #t acc))]
                    [else
                     (loop inf-is
                           rest-fis
                           (cons #f acc))])]))))

(define (nested-cons-list/e . es)
  (define l (length es))
  (define split-point (quotient l 2))
  (define-values (left right) (split-at es split-point))
  (map/e
   (λ (pr) (append (car pr) (cdr pr)))
   (λ (lst)
      (define-values (left right) (split-at lst split-point))
      (cons left right))
   (fin-cons/e (apply list/e left) (apply list/e right))))


(define (all-infinite? es)
  (all-sizes-something? infinite? es))

(define (all-finite? es)
  (all-sizes-something? (negate infinite?) es))

(define (all-sizes-something? p? es)
  (for/and ([e (in-list es)])
    (p? (size e))))

;; Fair tupling via generalized cantor n-tupling
;; ordering is monotonic in the sum of the elements of the list
(define (cantor-list/e . es)
  (define all-inf? (all-infinite? es))
  (cond [(empty? es) (const/e '())]
        [(not all-inf?)
         (apply list/e es)]
        [else
         (define k (length es))
         (define dec
           (compose
            (λ (xs) (map decode es xs))
            (cantor-untuple k)))
         (define enc
           (compose
            (cantor-tuple k)
            (λ (xs) (map encode es xs))))
         (enum +inf.0 dec enc)]))

;; Fair tupling via generalized
;; ordering is monotonic in the max of the elements of the list
(define (box-list/e . es)
  (define all-inf? (all-infinite? es))
  (cond [(empty? es) (const/e '())]
        [(not all-inf?) (apply list/e es)]
        [else
         (define k (length es))
         (map/e
          (curry map decode es)
          (curry map encode es)
          (box-tuples/e k))]))

(define (box-tuples/e k)
  (enum +inf.0 (box-untuple k) (box-tuple k)))

;; Tuples of length k with maximum bound
(define (bounded-list/e len bound)
  (define (loop len)
    (match len
      [0 (const/e '())]
      [1 (const/e `(,bound))]
      [_
       (define smallers/e (loop (sub1 len)))
       (define bounded/e (take/e nat/e (add1 bound)))
       (define first-max/e
         (map/e
          (curry cons bound)
          cdr
          (apply list/e
           (for/list ([_ (in-range (sub1 len))])
             bounded/e))))
       (define first-not-max/e
         (match bound
           [0 empty/e]
           [_ (fin-cons/e (take/e nat/e bound)
                          smallers/e)]))
       (define (first-max? l)
         ((first l) . = . bound))
       (disj-append/e (cons first-not-max/e (negate first-max?))
                      (cons first-max/e     first-max?))]))
  (loop len))

(define (box-tuple k)
  (λ (xs)
     (define layer (apply max xs))
     (define smallest (expt layer k))
     (define layer/e (bounded-list/e k layer))
     (smallest . + . (encode layer/e xs))))

(define (box-untuple k)
  (λ (n)
     (define layer (integer-root n k))
     (define smallest (expt layer k))
     (define layer/e (bounded-list/e k layer))
     (decode layer/e (n . - . smallest))))

(define (nat+/e n)
  (map/e (λ (k)
            (+ k n))
         (λ (k)
            (- k n))
         nat/e))

;; fail/e : exn -> enum ()
;; returns an enum that calls a thunk
(define (fail/e e)
  (let ([t
         (λ (_)
            (raise e))])
    (enum 1
          t
          t)))

(module+
 test
 (require rackunit)
 (provide check-bijection?
          int/e
          find-size
          list->inc-set
          inc-set->list)
 (define confidence 1000)

 (define-simple-check (check-bijection? e)
   (let ([nums (build-list (if (<= (enum-size e) confidence)
                               (enum-size e)
                               confidence)
                           identity)])
     (andmap =
             nums
             (map (λ (n)
                     (encode e (decode e n)))
                  nums)))))
;; Base Type enumerators
(define (between? x low high)
  (and (>= x low)
       (<= x high)))
(define (range-with-pred/e-p low high)
  (cons (range/e low high)
        (λ (n) (between? n low high))))
(define low/e-p
  (range-with-pred/e-p #x61 #x7a))
(define up/e-p
  (range-with-pred/e-p #x41 #x5a))
(define bottom/e-p
  (range-with-pred/e-p #x0 #x40))
(define mid/e-p
  (range-with-pred/e-p #x5b #x60))
(define above1/e-p
  (range-with-pred/e-p #x7b #xd7FF))
(define above2/e-p
  (range-with-pred/e-p #xe000 #x10ffff))

(define char/e
  (map/e
   integer->char
   char->integer
   (disj-append/e low/e-p
                  up/e-p
                  bottom/e-p
                  mid/e-p
                  above1/e-p
                  above2/e-p)))

(define string/e
  (map/e
   list->string
   string->list
   (many/e char/e)))

(define from-1/e
  (map/e add1
         sub1
         nat/e))

(define integer/e
  (disj-sum/e (cons (const/e 0) zero?)
              (cons from-1/e (λ (n) (> n 0)))
              (cons (map/e - - from-1/e)
                    (λ (n) (< n 0)))))

(define weird-flonums/e-p
  (cons (from-list/e '(+inf.0 -inf.0 +nan.0))
        (λ (n)
           (and (flonum? n)
                (or (infinite? n)
                    (nan? n))))))
(define normal-flonums/e-p
  (cons (take/e (map/e
                 ordinal->flonum
                 flonum->ordinal
                 integer/e)
                (+ 1 (* 2 9218868437227405311)))
        (λ (n)
           (and (flonum? n)
                (nor (infinite? n)
                     (nan? n))))))
(define float/e
  (disj-append/e weird-flonums/e-p
                 normal-flonums/e-p))

(define real/e
  (disj-sum/e (cons integer/e exact-integer?)
              (cons float/e flonum?)))

(define non-real/e
  (map/e make-rectangular
         (λ (z)
            (values (real-part z)
                    (imag-part z)))
         real/e
         (except/e real/e 0 0.0)))

(define num/e
  (disj-sum/e (cons real/e real?)
              (cons non-real/e complex?)))

(define bool/e
  (from-list/e '(#t #f)))

(define var/e
  (map/e
   (compose string->symbol list->string)
   (compose string->list symbol->string)
   (many/e char/e)))

(define base/e
  (disj-sum/e (cons (const/e '()) null?)
              (cons num/e number?)
              (cons string/e string?)
              (cons bool/e boolean?)
              (cons var/e symbol?)))

(define any/e
  (fix/e +inf.0
         (λ (any/e)
            (disj-sum/e (cons base/e (negate pair?))
                        (cons (cons/e any/e any/e) pair?)))))
(define (var-prefix/e s)
  (define as-str (symbol->string s))
  (map/e (compose string->symbol
                  (curry string-append as-str)
                  symbol->string)
         (compose string->symbol
                  list->string
                  (curry (flip drop) (string-length as-str))
                  string->list
                  symbol->string)
         var/e))

(define (flip f)
  (λ (x y)
     (f y x)))
