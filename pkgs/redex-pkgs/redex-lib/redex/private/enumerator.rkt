#lang racket/base
(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/math
         racket/match
         racket/promise

         data/gvector
         math/flonum

         "error.rkt")

(provide enum
         enum?
         size
         encode
         decode
         empty/e
         const/e
         from-list/e
         sum/e
         disj-sum/e
         cons/e
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
         traverse/e
         hash-traverse/e
         
         fail/e
         
         to-list
         take/e
         fold-enum

         nats/e
         range/e
         nats+/e

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
      (redex-error 'decode "Index into enumerator out of range")))

;; encode : enum a, a -> Nat
(define/contract (encode e a)
  (-> enum? any/c exact-nonnegative-integer?)
  ((enum-to e) a))

;; Helper functions
;; map/e : (a -> b), (b -> a), enum a -> enum b
(define (map/e f inv-f e . es)
  (cond [(empty? es)
         (enum (size e)
               (compose f (enum-from e))
               (compose (enum-to e) inv-f))]
        [else
         (define es/e (list/e (cons e es)))
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
  (unless (or (<= n (size e)))
    (redex-error 'take/e "there aren't ~s elements in ~s" n e))
  (enum n
        (λ (k)
           (decode e k))
        (λ (x)
           (let ([k (encode e x)])
             (unless (< k n)
               (redex-error 'take/e "attempted to encode an element not in an enumerator"))
             k))))

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

(define nats/e
  (enum +inf.f
        identity
        (λ (n)
           (unless (>= n 0)
             (redex-error 'encode "Not a natural"))
           n)))
(define ints/e
  (enum +inf.f
        (λ (n)
           (if (even? n)
               (* -1 (/ n 2))
               (/ (+ n 1) 2)))
        (λ (n)
           (if (> n 0)
               (- (* 2 n) 1)
               (* 2 (abs n))))))

;; sum :: enum a, enum b -> enum (U a b)
(define sum/e
  (case-lambda
    [(e) e]
    [(e1 e2)
     ;; Sum two enumerators of different sizes
     (define (sum-uneven less/e more/e)
       ;; interleave until less/e is exhausted
       ;; pairsdone is 1+ the highest index using less/e
       (let* ([less-size (size less/e)]
              [pairsdone (* 2 less-size)])
         (enum (+ less-size (size more/e))
               (λ (n)
                  (if (< n pairsdone)
                      (let-values ([(q r) (quotient/remainder n 2)])
                        ;; Always put e1 first though!
                        (decode (match r
                                  [0 e1]
                                  [1 e2])
                                q))
                      (decode more/e (- n less-size))))
               (λ (x)
                  (with-handlers
                      ([exn:fail?
                        (λ (_)
                           (let ([i (encode more/e x)])
                             (if (< i less-size)
                                 (+ (* 2 i) 1)
                                 (+ (- i less-size) pairsdone))))])
                    (* 2 (encode less/e x)))))))
     (let* ([s1 (size e1)]
            [s2 (size e2)])
       (cond
        [(= 0 s1) e2]
        [(= 0 s2) e1]
        [(< s1 s2)
         (sum-uneven e1 e2)]
        [(< s2 s1)
         (sum-uneven e2 e1)]
        [else ;; both the same length, interleave them
         (enum (+ s1 s2)
               (λ (n)
                  (if (even? n)
                      ((enum-from e1) (/ n 2))
                      ((enum-from e2) (/ (- n 1) 2))))
               (λ (x)
                  (with-handlers ([exn:fail? 
                                   (λ (_)
                                      (+  (* ((enum-to e2) x) 2)
                                          1))])
                    (* ((enum-to e1) x) 2))))]))]
    [(a b c . rest)
     ;; map-pairs : (a, a -> b), (a -> b), listof a -> listof b
     ;; apply the function to every pair, applying f to the first element of an odd length list
     (define (map-pairs f d l)
       (define (map-pairs/even l)
         (match l
           ['() '()]
           [`(,x ,y ,rest ...)
            (cons (f x y)
                  (map-pairs f d rest))]))
       (if (even? (length l))
           (map-pairs/even l)
           (cons (d (car l))
                 (map-pairs/even (cdr l)))))
     (apply sum/e (map-pairs sum/e identity (list* a b c rest)))]))

(define (disj-sum/e #:alternate? [alternate? #f] #:append? [append? #f] e-p . e-ps)
  (define/match (disj-sum2/e e-p1 e-p2)
    [((cons e1 1?) (cons e2 2?))
     (define (alternate-uneven less/e less? more/e more? #:less-first? less-first?)
       (define-values (first/e second/e)
         (if less-first?
             (values less/e more/e)
             (values more/e less/e)))
       ;; interleave until less/e is exhausted
       ;; pairsdone is 1+ the highest index using less/e
       (define less-size (size less/e))
       (define pairsdone (* 2 less-size))
       (define (from-nat n)
         (cond [(< n pairsdone)
                (define-values (q r) (quotient/remainder n 2))
                ;; Always put e1 first though!
                (decode (match r
                          [0 first/e]
                          [1 second/e])
                        q)]
               [else (decode more/e (- n less-size))]))
       (define (to-nat x)
         (cond [(less? x)
                (+ (* 2 (encode less/e x))
                   (if less-first? 0 1))]
               [(more? x)
                (define i (encode more/e x))
                (if (< i less-size)
                    (+ (* 2 i)
                       (if less-first? 1 0))
                    (+ (- i less-size) pairsdone))]
               [else (redex-error 'encode "bad term")]))
       (enum (+ less-size (size more/e))
             from-nat
             to-nat))     
     (define s1 (size e1))
     (define s2 (size e2))
     (cond [(not (xor alternate? append?))
            (redex-error 'disj-sum/e "Conflicting options chosen, must pick exactly one of #:alternate? or #:append?")]
           [alternate?
            (cond [(= 0 s1) e2]
                  [(= 0 s2) e1]
                  [(< s1 s2) (alternate-uneven e1 1? e2 2? #:less-first? #t)]
                  [(< s2 s1) (alternate-uneven e2 2? e1 1? #:less-first? #f)]
                  [else ;; both the same length, interleave them
                   (define (from-nats n)
                     (cond [(even? n) (decode e1 (/ n 2))]
                           [else (decode e2 (/ (- n 1) 2))]))
                   (define (to-nats x)
                     (cond [(1? x) (* (encode e1 x) 2)]
                           [(2? x) (+ 1 (* (encode e2 x) 2))]
                           [else (redex-error 'encode "bad term")]))
                   (enum (+ s1 s2) from-nats to-nats)])]
           [append?
            (define (from-nat n)
              (cond [(< n s1) (decode e1 n)]
                    [else (decode e2 (- n s1))]))
            (define (to-nat x)
              (cond [(1? x) (encode e1 x)]
                    [(2? x) (+ (encode e2 x) s1)]
                    [else (redex-error 'encode "bad term")]))
            (enum (+ s1 s2) from-nat to-nat)]
           [(nor alternate? append?)
            (redex-error 'disj-sum/e "Must specify either #:alternate? or #:append?")])])
  (car
   (foldr (λ (e-p1 e-p2)
             (match* (e-p1 e-p2)
                     [((cons e1 1?) (cons e2 2?))
                      (cons (disj-sum2/e e-p1
                                         (cons e2 (negate 1?)))
                            (λ (x)
                               (or (1? x)
                                   (2? x))))]))
          (cons empty/e (λ (_) #f))
          (cons e-p e-ps))))

;; cons/e : enum a, enum b -> enum (cons a b)
(define cons/e
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond [(or (= 0 (size e1))
                (= 0 (size e2))) empty/e]
           [(not (infinite? (enum-size e1)))
            (cond [(not (infinite? (enum-size e2)))
                   (define size (* (enum-size e1)
                                   (enum-size e2)))
                   (enum size
                         (λ (n) ;; bijection from n -> axb
                            (if (> n size)
                                (redex-error 'decode "out of range")
                                (call-with-values
                                    (λ ()
                                       (quotient/remainder n (enum-size e2)))
                                  (λ (q r)
                                     (cons ((enum-from e1) q)
                                           ((enum-from e2) r))))))
                         (λ (xs)
                            (unless (pair? xs)
                              (redex-error 'encode "not a pair"))
                            (define q (encode e1 (car xs)))
                            (define r (encode e2 (cdr xs)))
                            (+ (* (enum-size e2) q) r)))]
                  [else
                   (enum +inf.f
                         (λ (n)
                            (call-with-values
                                (λ ()
                                   (quotient/remainder n (enum-size e1)))
                              (λ (q r)
                                 (cons ((enum-from e1) r)
                                       ((enum-from e2) q)))))
                         (λ (xs)
                            (unless (pair? xs)
                              (redex-error 'encode "not a pair"))
                            (+ ((enum-to e1) (car xs))
                               (* (enum-size e1)
                                  ((enum-to e2) (cdr xs))))))])]
           [(not (infinite? (enum-size e2)))
            (enum +inf.f
                  (λ (n)
                     (call-with-values
                         (λ ()
                            (quotient/remainder n (enum-size e2)))
                       (λ (q r)
                          (cons ((enum-from e1) q)
                                ((enum-from e2) r)))))
                  (λ (xs)
                     (unless (pair? xs)
                       (redex-error 'encode "not a pair"))
                     (+ (* (enum-size e2)
                           ((enum-to e1) (car xs)))
                        ((enum-to e2) (cdr xs)))))]
           [else
            (enum (* (enum-size e1)
                     (enum-size e2))
                  (λ (n)
                     (let* ([k (floor-untri n)]
                            [t (tri k)]
                            [l (- n t)]
                            [m (- k l)])
                       (cons ((enum-from e1) l)
                             ((enum-from e2) m))))
                  (λ (xs) ;; bijection from nxn -> n, inverse of previous
                     ;; (n,m) -> (n+m)(n+m+1)/2 + n
                     (unless (pair? xs)
                       (redex-error 'encode "not a pair"))
                     (let ([l ((enum-to e1) (car xs))]
                           [m ((enum-to e2) (cdr xs))])
                       (+ (/ (* (+ l m) (+ l m 1))
                             2)
                          l))))])]
    [(a b c . rest)
     (cons/e a (apply cons/e b c rest))]))

;; Traversal (maybe come up with a better name
;; traverse/e : (a -> enum b), (listof a) -> enum (listof b)
(define (traverse/e f xs)
  (list/e (map f xs)))

;; Hash Traversal
;; hash-traverse/e : (a -> enum b), (hash[k] -o> a) -> enum (hash[k] -o> b)
(define (hash-traverse/e f ht)
  ;; as-list : listof (cons k a)
  (define as-list (hash->list ht))
  ;; on-cdr : (cons k a) -> enum (cons k b)
  (define/match (on-cdr pr)
    [((cons k v))
     (cons/e (const/e k)
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
                      +inf.f
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
          (enum +inf.f
                (λ (n)
                   (call-with-values
                       (λ ()
                          (quotient/remainder n (size e)))
                     (λ (q r)
                        (cons (decode e r)
                              (decode (f (decode e r)) q)))))
                (λ (ab)
                   (+ (* (size e) (encode (f (car ab)) (cdr ab)))
                      (encode e (car ab)))))]
         [else ;; both infinite, same as cons/e
          (enum +inf.f               
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
                    +inf.f
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
        (enum +inf.f
              (λ (n)
                 (call-with-values
                     (λ ()
                        (quotient/remainder n (size e)))
                   (λ (q r)
                      (cons (decode e r)
                            (decode (f (decode e r)) q)))))
              (λ (ab)
                 (+ (* (size e) (encode (f (car ab)) (cdr ab)))
                    (encode e (car ab)))))]
       [else ;; both infinite, same as cons/e
        (enum +inf.f               
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
             ints/e
             (map/e
              (λ (n)
                 (+ n low))
              (λ (n)
                 (- n low))
              nats/e))]
        [(infinite? low)
         (map/e
          (λ (n)
             (- high n))
          (λ (n)
             (+ high n))
          nats/e)]
        [else
         (map/e (λ (n) (+ n low))
                (λ (n) (- n low))
                (take/e nats/e (+ 1 (- high low))))]))

;; thunk/e : Nat or +-Inf, ( -> enum a) -> enum a
(define (thunk/e s thunk)
  (define promise/e (delay (thunk)))
  (enum s
        (λ (n)
           (decode (force promise/e) n))
        (λ (x)
           (encode (force promise/e) x))))

;; fix/e : size (enum a -> enum a) -> enum a
(define (fix/e size f/e)
  (define self (delay (f/e (fix/e size f/e))))
  (enum size
        (λ (n)
           (decode (force self) n))
        (λ (x)
           (encode (force self) x))))

;; many/e : enum a -> enum (listof a)
;;       or : enum a, #:length natural -> enum (listof a)
(define many/e
  (case-lambda
    [(e)
     (define fix-size
       (if (= 0 (size e))
           0
           +inf.f))
     (fix/e fix-size
            (λ (self)
               (disj-sum/e #:alternate? #t
                           (cons (const/e '()) null?)
                           (cons (cons/e e self) pair?))))]
    [(e n)
     (list/e (build-list n (const e)))]))

;; many1/e : enum a -> enum (nonempty listof a)
(define (many1/e e)
  (except/e (many/e e) '()))

;; list/e : listof (enum any) -> enum (listof any)
(define (list/e es)
  (foldr cons/e
         (const/e '())
         es))

(define (nats+/e n)
  (map/e (λ (k)
            (+ k n))
         (λ (k)
            (- k n))
         nats/e))

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
          ints/e
          find-size)
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
   (disj-sum/e #:append? #t
               low/e-p
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
         nats/e))

(define integer/e
  (disj-sum/e #:alternate? #t
              (cons (const/e 0) zero?)
              (cons from-1/e (λ (n) (> n 0)))
              (cons (map/e - - from-1/e)
                    (λ (n) (< n 0)))))

;; The last 3 here are -inf.0, +inf.0 and +nan.0
;; Consider moving those to the beginning
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
  (disj-sum/e #:append? #t
              weird-flonums/e-p
              normal-flonums/e-p))

(define real/e
  (disj-sum/e #:alternate? #t
              (cons integer/e exact-integer?)
              (cons float/e flonum?)))

(define non-real/e
  (map/e make-rectangular
         (λ (z)
            (values (real-part z)
                    (imag-part z)))
         real/e
         (except/e real/e 0 0.0)))

(define num/e
  (disj-sum/e #:alternate? #t
              (cons real/e real?)
              (cons non-real/e complex?)))

(define bool/e
  (from-list/e '(#t #f)))

(define var/e
  (map/e
   (compose string->symbol list->string)
   (compose string->list symbol->string)
   (many/e char/e)))

(define base/e
  (disj-sum/e #:alternate? #t
              (cons (const/e '()) null?)
              (cons num/e number?)
              (cons string/e string?)
              (cons bool/e boolean?)
              (cons var/e symbol?)))

(define any/e
  (fix/e +inf.f
         (λ (any/e)
            (disj-sum/e #:alternate? #t
                        (cons base/e (negate pair?))
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
