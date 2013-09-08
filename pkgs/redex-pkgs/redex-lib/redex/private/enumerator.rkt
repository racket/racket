#lang racket/base
(require racket/math
         racket/list
         racket/function
         data/gvector)

(provide enum
         enum?
         size
         encode
         decode
         empty/e
         const/e
         from-list/e
         sum/e
         prod/e
         dep/e
         dep2/e ;; doesn't require size
         map/e
         filter/e ;; very bad, only use for small enums
         except/e 
         thunk/e
         listof/e
         list/e
         fail/e
         
         to-list
         take/e
         drop/e
         fold-enum
         display-enum

         nats
         range/e
         nats+/e
         )

;; an enum a is a struct of < Nat or +Inf, Nat -> a, a -> Nat >
(struct enum
        (size from to)
        #:prefab)

;; size : enum a -> Nat or +Inf
(define (size e)
  (enum-size e))

;; decode : enum a, Nat -> a
(define (decode e n)
  (if (and (< n (enum-size e))
           (>= n 0))
      ((enum-from e) n)
      (error 'out-of-range)))

;; encode : enum a, a -> Nat
(define (encode e a)
  ((enum-to e) a))

;; Helper functions
;; map/e : (a -> b), (b -> a), enum a -> enum b
(define (map/e f inv-f e)
  (enum (size e)
        (compose f (enum-from e))
        (compose (enum-to e) inv-f)))

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

;; except/e : enum a, a -> enum a
(define (except/e e excepts)
  (cond [(empty? excepts) e]
        [else
         (except/e
          (begin
            (with-handlers ([exn:fail? (λ (_) e)])
              (let ([m (encode e (car excepts))])
                (enum (- (size e) 1)
                      (λ (n)
                         (if (< n m)
                             (decode e n)
                             (decode e (+ n 1))))
                      (λ (x)
                         (let ([n (encode e x)])
                           (cond [(< n m) n]
                                 [(> n m) (- n 1)]
                                 [else (error 'excepted)])))))))
          (cdr excepts))]))

;; to-list : enum a -> listof a
;; better be finite
(define (to-list e)
  (when (infinite? (size e))
    (error 'too-big))
  (map (enum-from e)
       (build-list (size e)
                   identity)))

;; take/e : enum a, Nat -> enum a
;; returns an enum of the first n parts of e
;; n must be less than or equal to size e
(define (take/e e n)
  (unless (or (infinite? (size e))
              (<= n (size e)))
    (error 'too-big))
  (enum n
        (λ (k)
           (unless (< k n)
             (error 'out-of-range))
           (decode e k))
        (λ (x)
           (let ([k (encode e x)])
             (unless (< k n)
               (error 'out-of-range))
             k))))

;; drop/e : enum a, Nat -> enum a
(define (drop/e e n)
  (unless (or (infinite? (size e))
              (<= n (size e)))
    (error 'too-big))
  (enum (- (size e) n)
        (λ (m)
           (decode e (+ n m)))
        (λ (x)
           (- (encode e x) n))))

;; display-enum : enum a, Nat -> void
(define (display-enum e n)
  (for ([i (range n)])
    (display (decode e i))
    (newline) (newline)))

(define empty/e
  (enum 0
        (λ (n)
           (error 'empty))
        (λ (x)
           (error 'empty))))

(define (const/e c)
  (enum 1
        (λ (n)
           c)
        (λ (x)
           (if (equal? c x)
               0
               (error 'bad-val)))))

;; from-list/e :: Listof a -> Gen a
;; input list should not contain duplicates
(define (from-list/e l)
  (if (empty? l)
      empty/e
      (enum (length l)
            (λ (n)
               (list-ref l n))
            (λ (x)
               (length (take-while l (λ (y)
                                        (not (eq? x y)))))))))

;; take-while : Listof a, (a -> bool) -> Listof a
(define (take-while l pred)
  (cond [(empty? l) (error 'empty)]
        [(not (pred (car l))) '()]
        [else
         (cons (car l)
               (take-while (cdr l) pred))]))

(define nats
  (enum +inf.f
        identity
        (λ (n)
           (unless (>= n 0)
             (error 'out-of-range))
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

;; sum :: enum a, enum b -> enum (a or b)
(define sum/e
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond
      [(= 0 (size e1)) e2]
      [(= 0 (size e2)) e1]
      [(not (infinite? (enum-size e1)))
       (enum (+ (enum-size e1)
                (enum-size e2))
             (λ (n)
                (if (< n (enum-size e1))
                    ((enum-from e1) n)
                    ((enum-from e2) (- n (enum-size e1)))))
             (λ (x)
                (with-handlers ([exn:fail? (λ (_)
                                              (+ (enum-size e1)
                                                 ((enum-to e2) x)))])
                  ((enum-to e1) x))))]
      [(not (infinite? (enum-size e2)))
       (sum/e e2 e1)]
      [else ;; both infinite, interleave them
       (enum +inf.f
             (λ (n)
                (if (even? n)
                    ((enum-from e1) (/ n 2))
                    ((enum-from e2) (/ (- n 1) 2))))
             (λ (x)
                (with-handlers ([exn:fail? 
                                 (λ (_)
                                    (+  (* ((enum-to e2) x) 2)
                                        1))])
                  (* ((enum-to e1) x) 2))))])]
    [(a b c . rest)
     (sum/e a (apply sum/e b c rest))]))

(define n*n
  (enum +inf.f
        (λ (n)
           ;; calculate the k for which (tri k) is the greatest
           ;; triangle number <= n
           (let* ([k (floor-untri n)]
                  [t (tri k)]
                  [l (- n t)]
                  [m (- k l)])
             (cons l m)))
        (λ (ns)
           (unless (pair? ns)
             (error "not a list"))
           (let ([l (car ns)]
                 [m (cdr ns)])
             (+ (/ (* (+ l m) (+ l m 1))
                   2)
                l))) ;; (n,m) -> (n+m)(n+m+1)/2 + n
        ))

;; prod/e : enum a, enum b -> enum (a,b)
(define prod/e
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond [(or (= 0 (size e1))
                (= 0 (size e2))) empty/e]
           [(not (infinite? (enum-size e1)))
            (cond [(not (infinite? (enum-size e2)))
                   (let ([size (* (enum-size e1)
                                  (enum-size e2))])
                     (enum size
                           (λ (n) ;; bijection from n -> axb
                              (if (> n size)
                                  (error "out of range")
                                  (call-with-values
                                      (λ ()
                                         (quotient/remainder n (enum-size e2)))
                                    (λ (q r)
                                       (cons ((enum-from e1) q)
                                             ((enum-from e2) r))))))
                           (λ (xs)
                              (unless (pair? xs)
                                (error "not a pair"))
                              (+ (* (enum-size e1)
                                    ((enum-to e1) (car xs)))
                                 ((enum-to e2) (cdr xs))))))]
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
                              (error "not a pair"))
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
                       (error "not a pair"))
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
                       (error "not a pair"))
                     (let ([l ((enum-to e1) (car xs))]
                           [m ((enum-to e2) (cdr xs))])
                       (+ (/ (* (+ l m) (+ l m 1))
                             2)
                          l))))])]
    [(a b c . rest)
     (prod/e a (apply prod/e b c rest))]))

;; Traversal (maybe come up with a better name
;; traverse/e : (a -> enum b), (listof a) -> enum (listof b)
(define (traverse/e f xs)
  (list/e (map f xs)))

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
         [else ;; both infinite, same as prod/e
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
                     (error "not a pair"))
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

;; dep2 : enum a (a -> enum b) -> enum (a,b)
(define (dep2/e e f)
  (cond [(= 0 (size e)) empty/e]
        [(not (infinite? (size (f (decode e 0)))))
         ;; memoize tab : boxof (hash nat -o> (nat . nat))
         ;; maps an index into the dep/e to the 2 indices that we need
         (let ([tab (box (hash))])
           (enum (if (infinite? (size e))
                     +inf.f
                     (foldl + 0 (map (compose size f) (to-list e))))
                 (λ (n) ;; n -> axb
                    (call-with-values
                        (λ ()
                           (letrec
                               ;; go : nat -> nat nat
                               ([go
                                 (λ (n)
                                    (cond [(hash-has-key? (unbox tab) n)
                                           (let ([ij (hash-ref (unbox tab) n)])
                                             (values (car ij) (cdr ij)))]
                                          [(= n 0) ;; find the first element
                                           (find 0 0 0)]
                                          [else ;; recur
                                           (call-with-values
                                               (λ () (go (- n 1)))
                                             (λ (ai bi)
                                                (find ai (- n bi 1) n)))]))]
                                ;; find : nat nat nat -> nat
                                [find
                                 ;; start is our starting eindex
                                 ;; seen is how many we've already seen
                                 (λ (start seen n)
                                    (let loop ([ai start]
                                               [seen seen])
                                      (let* ([a (decode e ai)]
                                             [bs (f a)])
                                        (cond [(< (- n seen)
                                                  (size bs))
                                               (let ([bi (- n seen)])
                                                 (begin
                                                   (set-box! tab
                                                             (hash-set (unbox tab)
                                                                       n
                                                                       (cons ai bi)))
                                                   (values ai bi)))]
                                              [else
                                               (loop (+ ai 1)
                                                     (+ seen (size bs)))]))))])
                             (go n)))
                      (λ (ai bi)
                         (let ([a (decode e ai)])
                           (cons a
                                 (decode (f a) bi))))))
                 ;; todo: memoize encode
                 (λ (ab) ;; axb -> n
                    (let ([ai (encode e (car ab))])
                      (+ (let loop ([i 0]
                                    [sum 0])
                           (if (>= i ai)
                               sum
                               (loop (+ i 1)
                                     (+ sum
                                        (size (f (decode e i)))))))
                         (encode (f (car ab))
                                 (cdr ab)))))))]
        [else ;; both infinite, same as prod/e
         (dep/e e f)]))

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
  (cond [(> low high) (error 'bad-range)]
        [(infinite? high)
         (if (infinite? low)
             ints/e
             (map/e
              (λ (n)
                 (+ n low))
              (λ (n)
                 (- n low))
              nats))]
        [(infinite? low)
         (map/e
          (λ (n)
             (- high n))
          (λ (n)
             (+ high n))
          nats)]
        [else
         (map/e (λ (n) (+ n low))
                   (λ (n) (- n low))
                   (take/e nats (+ 1 (- high low))))]))

;; thunk/e : Nat or +-Inf, ( -> enum a) -> enum a
(define (thunk/e s thunk)
  (enum s
        (λ (n)
           (decode (thunk) n))
        (λ (x)
           (encode (thunk) x))))

;; listof/e : enum a -> enum (listof a)
(define (listof/e e)
  (thunk/e
   (if (= 0 (size e))
       0
       +inf.f)
   (λ ()
      (sum/e
       (const/e '())
       (prod/e e (listof/e e))))))

;; list/e : listof (enum any) -> enum (listof any)
(define (list/e es)
  (apply prod/e (append es `(,(const/e '())))))

(define (nats+/e n)
  (map/e (λ (k)
               (+ k n))
            (λ (k)
               (- k n))
            nats))

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
