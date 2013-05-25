#lang racket/base
(require racket/math
         racket/list
         racket/function)

(provide enum
         enum?
         size
         encode
         decode
         empty/enum
         const/enum
         from-list/enum
         sum/enum
         prod/enum
         dep/enum
         dep2/enum ;; doesn't require size
         map/enum
         filter/enum ;; very bad, only use for small enums
         except/enum 
         thunk/enum
         listof/enum
         list/enum
         fail/enum
         
         to-list
         take/enum
         drop/enum
         fold-enum
         display-enum

         nats
         range/enum
         nats+/enum
         
         error/enum
         unsupported/enum)

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
;; map/enum : (a -> b), (b -> a), enum a -> enum b
(define (map/enum f inv-f e)
  (enum (size e)
        (compose f (enum-from e))
        (compose (enum-to e) inv-f)))

;; filter/enum : enum a, (a -> bool) -> enum a
;; size won't be accurate!
;; encode is not accurate right now!
(define (filter/enum e p)
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

;; except/enum : enum a, a -> enum a
(define (except/enum e excepts)
  (cond [(empty? excepts) e]
        [else
         (except/enum
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

;; take/enum : enum a, Nat -> enum a
;; returns an enum of the first n parts of e
;; n must be less than or equal to size e
(define (take/enum e n)
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

;; drop/enum : enum a, Nat -> enum a
(define (drop/enum e n)
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

(define empty/enum
  (enum 0
        (λ (n)
           (error 'empty))
        (λ (x)
           (error 'empty))))

(define (const/enum c)
  (enum 1
        (λ (n)
           c)
        (λ (x)
           (if (equal? c x)
               0
               (error 'bad-val)))))

;; from-list/enum :: Listof a -> Gen a
;; input list should not contain duplicates
(define (from-list/enum l)
  (if (empty? l)
      empty/enum
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

(define bools
  (from-list/enum (list #t #f)))
(define nats
  (enum +inf.f
        identity
        (λ (n)
           (unless (>= n 0)
             (error 'out-of-range))
           n)))
(define ints
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
(define sum/enum
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
       (sum/enum e2 e1)]
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
     (sum/enum a (apply sum/enum b c rest))]))

(define odds
  (enum +inf.f
        (λ (n)
           (+ (* 2 n) 1))
        (λ (n)
           (if (and (not (zero? (modulo n 2)))
                    (>= n 0))
               (/ (- n 1) 2)
               (error 'odd)))))

(define evens
  (enum +inf.f
        (λ (n)
           (* 2 n))
        (λ (n)
           (if (and (zero? (modulo n 2))
                    (>= n 0))
               (/ n 2)
               (error 'even)))))

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

;; prod/enum : enum a, enum b -> enum (a,b)
(define prod/enum
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond [(or (= 0 (size e1))
                (= 0 (size e2))) empty/enum]
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
     (prod/enum a (apply prod/enum b c rest))]))

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


;; dep/enum : enum a (a -> enum b) -> enum (a,b)
(define (dep/enum e f)
  (cond [(= 0 (size e)) empty/enum]
        [(not (infinite? (size (f (decode e 0)))))
         (enum (if (infinite? (size e))
                   +inf.f
                   (foldl + 0 (map (compose size f) (to-list e))))
               (λ (n) ;; n -> axb
                  (let loop ([ei 0]
                             [seen 0])
                    (let* ([a (decode e ei)]
                           [e2 (f a)])
                      (if (< (- n seen)
                             (size e2))
                          (cons a (decode e2 (- n seen)))
                          (loop (+ ei 1)
                                (+ seen (size e2)))))))
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
                               (cdr ab))))))]
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
        [else ;; both infinite, same as prod/enum
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
                       l))))]))

;; dep2 : enum a (a -> enum b) -> enum (a,b)
(define (dep2/enum e f)
  (cond [(= 0 (size e)) empty/enum]
        [(not (infinite? (size (f (decode e 0)))))
         ;; memoize tab : boxof (hash nat -o> (nat . nat))
         ;; maps an index into the dep/enum to the 2 indices that we need
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
        [else ;; both infinite, same as prod/enum
         (dep/enum e f)]))

;; fold-enum : ((listof a), b -> enum a), (listof b) -> enum (listof a)
(define (fold-enum f l)
  (map/enum
   reverse
   reverse
   (let loop ([l l]
              [acc (const/enum '())])
     (cond [(empty? l) acc]
           [else
            (loop
             (cdr l)
             (flip-dep/enum
              acc
              (λ (xs)
                 (f xs (car l)))))]))))

;; flip-dep/enum : enum a (a -> enum b) -> enum (b,a)
(define (flip-dep/enum e f)
  (map/enum
   (λ (ab)
      (cons (cdr ab)
            (car ab)))
   (λ (ba)
      (cons (cdr ba)
            (car ba)))
   (dep/enum e f)))

;; more utility enums
;; nats of course
(define (range/enum low high)
  (cond [(> low high) (error 'bad-range)]
        [(infinite? high)
         (if (infinite? low)
             ints
             (map/enum
              (λ (n)
                 (+ n low))
              (λ (n)
                 (- n low))
              nats))]
        [(infinite? low)
         (map/enum
          (λ (n)
             (- high n))
          (λ (n)
             (+ high n))
          nats)]
        [else
         (map/enum (λ (n) (+ n low))
                   (λ (n) (- n low))
                   (take/enum nats (+ 1 (- high low))))]))

;; thunk/enum : Nat or +-Inf, ( -> enum a) -> enum a
(define (thunk/enum s thunk)
  (enum s
        (λ (n)
           (decode (thunk) n))
        (λ (x)
           (encode (thunk) x))))

;; listof/enum : enum a -> enum (listof a)
(define (listof/enum e)
  (thunk/enum
   (if (= 0 (size e))
       0
       +inf.f)
   (λ ()
      (sum/enum
       (const/enum '())
       (prod/enum e (listof/enum e))))))

;; list/enum : listof (enum any) -> enum (listof any)
(define (list/enum es)
  (apply prod/enum (append es `(,(const/enum '())))))

(define (nats+/enum n)
  (map/enum (λ (k)
               (+ k n))
            (λ (k)
               (- k n))
            nats))

;; fail/enum : exn -> enum ()
;; returns an enum that calls a thunk
(define (fail/enum e)
  (let ([t
         (λ (_)
            (raise e))])
    (enum 1
          t
          t)))

(define (error/enum . args)
  (define (fail n) (apply error args))
  (enum +inf.0 fail fail))

(define (unsupported/enum pat)
  (error/enum 'generate-term "#:i-th does not support ~s patterns" pat))


(module+
 test
 (require rackunit)
 (provide check-bijection?)
 (define confidence 1000)
 (define nums (build-list confidence identity))
 (define-simple-check (check-bijection? e)
   (let ([nums (build-list (if (<= (enum-size e) confidence)
                               (enum-size e)
                               confidence)
                           identity)])
     (andmap =
             nums
             (map (λ (n)
                     (encode e (decode e n)))
                  nums))))

 ;; const/enum tests
 (let ([e (const/enum 17)])
   (test-begin
    (check-eq? (decode e 0) 17)
    (check-exn exn:fail? 
               (λ ()
                  (decode e 1)))
    (check-eq? (encode e 17) 0)
    (check-exn exn:fail?
               (λ ()
                  (encode e 0)))
    (check-bijection? e)))

 ;; from-list/enum tests
 (let ([e (from-list/enum '(5 4 1 8))])
   (test-begin
    (check-eq? (decode e 0) 5)
    (check-eq? (decode e 3) 8)
    (check-exn exn:fail?
               (λ () (decode e 4)))
    (check-eq? (encode e 5) 0)
    (check-eq? (encode e 8) 3)
    (check-exn exn:fail?
               (λ ()
                  (encode e 17)))
    (check-bijection? e)))

 ;; map test
 (define nats+1 (nats+/enum 1))

 (test-begin
  (check-equal? (size nats+1) +inf.f)
  (check-equal? (decode nats+1 0) 1)
  (check-equal? (decode nats+1 1) 2)
  (check-bijection? nats+1))
 ;; encode check
 (test-begin
  (check-exn exn:fail?
             (λ ()
                (decode nats -1))))

 ;; ints checks
 (test-begin
  (check-eq? (decode ints 0) 0)         ; 0 -> 0
  (check-eq? (decode ints 1) 1)         ; 1 -> 1
  (check-eq? (decode ints 2) -1)        ; 2 -> 1
  (check-eq? (encode ints 0) 0)
  (check-eq? (encode ints 1) 1)
  (check-eq? (encode ints -1) 2)
  (check-bijection? ints))              ; -1 -> 2, -3 -> 4

 ;; sum tests
 (test-begin
  (let ([bool-or-num (sum/enum bools
                               (from-list/enum '(0 1 2)))]
        [bool-or-nat (sum/enum bools
                               nats)]
        [nat-or-bool (sum/enum nats
                               bools)]
        [odd-or-even (sum/enum evens
                               odds)])
    (check-equal? (enum-size bool-or-num)
                  5)
    (check-equal? (decode bool-or-num 0) #t)
    (check-equal? (decode bool-or-num 1) #f)
    (check-equal? (decode bool-or-num 2) 0)
    (check-exn exn:fail?
               (λ ()
                  (decode bool-or-num 5)))
    (check-equal? (encode bool-or-num #f) 1)
    (check-equal? (encode bool-or-num 2) 4)
    (check-bijection? bool-or-num)
     
    (check-equal? (enum-size bool-or-nat)
                  +inf.f)
    (check-equal? (decode bool-or-nat 0) #t)
    (check-equal? (decode bool-or-nat 2) 0)
    (check-bijection? bool-or-nat)
     
    (check-equal? (encode bool-or-num #f) 1)
    (check-equal? (encode bool-or-num 2) 4)

    (check-equal? (enum-size odd-or-even)
                  +inf.f)
    (check-equal? (decode odd-or-even 0) 0)
    (check-equal? (decode odd-or-even 1) 1)
    (check-equal? (decode odd-or-even 2) 2)
    (check-exn exn:fail?
               (λ ()
                  (decode odd-or-even -1)))
    (check-equal? (encode odd-or-even 0) 0)   
    (check-equal? (encode odd-or-even 1) 1)
    (check-equal? (encode odd-or-even 2) 2)
    (check-equal? (encode odd-or-even 3) 3)
    (check-bijection? odd-or-even)))

 ;; prod/enum tests
 (define bool*bool (prod/enum bools bools))
 (define 1*b (prod/enum (const/enum 1) bools))
 (define bool*nats (prod/enum bools nats))
 (define nats*bool (prod/enum nats bools))
 (define nats*nats (prod/enum nats nats))
 (define ns-equal? (λ (ns ms)
                      (and (= (car ns)
                              (car ms))
                           (= (cdr ns)
                              (cdr ms)))))

 ;; prod tests
 (test-begin

  (check-equal? (size 1*b) 2)
  (check-equal? (decode 1*b 0) (cons 1 #t))
  (check-equal? (decode 1*b 1) (cons 1 #f))
  (check-bijection? 1*b)
  (check-equal? (enum-size bool*bool) 4)
  (check-equal? (decode bool*bool 0)
                (cons #t #t))
  (check-equal? (decode bool*bool 1)
                (cons #t #f))
  (check-equal? (decode bool*bool 2)
                (cons #f #t))
  (check-equal? (decode bool*bool 3)
                (cons #f #f))
  (check-bijection? bool*bool)

  (check-equal? (enum-size bool*nats) +inf.f)
  (check-equal? (decode bool*nats 0)
                (cons #t 0))
  (check-equal? (decode bool*nats 1)
                (cons #f 0))
  (check-equal? (decode bool*nats 2)
                (cons #t 1))
  (check-equal? (decode bool*nats 3)
                (cons #f 1))
  (check-bijection? bool*nats)

  (check-equal? (enum-size nats*bool) +inf.f)
  (check-equal? (decode nats*bool 0)
                (cons 0 #t))
  (check-equal? (decode nats*bool 1)
                (cons 0 #f))
  (check-equal? (decode nats*bool 2)
                (cons 1 #t))
  (check-equal? (decode nats*bool 3)
                (cons 1 #f))
  (check-bijection? nats*bool)

  (check-equal? (enum-size nats*nats) +inf.f)
  (check ns-equal?
         (decode nats*nats 0)
         (cons 0 0))
  (check ns-equal?
         (decode nats*nats 1)
         (cons 0 1))
  (check ns-equal?
         (decode nats*nats 2)
         (cons 1 0))
  (check ns-equal?
         (decode nats*nats 3)
         (cons 0 2))
  (check ns-equal?
         (decode nats*nats 4)
         (cons 1 1))
  (check-bijection? nats*nats))


 ;; dep/enum tests
 (define (up-to n)
   (take/enum nats (+ n 1)))

 (define 3-up
   (dep/enum
    (from-list/enum '(0 1 2))
    up-to))

 (define from-3
   (dep/enum
    (from-list/enum '(0 1 2))
    nats+/enum))

 (define nats-to
   (dep/enum nats up-to))

 (define nats-up
   (dep/enum nats nats+/enum))

 (test-begin
  (check-equal? (size 3-up) 6)
  (check-equal? (decode 3-up 0) (cons 0 0))
  (check-equal? (decode 3-up 1) (cons 1 0))
  (check-equal? (decode 3-up 2) (cons 1 1))
  (check-equal? (decode 3-up 3) (cons 2 0))
  (check-equal? (decode 3-up 4) (cons 2 1))
  (check-equal? (decode 3-up 5) (cons 2 2))
  (check-bijection? 3-up)

  (check-equal? (size from-3) +inf.f)
  (check-equal? (decode from-3 0) (cons 0 0))
  (check-equal? (decode from-3 1) (cons 1 1))
  (check-equal? (decode from-3 2) (cons 2 2))
  (check-equal? (decode from-3 3) (cons 0 1))
  (check-equal? (decode from-3 4) (cons 1 2))
  (check-equal? (decode from-3 5) (cons 2 3))
  (check-equal? (decode from-3 6) (cons 0 2))
  (check-bijection? from-3)

  (check-equal? (size nats-to) +inf.f)
  (check-equal? (decode nats-to 0) (cons 0 0))
  (check-equal? (decode nats-to 1) (cons 1 0))
  (check-equal? (decode nats-to 2) (cons 1 1))
  (check-equal? (decode nats-to 3) (cons 2 0))
  (check-equal? (decode nats-to 4) (cons 2 1))
  (check-equal? (decode nats-to 5) (cons 2 2))
  (check-equal? (decode nats-to 6) (cons 3 0))
  (check-bijection? nats-to)

  (check-equal? (size nats-up) +inf.f)
  (check-equal? (decode nats-up 0) (cons 0 0))
  (check-equal? (decode nats-up 1) (cons 0 1))
  (check-equal? (decode nats-up 2) (cons 1 1))
  (check-equal? (decode nats-up 3) (cons 0 2))
  (check-equal? (decode nats-up 4) (cons 1 2))
  (check-equal? (decode nats-up 5) (cons 2 2))
  (check-equal? (decode nats-up 6) (cons 0 3))
  (check-equal? (decode nats-up 7) (cons 1 3))

  (check-bijection? nats-up))

 ;; dep2/enum tests
 ;; same as dep unless the right side is finite
 (define 3-up-2
   (dep2/enum
    (from-list/enum '(0 1 2))
    up-to))

 (define nats-to-2
   (dep2/enum nats up-to))


 (test-begin
  (check-equal? (size 3-up-2) 6)
  (check-equal? (decode 3-up-2 0) (cons 0 0))
  (check-equal? (decode 3-up-2 1) (cons 1 0))
  (check-equal? (decode 3-up-2 2) (cons 1 1))
  (check-equal? (decode 3-up-2 3) (cons 2 0))
  (check-equal? (decode 3-up-2 4) (cons 2 1))
  (check-equal? (decode 3-up-2 5) (cons 2 2))
  (check-bijection? 3-up-2)

  (check-equal? (size nats-to-2) +inf.f)
  (check-equal? (decode nats-to-2 0) (cons 0 0))
  (check-equal? (decode nats-to-2 1) (cons 1 0))
  (check-equal? (decode nats-to-2 2) (cons 1 1))
  (check-equal? (decode nats-to-2 3) (cons 2 0))
  (check-equal? (decode nats-to-2 4) (cons 2 1))
  (check-equal? (decode nats-to-2 5) (cons 2 2))
  (check-equal? (decode nats-to-2 6) (cons 3 0))
  (check-bijection? nats-to-2)
  )

 ;; take/enum test
 (define to-2 (up-to 2))
 (test-begin
  (check-equal? (size to-2) 3)
  (check-equal? (decode to-2 0) 0)
  (check-equal? (decode to-2 1) 1)
  (check-equal? (decode to-2 2) 2)
  (check-bijection? to-2))

 ;; to-list test
 (test-begin
  (check-equal? (to-list (up-to 3))
                '(0 1 2 3)))

 ;; except/enum test
 (define not-3 (except/enum nats '(3)))
 (test-begin
  (check-equal? (decode not-3 0) 0)
  (check-equal? (decode not-3 3) 4)
  (check-bijection? not-3))
 (define not-a (except/enum nats '(a)))
 (test-begin
  (check-equal? (decode not-a 0) 0)
  (check-bijection? not-a))

 ;; fold-enum tests
 (define complicated
   (fold-enum
    (λ (excepts n)
       (except/enum (up-to n) excepts))
    '(2 4 6)))
 (check-bijection? complicated))
