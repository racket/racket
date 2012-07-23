#lang typed/racket

(require math/flonum racket/flonum
         math/bigfloat
         math/special-functions)

(bf-precision 53)

(define regular-test-num 10000)

(define special-tests (list +nan.0 -inf.0 -max.0 -0.0 0.0 +max.0 +inf.0))

(define min-ordinal (flonum->ordinal +min.0))
(define max-ordinal (flonum->ordinal +max.0))

(define 1d-positive-test-num (quotient regular-test-num 2))
(define 1d-positive-ordinals (in-range min-ordinal max-ordinal
                                       (quotient max-ordinal 1d-positive-test-num)))
(define 1d-positive-tests (map ordinal->flonum (sequence->list 1d-positive-ordinals)))
(define 1d-negative-tests (map (λ: ([x : Float]) (fl* -1.0 x)) 1d-positive-tests))
(define 1d-tests (append special-tests 1d-negative-tests 1d-positive-tests))

(: test-1d-fun (Symbol (Float -> Float) (Bigfloat -> Bigfloat) -> Void))
(define (test-1d-fun name flfun bffun)
  (printf "Testing ~a: " name)
  (for ([x  (in-list 1d-tests)] [j  (in-naturals)])
    (when (zero? (modulo j 500))
      (printf "*")
      (flush-output (current-output-port)))
    ;(printf "x = ~v~n" x)
    (define y0 (flfun x))
    (define y1 (bigfloat->flonum (bffun (flonum->bigfloat x))))
    (define err (flulp-error y0 y1))
    (when (err . > . 2.0)
      (printf "~n(~a ~v)~n(flulp-error ~v ~v) = ~v~n" name x y0 y1 err)))
  (printf "~n~n"))

(define 2d-positive-test-num (quotient (exact-round (sqrt regular-test-num)) 2))
(define 2d-positive-ordinals (in-range min-ordinal max-ordinal
                                       (quotient max-ordinal 2d-positive-test-num)))
(define 2d-positive-tests (map ordinal->flonum (sequence->list 2d-positive-ordinals)))
(define 2d-negative-tests (map (λ: ([x : Float]) (fl* -1.0 x)) 2d-positive-tests))
(define 2d-tests (append special-tests 2d-negative-tests 2d-positive-tests))

(: test-2d-fun (Symbol (Float Float -> Float) (Bigfloat Bigfloat -> Bigfloat) -> Void))
(define (test-2d-fun name flfun bffun)
  (printf "Testing ~a: " name)
  (define j 0)
  (for*: ([x  (in-list 2d-tests)] [y  (in-list 2d-tests)])
    (when (zero? (modulo j 500))
      (printf "*")
      (flush-output (current-output-port)))
    (define z0 (flfun x y))
    (define z1 (bigfloat->flonum (bffun (flonum->bigfloat x) (flonum->bigfloat y))))
    (define err (flulp-error z0 z1))
    (when (err . > . 1.0)
      (printf "~n(~a ~v ~v)~n(flulp-error ~v ~v) = ~v~n" name x y z0 z1 err))
    (set! j (+ j 1)))
  (printf "~n~n"))

(test-1d-fun 'fllog fllog bflog)
(test-1d-fun 'flexp flexp bfexp)
(test-1d-fun 'flsqrt flsqrt bfsqrt)
(test-1d-fun 'flsin flsin bfsin)
(test-1d-fun 'flcos flcos bfcos)
(test-1d-fun 'fltan fltan bftan)
(test-1d-fun 'flasin flasin bfasin)
(test-1d-fun 'flacos flacos bfacos)
(test-1d-fun 'flatan flatan bfatan)

(test-2d-fun 'fl+ fl+ bf+)
(test-2d-fun 'fl- fl- bf-)
(test-2d-fun 'fl* fl* bf*)
(test-2d-fun 'fl/ fl/ bf/)
(test-2d-fun 'flexpt flexpt bfexpt)
(test-2d-fun 'atan atan bfatan2)

(test-1d-fun 'flgamma flgamma bfgamma)
(test-1d-fun 'fllog-gamma fllog-gamma bflog-gamma)

#|
(: flulp-error-near ((Float -> Float) (Bigfloat -> Bigfloat) Float -> Float))
(define (flulp-error-near flfun bffun x)
  (define errs
    (for/list: : (Listof (Pair Float Float)) ([dx  (in-range -128 129)])
      (define y (flstep x dx))
      (define z0 (flfun y))
      (define z1 (bigfloat->float (bffun (float->bigfloat y))))
      (define w (flexp (- (/ (abs (real->double-flonum dx)) 10.0))))
      (cons (* w (flulp-error z0 z1)) w)))
  (/ (apply + (map (inst car Float Float) errs))
     (apply + (map (inst cdr Float Float) errs))))

(: dx-grow (Integer -> Integer))
(define (dx-grow dx)
  (quotient (* (+ dx 1) 3) 2))

(: dx-shrink (Integer -> Integer))
(define (dx-shrink dx)
  (quotient dx 2))

(: test-near ((Float -> Float) (Bigfloat -> Bigfloat) Float -> (Listof (Pair Float Float))))
(define (test-near flfun bffun x)
  (let loop ([x x] [last-x x] [dx 1] [#{bad-xs : (Listof (Pair Float Float))} null])
    (define err (flulp-error-near flfun bffun x))
    (printf "x = ~v  dx = ~v  err = ~v~n" x dx err)
    (cond [(and (rational? err) (err . >= . 2.0))
           (loop (flstep x dx) x (dx-grow dx) (list* (cons x err) bad-xs))]
          [(<= dx 6)
           (remove-duplicates (reverse bad-xs))]
          [else
           (loop last-x last-x (dx-shrink dx) bad-xs)])))
|#
