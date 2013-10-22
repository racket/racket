#lang racket/base

(require
  rackunit
  racket/sandbox
  racket/flonum racket/fixnum racket/unsafe/ops
  racket/math
  syntax/srcloc
  (for-syntax
    racket/base
    syntax/parse))

(provide tests)


(define (mk-eval lang)
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-memory-limit 300])
       (make-evaluator lang)))))
(define racket-eval (mk-eval 'racket))
(define tr-eval     (mk-eval 'typed/racket))

(define-syntax bad-opt
  (syntax-parser
    [(_ exp:expr)
     #`(test-case #,(format "~a" (syntax->datum #'exp))
         (define r-value (racket-eval #'exp))
         (define tr-value (tr-eval #'exp))
         (with-check-info (['r-value r-value]
                           ['tr-value tr-value]
                           ['location (build-source-location-list (quote-syntax exp))])
           (when (equal? r-value tr-value)
             (fail-check "Known bug no longer exists."))))]))

;; TODO add this as a test
;; type-before = Single-Flonum-Complex
;; type-after = Float-Complex
;; redex-check: <collects>/tests/typed-racket/tr-random-testing.rkt:258
;; counterexample found after 499 attempts:
;(log (make-rectangular -inf.f -inf.f))

(define tests
  (test-suite "Known bugs"
    (bad-opt (/ (cosh (tanh (min 0)))
                (sqr (make-polar 9.8813129168249e-324 5.64234687795904e-184))))
    (bad-opt (/ 1.0 (sqr 9.8813129168249e-324+0.0i)))
    (bad-opt (/ 1.0 0.0+0.0i))

    (bad-opt
      (/
       (*
        (max 3/11 3/2 -0.0f0)
        (unsafe-fl* (real->double-flonum 0.0034637533f0) (real->double-flonum 2)))
       (*
        0.47415978f0
        (max (exact-round 0) (exact-round 2) (exact-round 1.3396765f0))
        (make-rectangular +inf.0 -2.6138428f-05))))

    (bad-opt (- (/ (ceiling 1.4821969375237e-323) (make-rectangular 1/3 -inf.0))))

    (bad-opt
      (+
       (make-rectangular 2.8794524657050558e-173 -26.76011f0)
       (max (sub1 (exact-round -0.0)))
       (*
        (fltruncate (real->double-flonum 3))
        (abs (real->double-flonum -9.194802638300139))
        (real->double-flonum +inf.f))))

    (bad-opt
      (/
       (* (fltruncate (real->double-flonum -0.0961751594300051)))
       (make-polar
        (unsafe-flmin (real->double-flonum 0) (real->double-flonum 9))
        (-
         (real->double-flonum 1.797693134862315e+308)
         (real->double-flonum 4.557755020079188e-100)
         (real->double-flonum -1.7568234f0)))))

    (bad-opt (/ (make-rectangular (+ (real->double-flonum 7/12)) 1.7976931348623157e+308)))


    (bad-opt
      (+
       (truncate
        (bitwise-xor
         (exact-round 3.4584595208887e-323)
         (exact-round 41.25529f0)
         (exact-round 5)))
       (abs (exact-round 1.0117665851393622))
       (/
        (make-rectangular -inf.0 -1.7976931348623157e+308)
        (+ -22024056634.94161 -3.141574f0 0.0))))

    (bad-opt
      (/
       (make-rectangular
        (max (real->double-flonum +inf.f) (real->double-flonum 2))
        (min (real->double-flonum 9) (real->double-flonum 1)))
       -23.447956f0))

    (bad-opt
      (-
       (* 6/11 (/ 1.2125638f0 142778.6f0))
       (sqr (* -4))
       (make-rectangular
        (unsafe-fl* (real->double-flonum 1) (real->double-flonum 0))
        (min (real->double-flonum -0.31099018f0)))))

    (bad-opt
      (/
        (+ (sqr 6.1239376f0) (make-rectangular -inf.f -2.4703282292062e-323))
        (max -1/8)))

    (bad-opt
      (-
       (* -1 (+ 2 -1.863321008013001) (/ +nan.0 -3 -0.2859221f0))
       (bitwise-xor
        (exact-round -2.5171401615688167e-250)
        (exact-round 9/5)
        (add1 (exact-round 0)))
       (sinh (/ (make-rectangular 1.7976931348623155e+308 -13/21) 2))))

    (bad-opt
      (/ (cosh (make-rectangular +inf.0 -1.7082773f0)) 8))

    (bad-opt
      (/
       (make-rectangular
        -1.0020975423559487e+94
        (*
         (real->double-flonum 9.8813129168249e-324)
         (real->double-flonum -5/7)
         (real->double-flonum -3)))))

    (bad-opt (sub1 (/ (make-rectangular -5.5945454f0 -inf.0))))

    (bad-opt
      (*
       (make-rectangular -inf.0 2.360318017838172)
       (* 1 (max (exact-round 1)))
       19/13))

    (bad-opt (/ (make-rectangular 1 +inf.0)))
    (bad-opt (/ (make-rectangular 1.0 +inf.0)))

    (bad-opt (+ (exp 1.7976931348623151e+308) 0.0+0.0i))

    (bad-opt (* (expt 10 500) (expt 10 -500) 1.0+1.0i))

    (bad-opt (+ (expt 10 501) (expt -10 501) 1.0+1.0i))



    (bad-opt (- (+ 0 0) 0.0+0.0i))))

(module+ main
  (require rackunit/text-ui)
  (void (run-tests tests)))
