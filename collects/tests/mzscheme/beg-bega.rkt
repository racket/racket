
(htdp-syntax-test #'(local [(define x 5)] x))
(htdp-syntax-test #'(recur name ([x 18]) x))

(htdp-syntax-test #'(define (f78 a) (a))) ; no functions as arguments

;; See htdp-top uses in beg-adv.ss:
(htdp-error-test #'f)
(htdp-error-test #'(x 1))
(htdp-error-test #'(+ f 1))
(htdp-error-test #'((f 1)))
(htdp-error-test #'a1)
(htdp-error-test #'make-a1)
(htdp-error-test #'a1?)
(htdp-error-test #'a1-b)

(htdp-top (define (g w) (h w))) ;; h isn't defined, yet
(htdp-error-test #'1)
(htdp-top (define (h j) (add1 j)))
(htdp-test 3 'h (h 2))

(htdp-top (define (p j) (x j)))
(htdp-error-test #'1)
(htdp-top-pop 1)

(htdp-top (define (my-f x) (+ x 5)))
(htdp-syntax-test #'my-f #rx"a procedure, so it must be applied")
(htdp-top-pop 1)

;; Teachpacks with higher-order primitives
;;   Builds on tests in beg-adv.ss
(htdp-teachpack my-teachpack)

(htdp-top (define (my-f x) x))
(htdp-top (define-struct foo (a b)))

(htdp-syntax-test #'(go 5 8))
(htdp-syntax-test #'(go add1 add1))
(htdp-syntax-test #'(go my-f add1))
(htdp-syntax-test #'(go foo? add1))
(htdp-syntax-test #'(go make-foo add1))
(htdp-syntax-test #'(go foo-a add1))
(htdp-syntax-test #'(go go add1))

(htdp-top-pop 1)
(htdp-teachpack-pop)

(htdp-err/rt-test (+) exn:application:arity?)
(htdp-err/rt-test (+ 1) exn:application:arity?)
(htdp-err/rt-test (*) exn:application:arity?)
(htdp-err/rt-test (* 1) exn:application:arity?)
(htdp-err/rt-test (-) exn:application:arity?)
(htdp-err/rt-test (/) exn:application:arity?)
(htdp-err/rt-test (/ 1) exn:application:arity?)

(err/rt-test (+) exn:application:arity?)


