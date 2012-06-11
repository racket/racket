
(htdp-err/rt-test (/) "/: expects at least 2 arguments, but found none")
(htdp-err/rt-test (+ 1) #rx"^[+]: expects at least 2 arguments, but found only 1$")

(htdp-top (define (f x) x))
(htdp-syntax-test #'(f 1 2) "f: expects only 1 argument, but found 2")
(htdp-top-pop 1)

(htdp-syntax-test #'(local [(define x 5)] x) "local: this function is not defined")
(htdp-syntax-test #'(recur name ([x 18]) x) "recur: this function is not defined")

(htdp-syntax-test #'(define (f78 a) (a)) "function call: expected a function after the open parenthesis, but found a variable")

;; See htdp-top uses in beg-adv.rkt:
(htdp-error-test #'f)
(htdp-error-test #'(x 1))
(htdp-error-test #'(+ f 1))
(htdp-error-test #'((f 1)) )
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
(htdp-syntax-test #'my-f #rx"expected a function call, but there is no open parenthesis before this function")
(htdp-top-pop 1)

;; Teachpacks with higher-order primitives
;;   Builds on tests in beg-adv.rkt
(htdp-teachpack my-teachpack)

(htdp-top (define (my-f x) x))
(htdp-top (define-struct foo (a b)))

(htdp-syntax-test #'(go 5 8) "go: expects a function in this position\n  at: 8\n  in: (go 5 8)")
(htdp-syntax-test #'(go add1 add1) "add1: expected a function call, but there is no open parenthesis before this function")
(htdp-syntax-test #'(go my-f add1) "my-f: expected a function call, but there is no open parenthesis before this function")
(htdp-syntax-test #'(go foo? add1) "foo?: expected a function call, but there is no open parenthesis before this function")
(htdp-syntax-test #'(go make-foo add1) "make-foo: expected a function call, but there is no open parenthesis before this function")
(htdp-syntax-test #'(go foo-a add1) "foo-a: expected a function call, but there is no open parenthesis before this function")
(htdp-syntax-test #'(go go add1) "go: expected a function call, but there is no open parenthesis before this function")

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


