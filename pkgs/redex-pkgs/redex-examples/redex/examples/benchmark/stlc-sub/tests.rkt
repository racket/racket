#lang racket/base
(require "stlc-sub-base.rkt" 
         "../stlc/tests-lib.rkt"
         redex/reduction-semantics
         racket/runtime-path)
(stlc-tests uses-bound-var?
            typeof
            red
            reduction-step-count
            Eval
            subst)

(test-equal (term (βv-> ((λ (x int) x) 1)))
            (term 1))
(test-equal (term (βv-> (((λ (x (int → int)) x) (λ (x int) x)) 1)))
            (term ((λ (x int) x) 1)))
(test-equal (term (βv-> ((+ ((λ (x int) x) 1)) ((λ (y int) y) 2))))
            (term ((+ 1) 2)))
(test-equal (term (βv-> (λ (y int) ((λ (x int) x) y))))
            (term (λ (y int) y)))
(test-equal (check (term ((λ (x int) x) 1))) #t)
(test-equal (check (term (hd ((cons 1) 2)))) #t)

(define-runtime-path here ".")
(for ([f (in-list (directory-list here))])
  (when (regexp-match #rx"^stlc-sub-[0-9]+[.]rkt$" (path->string f))
    (define small-counter-example (dynamic-require (build-path here f) 'small-counter-example))
    (unless (check small-counter-example)
      (error 'tests.rkt "the counterexample in ~a also fails check for stlc-sub-base.rkt"
             f))))

(test-results)
