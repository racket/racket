#lang scheme


;; run shared.rkt unit tests:
(require "shared-unit-tests.rkt")

;; now, the rest:
(require "through-tests.rkt" 
         "test-engine.rkt")

(define lazy-tests 
  '(lazy1 lazy2 lazy3 lazy-multi lazy-app1 lazy-app2 lazy-app3 
    lazy-cons1 lazy-cons2 lazy-list1 lazy-list2 lazy-list3 lazy-list4 lazy-list5
    lazy-caar lazy-cadr lazy-cdar lazy-cddr lazy-caaar lazy-caadr lazy-cadar
    lazy-caddr lazy-cdaar lazy-cdadr lazy-cddar lazy-cdddr lazy-caaaar 
    lazy-caaadr lazy-caadar lazy-caaddr lazy-cadaar lazy-cadadr lazy-caddar
    lazy-cadddr lazy-cdaaar lazy-cdaadr lazy-cdadar lazy-cdaddr lazy-cddaar
    lazy-cddadr lazy-cdddar lazy-cddddr lazy-second lazy-third lazy-fourth
    lazy-fifth lazy-sixth lazy-seventh lazy-eighth
    lazy-if1 lazy-if2 lazy-take-0 lazy-take lazy-take-impl
    lazy-unknown1 lazy-unknown2 lazy-inf-list1 lazy-cond1 lazy-cond2 lazy-cond3
    lazy-eq? lazy-eqv? lazy-equal? lazy-list?1 lazy-list?2 lazy-list?3
    lazy-length lazy-list-ref lazy-list-tail lazy-append lazy-reverse lazy-empty? 
    lazy-assoc lazy-assq lazy-assv lazy-cons? lazy-remove lazy-remq lazy-remv
    lazy-member lazy-memq lazy-memv lazy-filter1 lazy-filter2 lazy-fold
    lazy-cyclic1 lazy-fn-app))

(let ((outer-namespace (current-namespace)))
  (parameterize ([display-only-errors #t]
                 ;; display-only-errors is insufficient, because the evals
                 ;; actually cause output.  So we just eat stdout.
                 [current-output-port (open-output-string)]
		 [current-namespace (make-base-namespace)])
    ;; make sure the tests' print-convert sees the teaching languages' properties
    #;(namespace-attach-module outer-namespace 'mzlib/pconvert-prop (current-namespace))
    (namespace-require 'test-engine/racket-tests)
    (if (and (run-all-tests-except 
              (append '(bad-and check-error begin-let-bug prims qq-splice time 
                                set! local-set! local-struct/i local-struct/ilam)
                      lazy-tests))
             (run-tests lazy-tests))
	(exit 0)
	(exit 1))))
