#lang scheme

(require "through-tests.ss" 
         "test-engine.ss")

(define steve-broke '(mz1 map))
(define lazy-tests 
  '(lazy1 lazy2 lazy3 lazy-multi lazy-app1 lazy-app2 lazy-app3 
    lazy-cons1 lazy-cons2 lazy-list1 lazy-list2 lazy-list3 lazy-list4 lazy-list5
    lazy-caar lazy-cadr lazy-cdar lazy-cddr lazy-caaar lazy-caadr lazy-cadar
    lazy-caddr lazy-cdaar lazy-cdadr lazy-cddar lazy-cdddr lazy-caaaar 
    lazy-caaadr lazy-caadar lazy-caaddr lazy-cadaar lazy-cadadr lazy-caddar
    lazy-cadddr lazy-cdaaar lazy-cdaadr lazy-cdadar lazy-cdaddr lazy-cddaar
    lazy-cddadr lazy-cdddar lazy-cddddr lazy-second lazy-third lazy-fourth
    lazy-fifth lazy-sixth lazy-seventh lazy-eighth))

(let ((outer-namespace (current-namespace)))
  (parameterize ([display-only-errors #t]
                 ;; display-only-errors is insufficient, because the evals
                 ;; actually cause output.  So we just eat stdout.
                 [current-output-port (open-output-string)]
		 [current-namespace (make-base-namespace)])
    ;; make sure the tests' print-convert sees the teaching languages' properties
    #;(namespace-attach-module outer-namespace 'mzlib/pconvert-prop (current-namespace))
    (namespace-require 'test-engine/racket-tests)
    (if (and #;(run-all-tests-except 
              (append '(bad-and bad-cons check-error begin-let-bug prims qq-splice time 
                                set! local-set! local-struct/i local-struct/ilam)
                      steve-broke
                      lazy-tests))
             (run-tests lazy-tests))
	(exit 0)
	(exit 1))))
