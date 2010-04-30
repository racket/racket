#lang scheme/base
(provide (all-defined-out))

(require scheme/require-syntax
         scheme/match
         scheme/gui/dynamic
	 typed-scheme/utils/utils
         (for-syntax scheme/base)
         (types comparison utils) 
         racunit racunit/text-ui)

(provide private typecheck (rename-out [infer r:infer]) utils env rep types)

(define (mk-suite ts)
  (match (map (lambda (f) (f)) ts)
	 [(list t) t]
	 [ts (make-test-suite "Combined Test Suite" ts)]))

(define (run . ts)
  (run-tests (mk-suite ts)))

(define (test/gui suite) 
  (((dynamic-require 'racunit/private/gui/gui 'make-gui-runner))
   suite))

(define (run/gui . ts)
  (test/gui (mk-suite ts)))


(define-syntax (define-go stx)
  (syntax-case stx ()
    [(_ args ...)
     (with-syntax 
      ([go (datum->syntax stx 'go)]
       [go/gui (datum->syntax stx 'go/gui)]
       [(tmps ...) (generate-temporaries #'(args ...))])
      #'(define-values (go go/gui)
	  (let ([tmps args] ...)
	    (values (lambda () (run tmps ...))
		    (lambda () (run/gui tmps ...))))))]))

;; FIXME - do something more intelligent
(define (tc-result-equal/test? a b)
  (equal? a b))

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm type-equal? a b))]))
(define-syntax (check-tc-result-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm tc-result-equal/test? a b))]))

