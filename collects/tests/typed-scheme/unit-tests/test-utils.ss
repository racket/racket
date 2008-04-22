#lang scheme/base
(provide (all-defined-out))

(require scheme/require-syntax
         scheme/match
         (for-syntax scheme/base))

(define-require-syntax private
  (lambda (stx)
    (syntax-case stx ()
      [(_ id ...)
       (andmap identifier? (syntax->list #'(id ...)))
       (with-syntax ([(id* ...) (map (lambda (id) (datum->syntax 
                                                   id 
                                                   (string->symbol 
                                                    (string-append 
                                                     "typed-scheme/private/" 
                                                     (symbol->string (syntax-e id))))))
                                     (syntax->list #'(id ...)))])
       #`(combine-in id* ...))])))

(require (private planet-requires type-comparison utils))

(require (schemeunit))

(define (mk-suite ts)
  (match (map (lambda (f) (f)) ts)
	 [(list t) t]
	 [ts (apply test-suite "Combined Test Suite" ts)]))

(define (run . ts)
  (test/text-ui (mk-suite ts)))

(define (run/gui . ts)
  (test/graphical-ui (mk-suite ts)))


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

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     #`(test-check nm type-equal? a b)]))
(define-syntax (check-tc-result-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     #`(test-check nm tc-result-equal? a b)]))

