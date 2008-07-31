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
                                                     (symbol->string (syntax-e id))))
                                                   id id))
                                     (syntax->list #'(id ...)))])
         (syntax/loc stx (combine-in id* ...)))])))

(require (private planet-requires type-comparison utils type-utils))

(require (schemeunit))

(define (mk-suite ts)
  (match (map (lambda (f) (f)) ts)
	 [(list t) t]
	 [ts (apply test-suite "Combined Test Suite" ts)]))

(define (run . ts)
  (test/text-ui (mk-suite ts)))

(define (test/gui suite) ((dynamic-require '(planet schematics/schemeunit:2/graphical-ui) 'test/graphical-ui) suite))

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

;; FIXME - check that effects are equal
(define (tc-result-equal/test? a b)
  (match* (a b)
          [((tc-result: t1 thn1 els1) (tc-result: t2 thn2 els2))
           (and (type-equal? t1 t2)
                (= (length thn1) (length thn2))
                (= (length els1) (length els2)))]))

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm type-equal? a b))]))
(define-syntax (check-tc-result-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm tc-result-equal/test? a b))]))

