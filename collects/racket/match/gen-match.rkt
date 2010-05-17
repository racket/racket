#lang scheme/base

(require "patterns.rkt" "compiler.rkt"
         syntax/stx scheme/nest syntax/parse
         (for-template scheme/base (only-in "runtime.rkt" match:error)))

(provide go go/one)

;; this transforms `match'-style clauses into ones acceptable to `go'
;; go : syntax syntax syntax [certifier] -> syntax
(define (go/one parse/cert stx expr clauses [cert (syntax-local-certifier)])
  (syntax-parse clauses
    [([p . rhs] ...) 
     (go parse/cert stx (quasisyntax/loc expr (#,expr))
         #'([(p) . rhs] ...) cert)]))

;; this parses the clauses using parse/cert, then compiles them
;; go : syntax syntax syntax [certifier] -> syntax
(define (go parse/cert stx exprs clauses [cert (syntax-local-certifier)])
  (syntax-case clauses ()
    [([pats . rhs] ...)
     (nest
       ([parameterize ([orig-stx stx])]
        [begin (unless (syntax->list exprs)
                 (raise-syntax-error
                  'match*
                  "expected a sequence of expressions to match"
                  exprs))]
        [let ([len (length (syntax->list exprs))])]
        [with-syntax ([(xs ...) (generate-temporaries exprs)]
                      [(exprs ...) exprs]
                      [(fail) (generate-temporaries #'(fail))])]      
        [with-syntax  
            ([body 
              (compile*
               (syntax->list #'(xs ...))
               (for/list ([clause (syntax->list clauses)]
                          [pats (syntax->list #'(pats ...))]
                          [rhs (syntax->list #'(rhs ...))])
		 (unless (syntax->list pats)
		   (raise-syntax-error 
		    'match*
		    "expected a sequence of patterns"
		    pats))
                 (let ([lp (length (syntax->list pats))])
                   (when (null? (syntax->list rhs))
                     (raise-syntax-error 
                      'match
                      "expected at least one expression on the right-hand side"
                      clause))
                   (unless (= len lp)
                     (raise-syntax-error
                      'match
                      (format 
                       "wrong number of match clauses, expected ~a and got ~a"
                       len lp)
                      pats))
                   (let ([mk (lambda (unm rhs)
                               (make-Row (for/list ([p (syntax->list pats)])
                                           (parse/cert p cert))
                                         #`(begin . #,rhs) unm null))])
                     (syntax-case* rhs (=>)
                       (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
                       [((=> unm) . rhs) (mk #'unm #'rhs)]
                       [_ (mk #f rhs)]))))
               #'fail)]
             [orig-expr 
              (if (= 1 len) (stx-car #'(xs ...)) #'(list xs ...))])])
       (quasisyntax/loc stx
         (let ([xs exprs] ...)
           (let ([fail (lambda ()
                         #,(syntax/loc stx (match:error orig-expr)))])
             body))))]))
