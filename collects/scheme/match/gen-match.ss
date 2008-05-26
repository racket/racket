#lang scheme/base

(require "patterns.ss" "compiler.ss"
         syntax/stx scheme/nest
         (for-template scheme/base (only-in "runtime.ss" match:error)))

(provide go)

;; this parses the clauses using parse/cert, then compiles them
;; go : syntax syntax syntax certifier -> syntax
(define (go parse/cert stx exprs clauses cert)
  (syntax-case clauses ()
    [([pats . rhs] ...)
     (nest
       ([parameterize ([orig-stx stx])]
        [let ([len (length (syntax->list exprs))])]
        [with-syntax ([(xs ...) (generate-temporaries exprs)]
                      [(exprs ...) exprs]
                      [(fail) (generate-temporaries #'(fail))])]      
        [with-syntax  
            ([body 
              (compile*
               (syntax->list #'(xs ...))
               (for/list ([pats (syntax->list #'(pats ...))]
                          [rhs (syntax->list #'(rhs ...))])
                 (let ([lp (length (syntax->list pats))])
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
