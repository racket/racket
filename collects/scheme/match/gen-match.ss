#lang scheme/base

(require "patterns.ss" "compiler.ss"
         syntax/stx
         (for-template scheme/base (only-in "patterns.ss" match:error)))

(provide go)

;; this parses the clauses using parse/cert, then compiles them
;; go : syntax syntax syntax certifier -> syntax
(define (go parse/cert stx exprs clauses cert)
  (parameterize ([orig-stx stx])
  (syntax-case clauses ()
    [([pats . rhs] ...)
     (let ([len (length (syntax->list exprs))])
       (with-syntax ([(xs ...) (generate-temporaries exprs)]
                     [(exprs ...) exprs]
                     [(fail) (generate-temporaries #'(fail))])
         (with-syntax ([body (compile* (syntax->list #'(xs ...))
                                       (map (lambda (pats rhs)
                                              (unless (= len (length (syntax->list pats)))
                                                (raise-syntax-error 'match 
                                                                    (format "wrong number of match clauses, expected ~a and got ~a" 
                                                                            len (length (syntax->list pats)))
                                                                    pats))
                                              (syntax-case* rhs (=>)
                                                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
                                                [((=> unm) . rhs)
                                                 (make-Row (map (lambda (s) (parse/cert s cert)) (syntax->list pats))
                                                           #`(begin . rhs) 
                                                           #'unm
                                                           null)]
                                                [_
                                                 (make-Row (map (lambda (s) (parse/cert s cert)) (syntax->list pats))
                                                           #`(begin . #,rhs)
                                                           #f
                                                           null)]))
                                            (syntax->list #'(pats ...))
                                            (syntax->list #'(rhs ...)))
                                       #'fail)]
                       [orig-expr (if (= 1 len) (stx-car #'(xs ...)) #'(list xs ...))])
           (quasisyntax/loc stx
             (let ([xs exprs]
                   ...)
               (let ([fail (lambda () #,(syntax/loc stx (match:error orig-expr)))])
                 body))))))])))