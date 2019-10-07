#lang racket/base
(require "wrap.rkt")

(provide recognize-inferred-names)

;; Startup code as an S-expression uses the pattern
;;   (lambda <formals> (begin '<id> <expr>))
;; or
;;   (case-lambda [<formals> (begin '<id> <expr>)] <clause> ...)
;; to record a name for a function. Detect that pattern and
;; convert back to an 'inferred-name property. We rely on the fact
;; that the names `lambda`, `case-lambda`, and `quote` are
;; never shadowed, so we don't have to parse expression forms
;; in general.

(define (recognize-inferred-names e)
  (cond
    [(not (pair? e)) e]
    [else
     (define (begin-name e)
       (and (pair? e)
            (eq? (car e) 'begin)
            (pair? (cdr e))
            (pair? (cddr e))
            (pair? (cadr e))
            (eq? 'quote (caadr e))
            (cadadr e)))
     (case (car e)
       [(quote) e]
       [(lambda)
        (define new-e (map recognize-inferred-names e))
        (define name (begin-name (caddr e)))
        (if name
            (wrap-property-set new-e 'inferred-name name)
            new-e)]
       [(case-lambda)
        (define new-e (map recognize-inferred-names e))
        (define name (and (pair? (cdr e))
                          (begin-name (cadadr e))))
        (if name
            (wrap-property-set new-e 'inferred-name name)
            new-e)]
       [else (cons (recognize-inferred-names (car e))
                   (recognize-inferred-names (cdr e)))])]))
