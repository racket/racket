#lang typed/racket

(require racket/list)

(define-type expression 'expression)


(: foo (expression -> expression))
(define (foo expr)
  (define-values (a b)
    (for/fold: : (values (Listof Symbol) expression)
               ((remaining-funs : (Listof Symbol) empty)
                (body : expression 'expression))
               ((fun : Symbol empty))
               (if (empty? (filter even? '(1 2))) ;non-trival list
                   (values remaining-funs body)
                   (values (cons fun remaining-funs body)))))

  ;error is on line above. paren should be moved before body
  (error 'dont-care))
