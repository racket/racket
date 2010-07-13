#lang racket/base
(require racket/contract
         racket/match
         datalog/ast
         datalog/stx)
(require (for-template datalog/stx))

(provide/contract
 [compile-program (program/c . -> . (listof syntax?))]
 [compile-statement (statement/c . -> . syntax?)])

(define (compile-program p)
  (map compile-statement p))

(define compile-statement
  (match-lambda
    [(assertion srcloc c)
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (! #,(compile-clause c)))]
    [(retraction srcloc c)
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (~ #,(compile-clause c)))]
    [(query srcloc l)
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (? #,(compile-literal l)))]))

(define compile-clause
  (match-lambda
    [(clause srcloc head (list))
     (define srcstx (datum->syntax #f 'x srcloc))
     (compile-literal head)]
    [(clause srcloc head body)
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (:- #,@(map compile-literal (list* head body))))]))

(define compile-literal
  (match-lambda
    [(literal srcloc '= (and ts (app length 2)))
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (= #,@(map compile-term ts)))]
    [(literal srcloc pred ts)
     (define srcstx (datum->syntax #f 'x srcloc))
     (quasisyntax/loc srcstx
       (#,pred #,@(map compile-term ts)))]))

(define compile-term
  (match-lambda
    [(variable srcloc sym)
     (datum->syntax #f sym srcloc)]
    [(constant srcloc sym)
     (datum->syntax #f sym srcloc)]))
