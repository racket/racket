#lang racket/base
(require racket/contract
         racket/match
         datalog/ast
         (only-in datalog/sexp/lang
                  ? :- ! ~))
(require (for-template datalog/sexp/lang))

(provide/contract
 [compile-program (program/c . -> . syntax?)]
 [compile-statement (statement/c . -> . syntax?)])

(define (compile-program p)
  (quasisyntax
   (#%module-begin #,@(map compile-statement p))))

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
