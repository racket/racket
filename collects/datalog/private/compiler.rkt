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
     (define pred-stx (if (predicate-sym? pred)
                          (sym->original-syntax (predicate-sym-sym pred)
                                                (predicate-sym-srcloc pred))
                          pred))
     (quasisyntax/loc srcstx
       (#,pred-stx #,@(map compile-term ts)))]))

(define compile-term
  (match-lambda
    [(variable srcloc sym)
     (sym->original-syntax sym srcloc)]
    [(constant srcloc sym)
     (datum->syntax #f sym srcloc)]))

(define (sym->original-syntax sym srcloc)
  (define p (open-input-string (symbol->string sym)))
  (port-count-lines! p)
  (match-define (list source-name line column position span) srcloc)
  (set-port-next-location! p line column position)
  (read-syntax source-name p))
