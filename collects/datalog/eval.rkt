#lang racket/base
(require racket/list
         racket/contract
         "ast.rkt"
         "pretty.rkt"
         "runtime.rkt")

(define current-theory (make-parameter (make-theory)))

(define (assume-if-safe assume thy s)
  (let ([c (assertion-clause s)])
    (if (safe-clause? c)
        (assume thy c)
        (raise-syntax-error 'datalog
                            "Unsafe clause in assertion"
                            (datum->syntax #f (format-statement s) (assertion-srcloc s))))))

(define (print-questions ls)
  (displayln
   (format-questions ls)))

(define (eval-program p)
  (for-each eval-top-level-statement p))

(define (eval-top-level-statement s)
  (define v (eval-statement s))
  (unless (void? v)
    (print-questions v)))

(define (eval-statement s)
  (cond
    [(assertion? s)
     (assume-if-safe assume! (current-theory) s)]
    [(retraction? s)
     (retract! (current-theory) (retraction-clause s))]
    [(query? s)
     (prove (current-theory) (query-question s))]))

(provide/contract
 [current-theory (parameter/c theory/c)]
 [print-questions ((listof question/c) . -> . void)]
 [eval-program (program/c . -> . void)]
 [eval-top-level-statement (statement/c . -> . void)]
 [eval-statement (statement/c . -> . (or/c void (listof question/c)))])
