#lang racket
(require racket/list
         "ast.rkt"
         "pretty.rkt"
         "runtime.rkt")

(define current-theory (make-parameter (make-mutable-theory)))

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

(define (eval-program/fresh p)
  (let loop ([thy (make-immutable-theory)]
             [p p])
    (if (empty? p)
        thy
        (let ([s (first p)])
          (loop
           (cond
             [(assertion? s)
              (assume-if-safe assume thy s)]
             [(retraction? s)
              (retract thy (retraction-clause s))]
             [(query? s)
              (print-questions (prove thy (query-question s)))
              thy])
           (rest p))))))

(provide/contract
 [current-theory (parameter/c mutable-theory/c)]
 [print-questions ((listof question/c) . -> . void)]
 [eval-program (program/c . -> . void)]
 [eval-top-level-statement (statement/c . -> . void)]
 [eval-statement (statement/c . -> . (or/c void (listof question/c)))]
 [eval-program/fresh (program/c . -> . immutable-theory/c)])