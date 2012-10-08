#lang racket/base

(require "../utils/utils.rkt"
         (rep free-variance type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         racket/match
         (contract-req))

;; this structure represents the result of typechecking an expression
(define-struct/cond-contract tc-result
  ([t Type/c] [f FilterSet/c] [o Object?])
  #:transparent)
(define-struct/cond-contract tc-results
  ([ts (listof tc-result?)] [drest (or/c (cons/c Type/c symbol?) #f)])
  #:transparent)

(define-match-expander tc-result:
  (syntax-rules ()
   [(_ tp fp op) (struct tc-result (tp fp op))]
   [(_ tp) (struct tc-result (tp _ _))]))

(define-match-expander tc-results:
  (syntax-rules ()
   [(_ tp fp op)
    (struct tc-results ((list (struct tc-result (tp fp op)) (... ...))
                          #f))]
   [(_ tp fp op dty dbound)
    (struct tc-results ((list (struct tc-result (tp fp op)) (... ...))
                          (cons dty dbound)))]
   [(_ tp)
    (struct tc-results ((list (struct tc-result (tp _ _)) (... ...))
                          #f))]))

(define-match-expander tc-result1:
  (syntax-rules ()
   [(_ tp fp op) (struct tc-results ((list (struct tc-result (tp fp op)))
                                       #f))]
   [(_ tp) (struct tc-results ((list (struct tc-result (tp _ _)))
                                 #f))]))

(define (tc-results-t tc)
  (match tc
    [(tc-results: t) t]))

(define-match-expander Result1:
  (syntax-rules ()
   [(_ tp) (Values: (list (Result: tp _ _)))]
   [(_ tp fp op) (Values: (list (Result: tp fp op)))]))

(define-match-expander Results:
  (syntax-rules ()
   [(_ tp) (Values: (list (Result: tp _ _) (... ...)))]
   [(_ tp fp op) (Values: (list (Result: tp fp op) (... ...)))]))

;; convenience function for returning the result of typechecking an expression
(define ret
  (case-lambda [(t)
                (let ([mk (lambda (t) (make-FilterSet (make-Top) (make-Top)))])
                  (make-tc-results
                   (cond [(Type? t)
                          (list (make-tc-result t (mk t) (make-Empty)))]
                         [else
                          (for/list ([i t])
                            (make-tc-result i (mk t) (make-Empty)))])
                   #f))]
               [(t f)
                (make-tc-results
                 (if (Type? t)
                     (list (make-tc-result t f (make-Empty)))
                     (for/list ([i t] [f f])
                               (make-tc-result i f (make-Empty))))
                 #f)]
               [(t f o)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map make-tc-result t f o)
                     (list (make-tc-result t f o)))
                 #f)]
               [(t f o dty)
                (int-err "ret used with dty without dbound")]
               [(t f o dty dbound)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map make-tc-result t f o)
                     (list (make-tc-result t f o)))
                 (cons dty dbound))]))

;(trace ret)

(provide/cond-contract
 [ret
  (->i ([t (or/c Type/c (listof Type/c))])
       ([f (t) (if (list? t)
                   (listof FilterSet/c)
                   FilterSet/c)]
        [o (t) (if (list? t)
                   (listof Object?)
                   Object?)]
        [dty Type/c]
        [dbound symbol?])
       [res tc-results?])])

(define (combine-results tcs)
  (match tcs
    [(list (tc-result1: t f o) ...)
     (ret t f o)]))

(define tc-result-equal? equal?)

(provide tc-result: tc-results: tc-result1: Result1: Results:)
(provide/cond-contract
 [combine-results ((listof tc-results?) . -> . tc-results?)]
 [tc-result? (any/c . -> . boolean?)]
 [tc-result-t (tc-result? . -> . Type/c)]
 [tc-result-equal? (tc-result? tc-result? . -> . boolean?)]
 [tc-results? (any/c . -> . boolean?)])
