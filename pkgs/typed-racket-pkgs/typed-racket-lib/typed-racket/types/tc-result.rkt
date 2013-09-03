#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep rep-utils)
         (utils tc-utils)
         (types base-abbrev)
         racket/match
         (prefix-in c: (contract-req)))

;; this structure represents the result of typechecking an expression
(define-struct/cond-contract tc-result
  ([t Type/c] [f FilterSet/c] [o Object?])
  #:transparent)
(define-struct/cond-contract tc-results
  ([ts (c:listof tc-result?)] [drest (c:or/c (c:cons/c Type/c symbol?) #f)])
  #:transparent)
(define-struct/cond-contract tc-any-results () #:transparent)
(define tc-any-results* (tc-any-results))

(define (tc-results/c v)
  (or (tc-results? v)
      (tc-any-results? v)))

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

(define-match-expander tc-any-results:
  (syntax-rules ()
   [(_)
    (struct tc-any-results ())]))


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
                (make-tc-results
                 (cond [(Type/c? t)
                        (list (make-tc-result t -no-filter -no-obj))]
                       [else
                        (for/list ([i (in-list t)])
                          (make-tc-result i -no-filter -no-obj))])
                 #f)]
               [(t f)
                (make-tc-results
                 (if (Type/c? t)
                     (list (make-tc-result t f -no-obj))
                     (for/list ([i (in-list t)] [f (in-list f)])
                       (make-tc-result i f -no-obj)))
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
  (c:->i ([t (c:or/c Type/c (c:listof Type/c))])
         ([f (t) (if (list? t)
                     (c:listof FilterSet/c)
                     FilterSet/c)]
          [o (t) (if (list? t)
                     (c:listof Object?)
                     Object?)]
          [dty Type/c]
          [dbound symbol?])
         [res tc-results/c])])

(define (combine-results tcs)
  (match tcs
    [(list (tc-result1: t f o) ...)
     (ret t f o)]))

(define tc-result-equal? equal?)

(provide tc-result: tc-results: tc-any-results: tc-result1: Result1: Results:
         (rename-out
           (tc-any-results* tc-any-results)))
(provide/cond-contract
 [combine-results ((c:listof tc-results?) . c:-> . tc-results?)]
 [tc-result-t (tc-result? . c:-> . Type/c)]
 [tc-result-equal? (tc-result? tc-result? . c:-> . boolean?)]
 [tc-results? (c:any/c . c:-> . boolean?)]
 [tc-results/c c:flat-contract?])
