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

;; Contract to check that values are tc-results/c and do not contain -no-filter or -no-obj.
;; Used to contract the return values of typechecking functions.
(define (full-tc-results/c r)
  (match r
    [(tc-any-results:) #t]
    [(tc-results: _ fs os)
     (and
       (not (member -no-filter fs))
       (not (member -no-obj os)))]
    [(tc-results: _ fs os _ _)
     (and
       (not (member -no-filter fs))
       (not (member -no-obj os)))]
    [else #f]))


(define-match-expander tc-result:
  (syntax-rules ()
   [(_ tp fp op) (tc-result tp fp op)]
   [(_ tp) (tc-result tp _ _)]))

;; expand-tc-results: (Listof tc-result) -> (Values (Listof Type) (Listof FilterSet) (Listof Object))
(define (expand-tc-results results)
  (values (map tc-result-t results) (map tc-result-f results) (map tc-result-o results)))

(define-match-expander tc-results:
  (syntax-rules ()
   [(_ tp)
    (tc-results (app expand-tc-results tp _ _) #f)]
   [(_ tp fp op)
    (tc-results (app expand-tc-results tp fp op) #f)]
   [(_ tp fp op dty dbound)
    (tc-results (app expand-tc-results tp fp op) (cons dty dbound))]))

(define-match-expander tc-any-results:
  (syntax-rules ()
   [(_)
    (tc-any-results)]))


(define-match-expander tc-result1:
  (syntax-rules ()
   [(_ tp) (tc-results: (list tp))]
   [(_ tp fp op) (tc-results: (list tp) (list fp) (list op))]))

(define (tc-results-ts* tc)
  (match tc
    [(tc-results: t) t]))

(define-match-expander Result1:
  (syntax-rules ()
   [(_ tp) (Results: (list tp))]
   [(_ tp fp op) (Results: (list tp) (list fp) (list op))]))

;; expand-Results: (Listof Rresult) -> (Values (Listof Type) (Listof FilterSet) (Listof Object))
(define (expand-Results results)
  (values (map Result-t results) (map Result-f results) (map Result-o results)))


(define-match-expander Results:
  (syntax-rules ()
   [(_ tp) (Values: (app expand-Results tp _ _))]
   [(_ tp fp op) (Values: (app expand-Results tp fp op))]
   [(_ tp fp op dty dbound) (ValuesDots: (app expand-Results tp fp op) dty dbound)]))

;; make-tc-result*: Type/c FilterSet/c Object? -> tc-result?
;; Smart constructor for a tc-result.
(define (-tc-result type filter object)
  (cond
    [(or (equal? type -Bottom) (equal? filter -bot-filter))
     (make-tc-result -Bottom -bot-filter object)]
    [else
     (make-tc-result type filter object)]))


;; convenience function for returning the result of typechecking an expression
(define ret
  (case-lambda [(t)
                (make-tc-results
                 (cond [(Type/c? t)
                        (list (-tc-result t -top-filter -empty-obj))]
                       [else
                        (for/list ([i (in-list t)])
                          (-tc-result i -top-filter -empty-obj))])
                 #f)]
               [(t f)
                (make-tc-results
                 (if (Type/c? t)
                     (list (-tc-result t f -empty-obj))
                     (for/list ([i (in-list t)] [f (in-list f)])
                       (-tc-result i f -empty-obj)))
                 #f)]
               [(t f o)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map -tc-result t f o)
                     (list (-tc-result t f o)))
                 #f)]
               [(t f o dty)
                (int-err "ret used with dty without dbound")]
               [(t f o dty dbound)
                (make-tc-results
                 (if (and (list? t) (list? f) (list? o))
                     (map -tc-result t f o)
                     (list (-tc-result t f o)))
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
 [rename tc-results-ts* tc-results-ts (tc-results? . c:-> . (c:listof Type/c))]
 [tc-result-equal? (tc-result? tc-result? . c:-> . boolean?)]
 [tc-results? (c:any/c . c:-> . boolean?)]
 [tc-results/c c:flat-contract?]
 [full-tc-results/c c:flat-contract?])
