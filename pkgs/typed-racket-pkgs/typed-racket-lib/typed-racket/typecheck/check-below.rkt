#lang racket/base

(require "../utils/utils.rkt"
         racket/match (prefix-in - (contract-req))
         (types utils union subtype filter-ops)
         (utils tc-utils)
         (rep type-rep object-rep filter-rep))

(provide/cond-contract
 [check-below (-->d ([s (-or/c Type/c tc-results/c)] [t (-or/c Type/c tc-results/c)]) ()
                    [_ (if (Type/c? s) Type/c tc-results/c)])]
 [cond-check-below (-->d ([s (-or/c Type/c tc-results/c)] [t (-or/c #f Type/c tc-results/c)]) ()
                         [_ (if (Type/c? s) Type/c tc-results/c)])])

(define (print-object o)
  (match o
    [(Empty:) "no object"]
    [_ (format "object ~a" o)]))

;; If expected is #f, then just return tr1
;; else behave as check-below
(define (cond-check-below tr1 expected)
  (if expected (check-below tr1 expected) tr1))

;; expected-but-got : (U Type String) (U Type String) -> Void
;;
;; Helper to print messages of the form
;;   "Expected a, but got b"
;;
;; Also handles cases like two type variables that
;; have the same name.
(define (expected-but-got t1 t2)
  (match* (t1 t2)
    [((F: s1) (F: s2))
     (=> fail)
     (unless (string=? (symbol->string s1) (symbol->string s2))
       (fail))
     (tc-error/expr "Expected ~a, but got a different ~a (bound in another scope)"
                    t1 t2)]
    [(_ _)
     (tc-error/expr "Expected ~a, but got ~a" t1 t2)]))

;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))
(define (check-below tr1 expected)
  (define (filter-better? f1 f2)
    (match* (f1 f2)
      [(f f) #t]
      [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-))
       (and (implied-atomic? f2+ f1+)
            (implied-atomic? f2- f1-))]
      [(_ _) #f]))
  (define (object-better? o1 o2)
    (match* (o1 o2)
      [(o o) #t]
      [(o (or (NoObject:) (Empty:))) #t]
      [(_ _) #f]))
  (match* (tr1 expected)
    ;; these two have to be first so that errors can be allowed in cases where multiple values are expected
    [((tc-result1: (? (lambda (t) (type-equal? t (Un))))) (tc-results: ts2 (NoFilter:) (NoObject:)))
     (ret ts2)]
    [((tc-result1: (? (lambda (t) (type-equal? t (Un))))) _)
     expected]
    [((or (tc-any-results:) (tc-results: _)) (tc-any-results:)) tr1]

    [((tc-results: ts fs os) (tc-results: ts2 (NoFilter:) (NoObject:)))
     (unless (= (length ts) (length ts2))
       (tc-error/expr "Expected ~a values, but got ~a" (length ts2) (length ts)))
     (unless (for/and ([t (in-list ts)] [s (in-list ts2)]) (subtype t s))
       (expected-but-got (stringify ts2) (stringify ts)))
     (if (= (length ts) (length ts2))
         (ret ts2 fs os)
         (ret ts2))]
    [((tc-result1: t1 f1 o1) (tc-result1: t2 (FilterSet: (Top:) (Top:)) (Empty:)))
     (cond
       [(not (subtype t1 t2))
        (expected-but-got t2 t1)])
     expected]
    [((tc-result1: t1 f1 o1) (tc-result1: t2 f2 o2))
     (cond
       [(not (subtype t1 t2))
        (expected-but-got t2 t1)]
       [(and (not (filter-better? f1 f2))
             (object-better? o1 o2))
        (tc-error/expr "Expected result with filter ~a, got filter ~a" f2 f1)]
       [(and (filter-better? f1 f2)
             (not (object-better? o1 o2)))
        (tc-error/expr "Expected result with object ~a, got object ~a" o2 o1)]
       [(and (not (filter-better? f1 f2))
             (not (object-better? o1 o2)))
        (tc-error/expr "Expected result with filter ~a and ~a, got filter ~a and ~a" f2 (print-object o2) f1 (print-object o1))])
     expected]
    ;; case where expected is like (Values a ... a) but got something else
    [((tc-results: t1 f o) (tc-results: t2 f o dty dbound))
     (unless (= (length t1) (length t2))
       (tc-error/expr "Expected ~a values and ~a ..., but got ~a values"
                      (length t2) dty (length t1)))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     expected]
    ;; case where you have (Values a ... a) but expected something else
    [((tc-results: t1 f o dty dbound) (tc-results: t2 f o))
     (unless (= (length t1) (length t2))
       (tc-error/expr "Expected ~a values, but got ~a values and ~a ..."
                      (length t2) (length t1) dty))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     expected]
    [((tc-results: t1 f o dty1 dbound) (tc-results: t2 f o dty2 dbound))
     (unless (= (length t1) (length t2))
       (tc-error/expr "Expected ~a non dotted values, but got ~a" (length t2) (length t1)))
     (unless (andmap subtype t1 t2)
       (expected-but-got (stringify t2) (stringify t1)))
     (unless (subtype dty1 dty2)
       (tc-error/expr "Expected ~a in ..., but got ~a" dty2 dty1))
     expected]
    [((tc-results: t1 fs os) (tc-results: t2 fs os))
     (unless (= (length t1) (length t2))
       (tc-error/expr "Expected ~a values, but got ~a" (length t2) (length t1)))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     expected]
    [((tc-any-results:) (or (? Type/c? t) (tc-result1: t _ _)))
     (tc-error/expr "Expected 1 value, but got unknown number")
     expected]
    [((tc-any-results:) (tc-results: t2 fs os))
     (tc-error/expr "Expected ~a values, but got unknown number" (length t2))
     expected]

    [((tc-result1: t1 f o) (? Type/c? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     (ret t2 f o)]


    [((? Type/c? t1) (tc-any-results:)) t1]
    [((? Type/c? t1) (tc-result1: t2 (FilterSet: (list) (list)) (Empty:)))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     t1]
    [((? Type/c? t1) (tc-result1: t2 f o))
     (if (subtype t1 t2)
         (tc-error/expr "Expected result with filter ~a and ~a, got ~a" f (print-object o) t1)
         (expected-but-got t2 t1))
     t1]
    [((? Type/c? t1) (tc-results: ts2 fs os))
       (tc-error/expr "Expected one value, but got ~a" (length ts2))
       t1]
    [((? Type/c? t1) (? Type/c? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     expected]
    [((tc-results: ts fs os dty dbound) (tc-results: ts* fs* os* dty* dbound*))
     (int-err "dotted types with different bounds/filters/objects in check-below nyi: ~a ~a" tr1 expected)]
    [(a b) (int-err "unexpected input for check-below: ~a ~a" a b)]))
