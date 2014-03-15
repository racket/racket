#lang racket/base

(require "../utils/utils.rkt"
         racket/match (prefix-in - (contract-req))
         (types utils union subtype filter-ops abbrev)
         (utils tc-utils)
         (rep type-rep object-rep filter-rep)
         (only-in (types printer) pretty-format-type))

(provide/cond-contract
 [check-below (-->i ([s (-or/c Type/c tc-results/c)]
                     [t (s) (if (Type/c? s) Type/c tc-results/c)])
                    [_ (s) (if (Type/c? s) Type/c tc-results/c)])]
 [cond-check-below (-->i ([s (-or/c Type/c tc-results/c)]
                          [t (s) (-or/c #f (if (Type/c? s) Type/c tc-results/c))])
                         [_ (s) (-or/c #f (if (Type/c? s) Type/c tc-results/c))])]
 [type-mismatch (-->* ((-or/c Type/c string?) (-or/c Type/c string?))
                      ((-or/c string? #f))
                      -any)])

(define (print-object o)
  (match o
    [(or (NoObject:) (Empty:)) "no object"]
    [_ (format "object ~a" o)]))

;; If expected is #f, then just return tr1
;; else behave as check-below
(define (cond-check-below tr1 expected)
  (if expected (check-below tr1 expected) tr1))

;; type-mismatch : Any Any [String] -> Void
;; Type errors with "type mismatch", arguments may be types or other things
;; like the length of a list of types
(define (type-mismatch t1 t2 [more #f])
  (define t1* (if (Type/c? t1) (pretty-format-type t1 #:indent 12) t1))
  (define t2* (if (Type/c? t2) (pretty-format-type t2 #:indent 9) t2))
  (tc-error/expr/fields "type mismatch" #:more more
                        "expected" t1* "given" t2*))

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
     ;; FIXME: this case could have a better error message that, say,
     ;;        prints the binding locations of each type variable.
     (type-mismatch (format "`~a'" t1) (format "a different `~a'" t2)
                    "type variables bound in different scopes")]
    [(_ _) (type-mismatch t1 t2)]))

;; fix-filter: FilterSet [FilterSet] -> FilterSet
;; Turns NoFilter into the actual filter; leaves other filters alone.
(define (fix-filter f [f2 -top-filter])
  (match f
    [(NoFilter:) f2]
    [else f]))

;; fix-object: Object [Object] -> Object
;; Turns NoObject into the actual abject; leaves other objects alone.
(define (fix-object o [o2 -empty-obj])
  (match o
    [(NoObject:) o2]
    [else o]))

;; fix-results: tc-results -> tc-results
;; Turns NoObject/NoFilter into the Empty/TopFilter
(define (fix-results r)
  (match r
    [(tc-any-results:) tc-any-results]
    [(tc-results: ts fs os)
     (ret ts (map fix-filter fs) (map fix-object os))]
    [(tc-results: ts fs os dty dbound)
     (ret ts (map fix-filter fs) (map fix-object os) dty dbound)]))

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
    ;; These two cases have to be first so that bottom (exceptions, etc.) can be allowed in cases
    ;; where multiple values are expected.
    ;; We can ignore the filters and objects in the actual value because they would never be about a value
    [((tc-result1: (? (lambda (t) (type-equal? t (Un))))) _)
     (fix-results expected)]
    [((or (tc-any-results:) (tc-results: _)) (tc-any-results:)) tr1]

    [((tc-results: ts fs os) (tc-results: ts2 (NoFilter:) (NoObject:)))
     (unless (= (length ts) (length ts2))
       (type-mismatch (length ts2) (length ts) "mismatch in number of values"))
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
        (type-mismatch f2 f1 "mismatch in filter")]
       [(and (filter-better? f1 f2)
             (not (object-better? o1 o2)))
        (type-mismatch (print-object o2) (print-object o1) "mismatch in object")]
       [(and (not (filter-better? f1 f2))
             (not (object-better? o1 o2)))
        (type-mismatch (format "`~a' and `~a'" f2 (print-object o2))
                       (format "`~a' and `~a'" f1 (print-object o1))
                       "mismatch in filter and object")])
     (ret t2 (fix-filter f2 f1) (fix-object o2 o1))]
    ;; case where expected is like (Values a ... a) but got something else
    [((tc-results: t1 f o) (tc-results: t2 f o dty dbound))
     (unless (= (length t1) (length t2))
       (type-mismatch (format "~a values and `~a ...'" (length t2) dty)
                      (format "~a values" (length t1))
                      "mismatch in number of values"))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     (fix-results expected)]
    ;; case where you have (Values a ... a) but expected something else
    [((tc-results: t1 f o dty dbound) (tc-results: t2 f o))
     (unless (= (length t1) (length t2))
       (type-mismatch (format "~a values" (length t2))
                      (format "~a values and `~a'" (length t1) dty)
                      "mismatch in number of values"))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     (fix-results expected)]
    [((tc-results: t1 f o dty1 dbound) (tc-results: t2 f o dty2 dbound))
     (unless (= (length t1) (length t2))
       (type-mismatch (length t2) (length t1) "mismatch in number of non-dotted values"))
     (unless (andmap subtype t1 t2)
       (expected-but-got (stringify t2) (stringify t1)))
     (unless (subtype dty1 dty2)
       (type-mismatch dty2 dty1 "mismatch in ... argument"))
     (fix-results expected)]
    [((tc-results: t1 fs os) (tc-results: t2 fs os))
     (unless (= (length t1) (length t2))
       (type-mismatch (length t2) (length t1) "mismatch in number of values"))
     (unless (for/and ([t (in-list t1)] [s (in-list t2)]) (subtype t s))
       (expected-but-got (stringify t2) (stringify t1)))
     (fix-results expected)]
    [((tc-any-results:) (tc-result1: t _ _))
     (type-mismatch "1 value" "unknown number")
     (fix-results expected)]
    [((tc-any-results:) (tc-results: t2 fs os))
     (type-mismatch (format "~a values" (length t2)) "unknown number")
     (fix-results expected)]

    [((? Type/c? t1) (? Type/c? t2))
     (unless (subtype t1 t2)
       (expected-but-got t2 t1))
     expected]
    [((tc-results: ts fs os dty dbound) (tc-results: ts* fs* os* dty* dbound*))
     (int-err "dotted types with different bounds/filters/objects in check-below nyi: ~a ~a" tr1 expected)]
    [(a b) (int-err "unexpected input for check-below: ~a ~a" a b)]))
