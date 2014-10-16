#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match
         (typecheck signatures tc-funapp)
         (types abbrev union utils)
         (rep type-rep)

         (for-label racket/base racket/bool))

(import tc-expr^)
(export tc-app-eq^)

(define-literal-set eq-literals
  #:for-label
  (eq? equal? eqv? string=? symbol=? memq member memv))

;; comparators that inform the type system
;; `=' is not included. Its type is more useful than this typing rule.
(define-syntax-class comparator
  #:literal-sets (eq-literals)
  (pattern (~or eq? equal? eqv? string=? symbol=? member memq memv)))


(define-tc/app-syntax-class (tc/app-eq expected)
  (pattern (eq?:comparator v1 v2)
    ;; make sure the whole expression is type correct
    (match* ((tc/funapp #'eq? #'(v1 v2) (tc-expr/t #'eq?)
                        (stx-map single-value #'(v1 v2)) expected)
             ;; check thn and els with the eq? info
             (tc/eq #'eq? #'v1 #'v2))
      [((tc-result1: t) (tc-result1: t* f o))
           (ret t f o)])))


;; typecheck eq? applications
;; identifier expr expr -> tc-results
(define (tc/eq comparator v1 v2)
  (define (eq?-able e) (or (boolean? e) (keyword? e) (symbol? e) (eof-object? e)))
  (define (eqv?-able e) (or (eq?-able e) (number? e) (char? e)))
  (define (equal?-able e) #t)
  (define (id=? a b)
    (free-identifier=? a b #f (syntax-local-phase-level)))
  (define (ok? val)
    (define-syntax-rule (alt nm pred ...)
      (and (id=? #'nm comparator)
           (or (pred val) ...)))
    (or (alt symbol=? symbol?)
        (alt string=? string?)
        (alt eq? eq?-able)
        (alt eqv? eqv?-able)
        (alt equal? equal?-able)))
  (match* ((single-value v1) (single-value v2))
    [((tc-result1: t _ o) (tc-result1: (Value: (? ok? val))))
     (ret -Boolean (-FS (-filter (-val val) o) (-not-filter (-val val) o)))]
    [((tc-result1: (Value: (? ok? val))) (tc-result1: t _ o))
     (ret -Boolean (-FS (-filter (-val val) o) (-not-filter (-val val) o)))]
    [((tc-result1: t _ o)
      (or (and (? (lambda _ (id=? #'member comparator)))
               (tc-result1: (List: (list (and ts (Value: _)) ...))))
          (and (? (lambda _ (id=? #'memv comparator)))
               (tc-result1: (List: (list (and ts (Value: (? eqv?-able))) ...))))
          (and (? (lambda _ (id=? #'memq comparator)))
               (tc-result1: (List: (list (and ts (Value: (? eq?-able))) ...))))))
     (let ([ty (apply Un ts)])
       (ret (Un (-val #f) t)
            (-FS (-filter ty o)
                 (-not-filter ty o))))]
    [(_ _) (ret -Boolean)]))


