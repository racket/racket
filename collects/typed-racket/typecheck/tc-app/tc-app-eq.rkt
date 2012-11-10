#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         syntax/parse/experimental/reflect
         (typecheck signatures tc-funapp check-below)
         (types abbrev union utils)
         (rep type-rep)

         ;; fixme - don't need to be bound in this phase - only to make tests work
         racket/bool
         ;; end fixme

         (for-template racket/base racket/bool))

(import tc-expr^)
(export tc-app-eq^)

;; comparators that inform the type system
(define-syntax-class comparator
  #:literals (eq? equal? eqv? = string=? symbol=? memq member memv)
  (pattern eq?) (pattern equal?) (pattern eqv?) (pattern =) (pattern string=?) (pattern symbol=?)
  (pattern member) (pattern memq) (pattern memv))


(define-tc/app-syntax-class (tc/app-eq expected)
  (pattern (eq?:comparator v1 v2)
    ;; make sure the whole expression is type correct
    (match* ((tc/funapp #'eq? #'(v1 v2) (single-value #'eq?)
                        (map single-value (syntax->list #'(v1 v2))) expected)
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
  (define (ok? val)
    (define-syntax-rule (alt nm pred ...)
      (and (free-identifier=? #'nm comparator) (or (pred val) ...)))
    (or (alt symbol=? symbol?)
        (alt string=? string?)        
        (alt eq? eq?-able)
        (alt eqv? eqv?-able)
        (alt equal? equal?-able)))
  (match* ((single-value v1) (single-value v2))
    [((tc-result1: t _ o) (tc-result1: (Value: (? ok? val))))
     (ret -Boolean
          (-FS (-filter-at (-val val) o)
               (-not-filter-at (-val val) o)))]
    [((tc-result1: (Value: (? ok? val))) (tc-result1: t _ o))
     (ret -Boolean
          (-FS (-filter-at (-val val) o)
               (-not-filter-at (-val val) o)))]
    [((tc-result1: t _ o)
      (or (and (? (lambda _ (free-identifier=? #'member comparator)))
               (tc-result1: (app untuple (list (and ts (Value: _)) ...))))
          (and (? (lambda _ (free-identifier=? #'memv comparator)))
               (tc-result1: (app untuple (list (and ts (Value: (? eqv?-able))) ...))))
          (and (? (lambda _ (free-identifier=? #'memq comparator)))
               (tc-result1: (app untuple (list (and ts (Value: (? eq?-able))) ...))))))
     (let ([ty (apply Un ts)])
       (ret (Un (-val #f) t)
            (-FS (-filter-at ty o)
                 (-not-filter-at ty o))))]
    [(_ _) (ret -Boolean)]))


