#lang racket/base
(require
  "test-utils.rkt"
  rackunit
  racket/list
  (for-syntax racket/base syntax/parse)
  (rep type-rep)
  (r:infer infer)

  (types numeric-tower utils abbrev))

(provide tests)
(gen-test-main)

(define-syntax-rule (fv-t ty elems ...)
  (let ([ty* ty])
    (test-check (format "~a" 'ty)
                equal?
                (fv ty*)
                (list (quote elems) ...))))

(define-syntax (infer-t stx)
  (define-splicing-syntax-class vars
    (pattern (~seq) #:with vars #'empty)
    (pattern (~seq #:vars vars:expr) ))
  (define-splicing-syntax-class indices
    (pattern (~seq) #:with indices #'empty)
    (pattern (~seq #:indices indices:expr) ))
  (define-splicing-syntax-class pass
    (pattern (~seq) #:with pass #'#t)
    (pattern #:pass #:with pass #'#t)
    (pattern #:fail #:with pass #'#f))
  (syntax-parse stx
    ([_ S:expr T:expr :vars :indices :pass]
     #'(test-case "foobar"
         (define result (infer vars indices (list S) (list T) #f))
         (unless (equal? result pass)
           (fail-check "Could not infer a substitution"))))))



(define fv-tests
  (test-suite "Tests for fv"
              (fv-t -Number)
              [fv-t (-v a) a]
              [fv-t (-poly (a) a)]
              [fv-t (-poly (a b c d e) a)]
              [fv-t (-poly (b) (-v a)) a]
              [fv-t (-poly (b c d e) (-v a)) a]
              [fv-t (-mu a (-lst a))]
              [fv-t (-mu a (-lst (-pair a (-v b)))) b]

              [fv-t (->* null (-v a) -Number) a] ;; check that a is CONTRAVARIANT
              ))

(define infer-tests
  (test-suite "Tests for infer"
    (infer-t Univ Univ)
    (infer-t (-v a) Univ)
    (infer-t Univ (-v a) #:fail)
    (infer-t Univ (-v a) #:vars '(a))
    (infer-t (-v a) Univ #:vars '(a))
    (infer-t (-v a) -Bottom #:vars '(a))
    (infer-t (-v a) (-v b) #:fail)
    (infer-t (-v a) (-v b) #:vars '(a))
    (infer-t (-v a) (-v b) #:vars '(b))

    (infer-t (make-ListDots -Symbol 'b) (-lst -Symbol) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (-lst Univ) #:indices '(b))
    (infer-t (make-ListDots (-v a) 'b) (make-ListDots -Symbol 'b) #:vars '(a))
    (infer-t (make-ListDots -Symbol 'b) (make-ListDots Univ 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots Univ 'b) #:indices '(b))

    ;; Currently Broken
    ;(infer-t (make-ListDots (-v b) 'b) (-lst -Symbol) #:indices '(b))
    ;(infer-t (-lst -Symbol) (make-ListDots -Symbol 'b) #:indices '(b))
    ;(infer-t (make-ListDots (-v b) 'b) (make-ListDots -Symbol 'b) #:indices '(b))
    ;(infer-t (make-ListDots -Symbol 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    ;(infer-t (make-ListDots -Symbol 'b) (-pair -Symbol (-lst -Symbol)) #:indices '(b))
  ))


(define tests
  (test-suite "All inference tests"
    fv-tests
    infer-tests))
