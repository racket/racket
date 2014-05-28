#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (types abbrev union filter-ops tc-result)
         (typecheck tc-metafunctions)
         (rep object-rep)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax (test-combine-props stx)
  (syntax-parse stx
    [(_ new:expr existing:expr expected:expr box-v:expr)
     (quasisyntax/loc stx
       (test-case (~a '(new + existing = expected))
         (define b (box #t))
         (define-values (res-formulas res-props) (combine-props new existing b))
         #,(syntax/loc stx (check-equal? (append res-formulas res-props) expected))
         #,(syntax/loc stx (check-equal? (unbox b) box-v))))]))


(define tests
  (test-suite "Metafunctions"

    (test-suite "combine-props"

      (test-combine-props
        (list (-or (-not-filter -String #'x) (-not-filter -String #'y)))
        (list (-filter (Un -String -Symbol) #'x) (-filter (Un -String -Symbol) #'y))
        (list (-or (-not-filter -String #'y) (-not-filter -String #'x))
              (-filter (Un -String -Symbol) #'y) (-filter (Un -String -Symbol) #'x))
        #t)

      (test-combine-props
        (list (-or (-filter -String #'x) (-filter -String #'y)))
        (list (-filter (Un -String -Symbol) #'x) (-filter (Un -String -Symbol) #'y))
        (list (-or (-filter -String #'y) (-filter -String #'x))
              (-filter (Un -String -Symbol) #'y) (-filter (Un -String -Symbol) #'x))
        #t)

    )

    (test-suite "merge-tc-results"
      (check-equal?
        (merge-tc-results (list))
        (ret -Bottom))
      (check-equal?
        (merge-tc-results (list (ret Univ)))
        (ret Univ))
      (check-equal?
        (merge-tc-results (list (ret Univ -top-filter (make-Path null #'x))))
        (ret Univ -top-filter (make-Path null #'x)))
      (check-equal?
        (merge-tc-results (list (ret -Bottom) (ret -Symbol -top-filter (make-Path null #'x))))
        (ret -Symbol -top-filter (make-Path null #'x)))
      (check-equal?
        (merge-tc-results (list (ret -String) (ret -Symbol)))
        (ret (Un -Symbol -String)))
      (check-equal?
        (merge-tc-results (list (ret -String -true-filter) (ret -Symbol -true-filter)))
        (ret (Un -Symbol -String) -true-filter))
      (check-equal?
        (merge-tc-results (list (ret (-val #f) -false-filter) (ret -Symbol -true-filter)))
        (ret (Un -Symbol (-val #f)) -top-filter))
      (check-equal?
        (merge-tc-results (list (ret (list (-val 0) (-val 1))) (ret (list (-val 1) (-val 2)))))
        (ret (list (Un (-val 0) (-val 1)) (Un (-val 1) (-val 2)))))
      (check-equal?
        (merge-tc-results (list (ret null null null -Symbol 'x) (ret null null null -String 'x)))
        (ret null null null (Un -Symbol -String) 'x))


    )
  ))
