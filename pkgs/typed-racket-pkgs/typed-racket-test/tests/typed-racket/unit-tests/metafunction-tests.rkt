#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (types abbrev union filter-ops)
         (typecheck tc-metafunctions)
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

  ))
