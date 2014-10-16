#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (rep filter-rep)
         (types abbrev union filter-ops)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define (not-implied-atomic? x y) (not (implied-atomic? x y)))

(define-syntax (test-opposite stx)
  (define-syntax-class complementary
     (pattern #:complementary #:with check #'check-true)
     (pattern #:not-complementary #:with check #'check-false))
  (define-syntax-class contradictory
     (pattern #:contradictory #:with check #'check-true)
     (pattern #:not-contradictory #:with check #'check-false))
  (syntax-parse stx
    [(_ comp:complementary contr:contradictory f1* f2*)
     (syntax/loc stx
       (test-case (~a '(opposite f1* f2*))
         (define f1 f1*)
         (define f2 f2*)
         (comp.check (complementary? f1 f2) "Complementary")
         (contr.check (contradictory? f1 f2) "Contradictory")))]))


(define tests
  (test-suite "Filters"
    (test-suite "Opposite"
      (test-opposite #:not-complementary #:contradictory
        (-filter -Symbol 0)
        (-not-filter (Un -Symbol -String) 0))

      (test-opposite #:complementary #:not-contradictory
        (-filter (Un -Symbol -String) 0)
        (-not-filter -Symbol 0))

      (test-opposite #:complementary #:contradictory
        (-not-filter -Symbol 0)
        (-filter -Symbol 0))

      (test-opposite #:not-complementary #:not-contradictory
        (-filter -Symbol 1)
        (-not-filter -Symbol 0))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-filter -Symbol 0)
        (-filter -String 0))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-filter -Symbol 0)
        (-filter -String 0))

      (test-opposite #:not-complementary #:contradictory
        -bot
        -bot)

      (test-opposite #:not-complementary #:contradictory
        -bot
        -top)

      (test-opposite #:complementary #:not-contradictory
        -top
        -top)

    )

    (test-suite "Implied Atomic"
      (check implied-atomic?
             -top -top)
      (check implied-atomic?
             -bot -bot)
      (check implied-atomic?
             -top -bot)
      (check not-implied-atomic?
             -bot -top)
      (check implied-atomic?
             -top (-filter -Symbol 0))
      (check implied-atomic?
             (-filter -Symbol 0) -bot)
      (check implied-atomic?
             (-filter (Un -String -Symbol) 0)
             (-filter -Symbol 0))
      (check not-implied-atomic?
             (-filter -Symbol 0)
             (-filter (Un -String -Symbol) 0))
      (check implied-atomic?
             (-not-filter -Symbol 0)
             (-not-filter (Un -String -Symbol) 0))
      (check not-implied-atomic?
             (-not-filter (Un -String -Symbol) 0)
             (-not-filter -Symbol 0))
      (check not-implied-atomic?
             (-filter -Symbol 1)
             (-filter -Symbol 0))
      (check implied-atomic?
             (-filter -Symbol #'x)
             (-filter -Symbol #'x))
      (check implied-atomic?
             (-or (-filter -Symbol 1) (-filter -Symbol #'x))
             (-filter -Symbol #'x))
      (check implied-atomic?
             (-filter -Symbol #'x)
             (-and (-filter -Symbol 1) (-filter -Symbol #'x)))
      (check implied-atomic?
             (-not-filter (-val #f) #'x)
             (-filter -Symbol #'x))
    )

    (test-suite "Implication"
      (check-equal? (-imp -bot (-filter -Symbol #'x)) -top)
      (check-equal? (-imp -top (-filter -Symbol #'x)) (-filter -Symbol #'x))
      (check-equal? (-imp (-filter -Symbol #'x) -top) -top)
      (check-equal? (-imp (-filter -Symbol #'x) -bot) (-not-filter -Symbol #'x))
      (check-equal? (-imp (-not-filter -Symbol #'x) -bot) (-filter -Symbol #'x))
      (check-equal? (-imp (-imp (-not-filter -Symbol #'x) (-not-filter -Symbol #'y)) -bot)
                    (-and (-not-filter -Symbol #'x) (-filter -Symbol #'y)))
      (check-equal?
        (-imp (-not-filter -Symbol #'x)
              (-not-filter -Symbol #'y))
        (make-ImpFilter (-not-filter -Symbol #'x)
                        (-not-filter -Symbol #'y))))

    (test-suite "Simplification"
      (check-equal?
        (-and (-filter -Symbol #'x) (-not-filter (-val #f) #'x))
        (-filter -Symbol #'x))
      (check-equal?
        (-and (-not-filter (-val #f) #'x) (-filter -Symbol #'x))
        (-filter -Symbol #'x))

      (check-equal?
        (-and (-filter (-val #f) #'y)
              (-or (-filter (-val #f) #'y)
                   (-filter (-val #f) #'x)))
        (-filter (-val #f) #'y))

      (check-equal?
        (-and (-not-filter (-val #f) #'y)
              (-or (-not-filter (-val #f) #'y)
                   (-not-filter (-val #f) #'x)))
        (-not-filter (-val #f) #'y)))

  ))
