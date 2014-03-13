#lang racket/base

(require "test-utils.rkt"
         rackunit 
         (types abbrev union filter-ops))

(provide tests)
(gen-test-main)

(define (not-opposite? x y) (not (opposite? x y)))
(define (not-implied-atomic? x y) (not (implied-atomic? x y)))

(define tests
  (test-suite "Filters"
    (test-suite "Opposite"
      (check opposite?
             (-filter -Symbol 0)
             (-not-filter (Un -Symbol -String) 0))
      
      (check opposite?
             (-not-filter -Symbol 0)
             (-filter -Symbol 0))

      (check not-opposite?
             (-filter -Symbol 1)
             (-not-filter -Symbol 0))

      (check not-opposite?
             (-filter -String 0)
             (-not-filter -Symbol 0))

      (check not-opposite?
             (-not-filter -Symbol 0)
             (-filter -String 0))
      )

    (test-suite "Implied Atomic"
      (check implied-atomic?
             -top -top)
      (check implied-atomic?
             -bot -bot)
      (check not-implied-atomic?
             -bot -top)
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
      )

  ))
