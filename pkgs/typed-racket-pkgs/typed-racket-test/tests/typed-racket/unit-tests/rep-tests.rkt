#lang racket/base

;; Tests for TR representation data structures such as types

(require "test-utils.rkt"
         rackunit
         typed-racket/rep/rep-utils
         typed-racket/rep/type-rep
         typed-racket/types/abbrev)

(provide tests)
(gen-test-main)

(define tests
  (test-suite
   "Tests for TR IR data structures"

   ;; Make sure that unsafe operations return the same results as safe ones
   (check-equal? (Rep-seq -String) (unsafe-Rep-seq -String))
   (check-equal? (Rep-seq (-pair -String -String)) (unsafe-Rep-seq (-pair -String -String)))
   (check-equal? (Type-key -String) (unsafe-Type-key -String))
   (check-equal? (Type-key (-pair -String -String)) (unsafe-Type-key (-pair -String -String)))
   ))
