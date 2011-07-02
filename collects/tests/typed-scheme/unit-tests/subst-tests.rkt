#lang scheme/base

(require "test-utils.rkt" (for-syntax scheme/base)
         (rep type-rep)
         (types utils abbrev numeric-tower substitute)
         rackunit)

(define-syntax-rule (s img var tgt result)
  (test-eq? "test" (substitute img 'var tgt) result))

(define-syntax-rule (s... imgs var tgt result)
  (test-eq? "test" (substitute-dots (list . imgs) #f 'var tgt) result))

(define (subst-tests)
  (test-suite "Tests for substitution"
              (s -Number a (-v a) -Number)
              (s... (-Number -Boolean) a (make-Function (list (make-arr-dots null -Number (-v a) 'a))) (-Number -Boolean . -> . -Number))
              (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v a) 'a))) (-String -Number -Boolean . -> . -Number))
              (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'a))) (-String (-v b) (-v b) . -> . -Number))
              (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b)))
                    (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b))))))

(define-go subst-tests)

