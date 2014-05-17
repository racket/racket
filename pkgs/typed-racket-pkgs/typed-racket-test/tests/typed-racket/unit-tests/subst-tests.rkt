#lang racket/base

(require "test-utils.rkt"
         (rep type-rep)
         (types utils abbrev numeric-tower substitute)
         rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (s img var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute img 'var tgt) result))


(define-syntax-rule (s* imgs rest var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) rest 'var tgt) result))

(define-syntax-rule (s... imgs var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) #f 'var tgt) result))

(define tests
  (test-suite "Tests for substitution"
    (s -Number a (-v a) -Number)
    (s* (-Symbol -String) #f a (make-ListDots (-v a) 'a) (-lst* -Symbol -String))
    (s* (-Symbol -String) Univ a (make-ListDots (-v a) 'a) (-lst* -Symbol -String #:tail (-lst Univ)))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots null -Number (-v a) 'a))) (-Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v a) 'a))) (-String -Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'a))) (-String (-v b) (-v b) . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b)))
          (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b))))))
