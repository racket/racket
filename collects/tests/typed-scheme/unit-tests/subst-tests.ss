#lang scheme/base

(require "test-utils.ss" "planet-requires.ss" (for-syntax scheme/base))
(require (rep type-rep)
	 (types type-utils type-abbrev)
         (schemeunit))

(define-syntax-rule (s img var tgt result)
  (test-eq? "test" (substitute img 'var tgt) result))

(define-syntax-rule (s... imgs var tgt result)
  (test-eq? "test" (substitute-dots (list . imgs) #f 'var tgt) result))

(define (subst-tests)
  (test-suite "Tests for substitution"
              (s N a (-v a) N)
              (s... (N B) a (make-Function (list (make-arr-dots null N (-v a) 'a))) (N B . -> . N))
              (s... (N B) a (make-Function (list (make-arr-dots (list -String) N (-v a) 'a))) (-String N B . -> . N))
              (s... (N B) a (make-Function (list (make-arr-dots (list -String) N (-v b) 'a))) (-String (-v b) (-v b) . -> . N))
              (s... (N B) a (make-Function (list (make-arr-dots (list -String) N (-v b) 'b))) 
                    (make-Function (list (make-arr-dots (list -String) N (-v b) 'b))))))

(define-go subst-tests)

