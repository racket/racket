#lang racket/base
(require "test-utils.rkt"
         rackunit
         (types numeric-tower utils abbrev))


(provide fv-tests)

(define-syntax-rule (fv-t ty elems ...)
  (let ([ty* ty])
    (test-check (format "~a" ty*)
                equal?
                (fv ty*)
                (list (quote elems) ...))))

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
