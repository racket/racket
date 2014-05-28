#lang racket/base

(require "test-utils.rkt"
         rackunit 
         (typecheck tc-metafunctions)
         (rep object-rep)
         (types abbrev union tc-result))

(provide tests)
(gen-test-main)



(define tests
  (test-suite "Metafunctions"
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


    )))
