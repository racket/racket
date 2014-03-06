#lang racket/base

(require "test-utils.rkt"
         rackunit racket/list
         (types abbrev union tc-result)
         (rep filter-rep object-rep)
         (typecheck check-below)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax test-below
  (syntax-parser
    [(_ t1:expr t2:expr)
     #'(check-not-exn (lambda () (check-below t1 t2)))]
    [(_ #:fail (~optional message:expr #:defaults [(message #'#rx"type mismatch")]) t1:expr t2:expr)
     #'(check-exn message
         (lambda () (check-below t1 t2)))]))


(define tests
  (test-suite "Check Below"
    (test-below (Un) Univ)
    (test-below #:fail -Symbol -String)
    (test-below 
      (ret (list -Symbol) (list -no-filter) (list -no-obj))
      (ret (list Univ) (list -no-filter) (list -no-obj)))

    (test-below #:fail 
      (ret (list -Symbol) (list -top-filter) (list -no-obj))
      (ret (list Univ) (list -true-filter) (list -no-obj)))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -no-obj))
      (ret (list Univ) (list -top-filter) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -top-filter) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -no-obj))
      (ret (list Univ) (list -true-filter) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -true-filter) (list (make-Path empty #'x))))


    ;; Enable these once check-below is fixed
    #;
    (test-below #:fail
      (ret (list Univ) (list -no-filter) (list -no-obj) Univ 'B)
      (ret (list Univ) (list -false-filter) (list -no-obj) Univ 'B))

    #;
    (test-below #:fail
      (ret (list Univ) (list -no-filter) (list -no-obj))
      (ret (list Univ) (list -false-filter) (list -no-obj) Univ 'B))

    #;
    (test-below #:fail
      (ret (list Univ Univ) (list -no-filter -no-filter) (list -no-obj -no-obj))
      (ret (list Univ Univ) (list -false-filter -false-filter) (list -no-obj -no-obj)))

  ))
