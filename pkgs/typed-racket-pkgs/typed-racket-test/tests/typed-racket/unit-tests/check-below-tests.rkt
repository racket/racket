#lang racket/base

(require "test-utils.rkt"
         rackunit racket/list racket/match racket/format
         syntax/srcloc syntax/location
         (types abbrev union tc-result)
         (rep filter-rep object-rep type-rep)
         (typecheck check-below)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

;; Ensure that we never return a filter of NoFilter or an object of NoObject.
(define (check-filter f)
  (match f
    [(NoFilter:) (fail-check "Result has no filter (instead of a top filter).")]
    [_ (void)]))

(define (check-object o)
  (match o
    [(NoObject:) (fail-check "Result has no object (instead of an empty object).")]
    [_ (void)]))

(define-syntax (test-below stx)
  (syntax-parse stx
    [(_ t1:expr t2:expr (~optional (~seq #:result expected-result:expr)
                                     #:defaults [(expected-result #'t2)]))
     #`(test-case (~a 't1 " <: " 't2)
         (with-check-info (['location (build-source-location-list (quote-srcloc #,stx))]
                           ['expected expected-result])
           (define result (check-below t1 t2))
           (with-check-info (['actual result])
             (match result 
               [(tc-results: ts fs os)
                (for-each check-filter fs)
                (for-each check-object os) ]
               [(tc-results: ts fs os dty bound)
                (for-each check-filter fs)
                (for-each check-object os)]
               [(or (tc-any-results:) (? Type/c?))
                (void)])
             (unless (equal? expected-result result)
               (fail-check "Check below did not return expected result.")))))]
    [(_ #:fail (~optional message:expr #:defaults [(message #'#rx"type mismatch")]) t1:expr t2:expr)
     #`(test-case (~a 't1 " !<: " 't2)
         (define exn
           (let/ec exit
             (with-handlers [(exn:fail? exit)]
               (check-below t1 t2))
             (fail-check "Check below did not fail.")))
         (check-regexp-match message (exn-message exn)))]))
           


(define tests
  (test-suite "Check Below"
    (test-below -Bottom Univ)
    (test-below -Bottom tc-any-results
      #:result -Bottom)
    (test-below
      -Bottom
      (ret (list Univ) (list -true-filter) (list -no-obj))
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))
    (test-below
      -Bottom
      (ret (list Univ Univ) (list -true-filter -no-filter) (list -no-obj -empty-obj))
      #:result (ret (list Univ Univ) (list -true-filter -top-filter) (list -empty-obj -empty-obj)))
    (test-below
      (ret -Bottom)
      (ret (list Univ Univ) (list -true-filter -no-filter) (list -no-obj -empty-obj))
      #:result (ret (list Univ Univ) (list -true-filter -top-filter) (list -empty-obj -empty-obj)))
    (test-below
      -Bottom
      (ret (list Univ) (list -no-filter) (list -no-obj) Univ 'B)
      #:result (ret (list Univ) (list -top-filter) (list -empty-obj) Univ 'B))
    (test-below
      (ret -Bottom)
      (ret (list Univ) (list -no-filter) (list -no-obj) Univ 'B)
      #:result (ret (list Univ) (list -top-filter) (list -empty-obj) Univ 'B))

    ;; Bottom is not below everything if the number of values doesn't match up.
    (test-below #:fail
      (ret (list -Bottom -Bottom))
      (ret (list Univ) (list -true-filter) (list -no-obj)))
    (test-below #:fail
      (ret (list))
      (ret (list Univ) (list -true-filter) (list -no-obj)))



    (test-below #:fail -Symbol -String)
    (test-below 
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -no-filter) (list -no-obj))
      #:result (ret (list Univ) (list -top-filter) (list -empty-obj)))

    (test-below #:fail 
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -true-filter) (list -no-obj)))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -top-filter) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -true-filter) (list (make-Path empty #'x))))


    (test-below -Symbol tc-any-results
      #:result -Symbol)
    (test-below (ret -Symbol) tc-any-results
      #:result (ret -Symbol))
    (test-below (ret -Symbol -true-filter -empty-obj) tc-any-results
      #:result (ret -Symbol -true-filter -empty-obj))
    (test-below (ret (list -Symbol -String)) tc-any-results
      #:result (ret (list -Symbol -String)))
    (test-below
      (ret (list -Symbol -String) (list -true-filter -false-filter) (list -empty-obj -empty-obj))
      tc-any-results
      #:result (ret (list -Symbol -String) (list -true-filter -false-filter) (list -empty-obj -empty-obj)))
    (test-below (ret -Symbol -true-filter -empty-obj Univ 'B) tc-any-results
      #:result (ret -Symbol -true-filter -empty-obj Univ 'B))

    (test-below #:fail
      (ret -Symbol -true-filter -empty-obj)
      (ret -Symbol -true-filter -empty-obj Univ 'B))

    (test-below #:fail
      (ret -Symbol -true-filter -empty-obj Univ 'B)
      (ret -Symbol -true-filter -empty-obj))

    (test-below
      (ret -Symbol)
      (ret -Symbol -top-filter -no-obj)
      #:result (ret -Symbol -top-filter -empty-obj))
    (test-below
      (ret -Symbol)
      (ret -Symbol -no-filter -empty-obj)
      #:result (ret -Symbol -top-filter -empty-obj))

    (test-below
      -Symbol
      (ret -Symbol -top-filter -no-obj)
      #:result -Symbol)
    (test-below
      -Symbol
      (ret -Symbol -no-filter -empty-obj)
      #:result -Symbol)
    (test-below #:fail
      -Symbol
      (ret -Symbol -true-filter -no-obj))

    (test-below #:fail
      -Symbol
      (ret -Symbol -no-obj -empty-obj Univ 'B))
    (test-below #:fail
      -Symbol
      (ret (list -Symbol -Symbol) (list -top-filter -no-filter) (list -no-obj -empty-obj)))
    (test-below #:fail
      (ret (list Univ) (list -true-filter) (list -empty-obj) Univ 'B)
      Univ)
    (test-below
      (ret (list Univ) (list -true-filter) (list -empty-obj))
      Univ
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))


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
