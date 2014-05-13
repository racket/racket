#lang racket/base

(require "test-utils.rkt"
         rackunit racket/list racket/match racket/format
         syntax/srcloc syntax/location
         (types abbrev union tc-result)
         (utils tc-utils)
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

(define (check-result result)
  (match result
    [(tc-results: ts fs os)
     (for-each check-filter fs)
     (for-each check-object os) ]
    [(tc-results: ts fs os dty bound)
     (for-each check-filter fs)
     (for-each check-object os)]
    [(tc-any-results: f)
     (check-filter f)]
    [(? Type/c?)
     (void)]))


(define-syntax (test-below stx)
  (syntax-parse stx
    [(_ t1:expr t2:expr (~optional (~seq #:result expected-result:expr)
                                     #:defaults [(expected-result #'t2)]))
     #`(test-case (~a 't1 " <: " 't2)
         (with-check-info (['location (build-source-location-list (quote-srcloc #,stx))]
                           ['expected expected-result])
           (define result (check-below t1 t2))
           (with-check-info (['actual result])
             (check-result result)
             (unless (equal? expected-result result)
               (fail-check "Check below did not return expected result.")))))]
    [(_ #:fail (~optional message:expr #:defaults [(message #'#rx"type mismatch")])
        t1:expr t2:expr
        (~optional (~seq #:result expected-result:expr)
                     #:defaults [(expected-result #'t2)]))
     #`(test-case (~a 't1 " !<: " 't2)
         (with-check-info (['location (build-source-location-list (quote-srcloc #,stx))]
                           ['expected expected-result])
           (define result
             (parameterize ([delay-errors? #t])
               (check-below t1 t2)))
           (with-check-info (['actual result])
             (define exn
               (let/ec exit
                 (with-handlers [(exn:fail? exit)]
                   (report-all-errors)
                   (fail-check "Check below did not fail."))))
             (check-result result)
             (unless (equal? expected-result result)
               (fail-check "Check below did not return expected result."))
             (check-regexp-match message (exn-message exn)))))]))


(define tests
  (test-suite "Check Below"
    (test-below -Bottom Univ)
    (test-below #:fail -Symbol -String)

    (test-below
      (ret -Bottom)
      (ret (list Univ Univ) (list -true-filter -no-filter) (list -no-obj -empty-obj))
      #:result (ret (list Univ Univ) (list -true-filter -bot-filter) (list -empty-obj -empty-obj)))

    (test-below
      (ret -Bottom)
      (ret (list Univ) (list -no-filter) (list -no-obj) Univ 'B)
      #:result (ret (list Univ) (list -bot-filter) (list -empty-obj) Univ 'B))

    ;; Bottom is not below everything if the number of values doesn't match up.
    (test-below #:fail
      (ret (list -Bottom -Bottom))
      (ret (list Univ) (list -true-filter) (list -no-obj))
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))

    (test-below #:fail
      (ret (list))
      (ret (list Univ) (list -true-filter) (list -no-obj))
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))

    (test-below
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -no-filter) (list -no-obj))
      #:result (ret (list Univ) (list -top-filter) (list -empty-obj)))

    (test-below
      (ret (list -Symbol) (list -true-filter) (list -empty-obj))
      (ret (list Univ) (list -top-filter) (list -empty-obj)))

    (test-below #:fail
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -true-filter) (list -no-obj))
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -top-filter) (list (make-Path empty #'x))))

    (test-below #:fail #rx"no object"
      (ret (list -Symbol) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -true-filter) (list (make-Path empty #'x))))

    (test-below (ret -Bottom) (tc-any-results -no-filter) #:result (tc-any-results -bot))
    (test-below (ret Univ) (tc-any-results -top) #:result (tc-any-results -top))
    (test-below (tc-any-results -bot) (tc-any-results -no-filter) #:result (tc-any-results -bot))
    (test-below
      (ret (list -Symbol -String) (list -true-filter -bot-filter))
      (tc-any-results -no-filter)
      #:result (tc-any-results -bot))
    (test-below (ret -Symbol -bot-filter) (tc-any-results -no-filter) #:result (tc-any-results -bot))

    (test-below (ret -Symbol -true-filter -empty-obj) (tc-any-results -no-filter)
      #:result (tc-any-results -top))
    (test-below (ret (list -Symbol -String)) (tc-any-results -no-filter)
      #:result (tc-any-results -top))
    (test-below
      (ret (list -Symbol -String) (list -true-filter -false-filter) (list -empty-obj -empty-obj))
      (tc-any-results -no-filter)
      #:result (tc-any-results -top))


    (test-below #:fail
      (ret -Symbol)
      (ret (list -Symbol -Symbol) (list -top-filter -no-filter) (list -no-obj -empty-obj))
      #:result (ret (list -Symbol -Symbol) (list -top-filter -top-filter) (list -empty-obj -empty-obj)))

    (test-below #:fail
      (tc-any-results -top)
      (ret -Symbol))


    (test-below #:fail
      (ret -Symbol -true-filter -empty-obj)
      (ret -Symbol -true-filter -empty-obj Univ 'B))

    (test-below #:fail
      (ret -Symbol -true-filter -empty-obj Univ 'B)
      (ret -Symbol -true-filter -empty-obj))

    (test-below #:fail
      (ret -Symbol)
      (ret -Symbol -no-filter -empty-obj Univ 'B)
      #:result (ret -Symbol -top-filter -empty-obj Univ 'B))

    (test-below #:fail
      (tc-any-results -top)
      (ret -Symbol -no-filter -empty-obj Univ 'B)
      #:result (ret (list -Symbol) (list -top-filter) (list -empty-obj) Univ 'B))

    (test-below #:fail
      (ret -Symbol -top-filter -empty-obj Univ 'B)
      (ret (list -Symbol -Symbol) (list -top-filter -top-filter)  (list -empty-obj -empty-obj) Univ 'B))

    (test-below (ret -Symbol -true-filter -empty-obj Univ 'B)
                (tc-any-results -no-filter)
                #:result (tc-any-results -top))

    (test-below
      (ret -Symbol)
      (ret -Symbol -no-filter -empty-obj)
      #:result (ret -Symbol -top-filter -empty-obj))

    (test-below
      (ret -Symbol -true-filter)
      (ret -Symbol -no-filter -empty-obj)
      #:result (ret -Symbol -true-filter -empty-obj))

    (test-below #:fail
      (ret -Symbol -true-filter)
      (ret (list Univ -Symbol) (list -no-filter -top-filter))
      #:result (ret (list Univ -Symbol) (list -top-filter -top-filter)))


    (test-below
      (ret (list Univ) (list -true-filter) (list -empty-obj))
      (ret Univ -no-filter)
      #:result (ret (list Univ) (list -true-filter) (list -empty-obj)))

    ;; Enable these once check-below is fixed
    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ) (list -top-filter) (list -empty-obj) Univ 'B)
      (ret (list Univ) (list -false-filter) (list -no-obj) Univ 'B)
      #:result (ret (list Univ) (list -false-filter) (list -empty-obj) Univ 'B))

    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ) (list -top-filter) (list -empty-obj))
      (ret (list Univ) (list -false-filter) (list -no-obj) Univ 'B)
      #:result (ret (list Univ) (list -false-filter) (list -empty-obj) Univ 'B))

    ;; Currently does not fail
    #;
    (test-below #:fail
      (ret (list Univ Univ) (list -top-filter -top-filter) (list -empty-obj -empty-obj))
      (ret (list Univ Univ) (list -false-filter -false-filter) (list -no-obj -no-obj))
      #:result (ret (list Univ Univ) (list -false-filter -false-filter) (list -empty-obj -empty-obj)))

  ))
