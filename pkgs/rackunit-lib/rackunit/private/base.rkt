#lang racket/base
(require racket/contract/base
         "check-info.rkt"
         "location.rkt")

;; struct test : 
(define-struct test ())
;; struct (rackunit-test-case test) : (U string #f) thunk
(define-struct (rackunit-test-case test) (name action) #:transparent)
;; struct (rackunit-test-suite test) : string (fdown fup fhere seed -> (listof test-result)) thunk thunk
(define-struct (rackunit-test-suite test) (name tests before after) #:transparent)

;; struct exn:test exn : ()
;;
;; The exception throw by test failures
(define-struct (exn:test exn:fail) ())
;; struct (exn:test:check struct:exn:test) : (list-of check-info)
;;
;; The exception thrown to indicate a check has failed
(define-struct (exn:test:check exn:test) (stack)
  #:property prop:exn:srclocs
  (lambda (self)
    ;; Try to get a location from the stack.
    (define maybe-location (for/or ([check-info (exn:test:check-stack self)])
                             (and (check-location? check-info) check-info)))
    (cond [maybe-location
           (list (location->srcloc (check-info-value maybe-location)))]
          [else
           (list)])))
;; struct (exn:test:check:internal exn:test:check) : ()
;;
;; Exception thrown to indicate an internal failure in an
;; check, distinguished from a failure in user code.
(define-struct (exn:test:check:internal exn:test:check) ())

;; struct test-result : (U string #f)
(define-struct test-result (test-case-name))
;; struct (test-failure test-result) : exn:test
(define-struct (test-failure test-result)  (result))
;; struct (test-error test-result) : any
(define-struct (test-error test-result) (result))
;; struct (test-success test-result) : any
(define-struct (test-success test-result) (result))

(provide/contract
 (struct (rackunit-test-case test)
         ((name (or/c string? false/c))
          (action (-> any))))
 (struct (rackunit-test-suite test)
         ((name string?)
          (tests procedure?)
          (before (-> any))
          (after (-> any)))))

(provide (struct-out test)
         (struct-out exn:test)
         (struct-out exn:test:check)
         (struct-out exn:test:check:internal)
         (struct-out test-result)
         (struct-out test-failure)
         (struct-out test-error)
         (struct-out test-success))
