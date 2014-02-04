#lang racket/base

(require (for-syntax racket/base)
         "base.rkt"
         "check.rkt"
         "check-info.rkt"
         "result.rkt"
         "test-case.rkt"
         "test-suite.rkt"
         "util.rkt")

(provide (struct-out exn:test)
         (struct-out exn:test:check)
         (struct-out check-info)
         (struct-out test-result)
         (struct-out test-failure)
         (struct-out test-error)
         (struct-out test-success)
         (struct-out rackunit-test-case)
         (struct-out rackunit-test-suite)

         with-check-info
         with-check-info*

         make-check-name
         make-check-params
         make-check-location
         make-check-expression
         make-check-message
         make-check-actual
         make-check-expected

         check-name?
         check-params?
         check-location?
         check-expression?
         check-message?
         check-actual?
         check-expected?

         test-begin
         test-case
         test-suite
         make-test-suite
         delay-test
         (rename-out [make-rackunit-test-case make-test-case]
                     [rackunit-test-case? test-case?]
                     [rackunit-test-suite? test-suite?])
         current-test-name
         current-test-case-around
         test-suite-test-case-around
         test-suite-check-around
         
         define-test-suite
         define/provide-test-suite
         test-suite*

         before
         after
         around

         require/expose
         dynamic-require/expose

         define-shortcut

         test-check
         test-pred
         test-equal?
         test-eq?
         test-eqv?
         test-=
         test-true
         test-false
         test-not-false
         test-exn
         test-not-exn

         foldts-test-suite
         fold-test-results
         run-test-case
         run-test

         fail-check

         define-check
         define-simple-check
         define-binary-check
         
         current-check-handler
         current-check-around

         check
         check-exn
         check-not-exn
         check-true
         check-false
         check-pred
         check-eq?
         check-eqv?
         check-equal?
         check-=
         check-not-false
         check-not-eq?
         check-not-eqv?
         check-not-equal?
         check-regexp-match
         check-match
         fail)

(define (void-thunk) (void))


(define-syntax (define-shortcut stx)
  (syntax-case stx ()
    [(_ (name param ...) expr)
     (with-syntax ([expected-form (syntax->datum
                                   #`(#,(syntax name)
                                      test-desc
                                      #,@(syntax (param ...))))])
       (syntax/loc stx
         (define-syntax (name name-stx)
           (syntax-case name-stx ()
             [(name test-desc param ...)
              (with-syntax ([name-expr (syntax/loc name-stx expr)])
                (syntax/loc name-stx
                  (test-case test-desc name-expr)))]
             [_
              (raise-syntax-error
               #f
               (format "Correct form is ~a" (quote expected-form))
               name-stx)]))))]
    [_
     (raise-syntax-error
      #f
      "Correct form is (define-shortcut (name param ...) expr)"
      stx)]))

(define-shortcut (test-check operator expr1 expr2)
  (check operator expr1 expr2))

(define-shortcut (test-pred pred expr)
  (check-pred pred expr))

(define-shortcut (test-equal? expr1 expr2)
  (check-equal? expr1 expr2))

(define-shortcut (test-eq? expr1 expr2)
  (check-eq? expr1 expr2))

(define-shortcut (test-eqv? expr1 expr2)
  (check-eqv? expr1 expr2))

(define-shortcut (test-= expr1 expr2 epsilon)
  (check-= expr1 expr2 epsilon))

(define-shortcut (test-true expr)
  (check-true expr))

(define-shortcut (test-false expr)
  (check-false expr))

(define-shortcut (test-not-false expr)
  (check-not-false expr))

(define-shortcut (test-exn pred thunk)
  (check-exn pred thunk))

(define-shortcut (test-not-exn thunk)
  (check-not-exn thunk))
