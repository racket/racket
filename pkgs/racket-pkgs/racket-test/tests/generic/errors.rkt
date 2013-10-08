#lang racket/base

(require racket/generic racket/engine unstable/macro-testing)

(module+ test
  (require rackunit)

  (check-exn #rx"expected 2 arguments after keyword"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods 3))))
  (check-exn #rx"not a name for a generic interface"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods 3 ()))))
  (check-exn #rx"not a name for a generic interface"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods bad ()))))
  (check-exn #rx"method definition has an incorrect arity"
             (lambda () (convert-compile-time-error
                         (let ()
                          (define-generics foobar
                            [foo foobar x])
                          (struct inst ()
                                  #:methods gen:foobar
                                  [(define (foo) 0)])
                          'ignore))))
  (check-exn #rx"method definition has an incorrect arity"
             (lambda () (convert-compile-time-error
                         (let ()
                          (define-generics foobar
                            [foo foobar x]
                            [bar foobar x])
                          (struct inst ()
                                  #:methods gen:foobar
                                  [(define (foo foobar x) 0)
                                   (define (bar foobar) 1)])
                          'ignore))))
  (check-exn #rx"generic method definition is not a function"
             (lambda () (convert-compile-time-error
                         (let ()
                          (define-generics foobar
                            [foo foobar x])
                          (struct inst ()
                                  #:methods gen:foobar
                                  [(define foo 0)])
                          'ignore))))
  ;; Test for PR 14069, make sure that the error reports
  ;; the faulty syntax as `bar` and not `#:methods`
  (check-exn #rx"at: bar"
             (lambda () (convert-compile-time-error
                         (let ()
                          (struct foo () #:methods bar [])
                          'ignore))))
  (check-exn #rx"foo: not implemented for 5"
             (lambda () (let ()
                          (define-generics foobar
                            [foo foobar x]
                            #:defaults ([number?]))
                          ;; a failing implementation may loop forever,
                          ;; due to self-reference to the generic method foo,
                          ;; so using an engine here to limit the time.
                          (define e (engine (lambda (suspend?) (foo 5 6))))
                          ;; 1000 ms should be far more than enough.
                          (or (engine-run 1000 e)
                              (error "computation did not terminate")))))
  )
