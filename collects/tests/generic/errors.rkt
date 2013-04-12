#lang racket/base

(require racket/generic unstable/macro-testing)

(module+ test
  (require rackunit)

  (check-exn #rx"not a name for a generics group"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods 3))))
  (check-exn #rx"not a name for a generics group"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods bad))))
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
  )
