#lang racket/base

(require racket/generic racket/engine syntax/macro-testing)

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
  
  ;; pull-request #821, report correct position of contract violation
  (check-exn #rx"meth:.*AA.*1st"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth AA b c])
                           (meth 1 2 3))))
             "1st arg contract violation")
  (check-exn #rx"meth:.*2nd"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a AA c])
                           (meth 1 2 3))))
             "2nd arg contract violation")
  (check-exn #rx"meth:.*3rd"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA])
                           (meth 1 2 3))))
             "3rd arg contract violation")
  (check-exn #rx"optional.*default"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d]])
                           (meth 1 2 3))))
             "omitted optional arg")
  (check-exn #rx"optional.*optarg"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d]])
                           (meth 1 2 3 "optarg"))))
             "given optional arg")
  (check-exn #rx"required keyword"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d] #:reqkw reqkw])
                           (meth 1 2 3 "optarg"))))
             "omitted required kw arg")
    (check-exn #rx"#:reqkw: \"requiredkw"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d] #:reqkw reqkw])
                           (meth 1 2 3 "optarg" #:reqkw "requiredkw"))))
             "given required kw arg")
  (check-exn #rx"#:optkw.*optional.*default"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d] #:optkw [optkw]])
                           (meth 1 2 3))))
             "omitted optional kw arg")
  (check-exn #rx"#:optkw.*optional.*optarg"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth a b AA [d] #:optkw [optkw]])
                           (meth 1 2 3 #:optkw "optarg"))))
             "given optional kw arg")
    (check-exn #rx"argument position: 1st$"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth AA])
                           (meth 1))))
             "no other args")
    (check-exn #rx"argument position: 1st\n"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth AA a])
                           (meth 1 2))))
             "other args present")
      (check-exn #rx"rest args:"
             (lambda () (convert-compile-time-error
                         (let ()
                           (define-generics AA [meth AA . a])
                           (meth 1 2))))
             "rest args present")
  )
