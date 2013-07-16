#!r6rs

(library (tests r6rs exceptions)
  (export run-exceptions-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-exceptions-tests)

    (test/output
     (guard (con
             ((error? con)
              (if (message-condition? con)
                  (display (condition-message con))
                  (display "an error has occurred"))
              'error)
             ((violation? con)
              (if (message-condition? con)
                  (display (condition-message con))
                  (display "the program has a bug"))
              'violation))
            (raise
             (condition
              (make-error)
              (make-message-condition "I am an error"))))
     'error
     "I am an error")

    (test/exn
     (guard (con
             ((error? con)
              (if (message-condition? con)
                  (display (condition-message con))
                  (display "an error has occurred"))
              'error))
            (raise
             (condition
              (make-violation)
              (make-message-condition "I am an error"))))
     &violation)

    (test/output
     (guard (con
             ((error? con)
              (display "error opening file")
              #f))
            (call-with-input-file "foo-must-not-exist.scm" read))
     #f
     "error opening file")

    (test/output
     (with-exception-handler
      (lambda (con)
        (cond
         ((not (warning? con))
          (raise con))
         ((message-condition? con)
          (display (condition-message con)))
         (else
          (display "a warning has been issued")))
        42)
      (lambda ()
        (+ (raise-continuable
            (condition
             (make-warning)
             (make-message-condition
              "should be a number")))
           23)))
     65
     "should be a number")

    (test/exn (with-exception-handler (lambda (x) 0)
                                      (lambda () (error #f "bad")))
              &non-continuable)

    
    (let ([v '()])
      (test (guard (exn [(equal? exn 5) 'five])
                   ;; `guard' should jump back in before re-raising
                   (guard (exn [(equal? exn 6) 'six])
                          (dynamic-wind
                              (lambda () (set! v (cons 'in v)))
                              (lambda () (raise 5))
                              (lambda () (set! v (cons 'out v))))))
            'five)
      (test v '(out in out in)))
      

      
    ;;
    ))

