;;; Copyright (c) 2000-2015 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(library (tests unit-test-helpers)
  (export test-suite test assert-equal? with-output-to-string)
  (import (rnrs) (tests unit-test-helpers-implementation))
  
  (define-syntax test-suite
    (lambda (x)
      (define name->run-name
        (lambda (name)
          (datum->syntax name
            (string->symbol
              (string-append "run-" (symbol->string (syntax->datum name)))))))
      (syntax-case x ()
        [(_ name test test* ...)
         (with-syntax ([run (name->run-name #'name)])
           #'(define run
               (lambda ()
                 (display "Running ")
                 (write (quote name))
                 (display " test suite...\n")
                 (let f ([tests (list (lambda () test) (lambda () test*) ...)]
                         [successes 0] [failures 0] [exceptions 0])
                   (if (null? tests)
                       (begin
                         (display "Ran ")
                         (write (+ successes failures exceptions))
                         (display " tests with ")
                         (write successes)
                         (display " successes, ")
                         (write failures)
                         (display " failures, and ")
                         (write exceptions)
                         (display " exceptions\n"))
                       (guard (e [else 
                                   (display "    caught expection... ")
                                   (display-condition e)
                                   (newline)
                                   (f (cdr tests) successes failures
                                      (+ exceptions 1))])
                              (let ([result ((car tests))])
                                (write result)
                                (newline)
                                (if result
                                    (f (cdr tests) (+ successes 1) failures 
                                       exceptions)
                                    (f (cdr tests) successes (+ failures 1)
                                       exceptions)))))))))])))
  
  (define-syntax test
    (syntax-rules ()
      [(_ name assertion assertion* ...)
       (begin
         (display "  Testing ")
         (write (quote name))
         (display " ...")
         (and assertion assertion* ...))]))
  
  (define-syntax assert-equal?
    (syntax-rules ()
      [(_ expected actual)
       (or (equal? expected actual)
           (begin
             (newline)
             (display "!!! ")
             (write actual)
             (display " does not match expected: ") 
             (write expected)
             (newline)
             #f))]))

  (define-syntax assert-error
    (syntax-rules ()
      [(_ ?msg ?expr)
       (let ([msg ?msg])
         (guard (e [else
                    (let ([e-msg
                           (or (and (format-condition? e)
                                    (apply format (condition-message e)
                                           (condition-irritants e)))
                               (and (message-condition? e)
                                    (string=? msg (condition-message e))))])
                      (or (string=? msg e-msg)
                          #t
                          (raise (condition
                                   (make-format-condition)
                                   (make-message-condition
                                     "expected error message of ~s but got ~s")
                                   (make-irritants (list msg e-mesg))
                                   e))))])
           (let ([t ?expr])
             (raise
               (condition
                 (make-format-condition)
                 (make-message-condition
                   "exptected error with message of ~s but instead got result ~s")
                 (make-irritants (list msg t)))))))])))

