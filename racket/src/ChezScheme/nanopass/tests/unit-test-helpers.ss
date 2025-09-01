;;; Copyright (c) 2000-2018 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(library (tests unit-test-helpers)
  (export test-suite test assert-equal? assert-error with-output-to-string format-error-message)
  (import (rnrs) (tests unit-test-helpers-implementation) (only (nanopass helpers) errorf))

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
                         (display " exceptions\n")
                         (and (= failures 0) (= exceptions 0)))
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

  ;; extended to cover record equality, but not doing the union-find
  ;; equality we should be doing.
  (define stupid-extended-equal?
    (lambda (x y)
      (or (equal? x y)
          (and (record? x)
               (record? y)
               (record=? x y)))))

  (define record-type-accessors
    (lambda (rtd)
      (let loop ([i (vector-length (record-type-field-names rtd))] [ls '()])
        (if (fx=? i 0)
            ls
            (let ([i (fx- i 1)])
              (loop i (cons (record-accessor rtd i) ls)))))))

  (define record=?
    (lambda (x y)
      (let ([rtd (record-rtd x)])
        (and (eq? rtd (record-rtd y))
             (let loop ([rtd rtd])
               (or (eq? rtd #f)
                   (and (for-all (lambda (ac) (stupid-extended-equal? (ac x) (ac y))) (record-type-accessors rtd))
                        (loop (record-type-parent rtd)))))))))

  (define-syntax assert-equal?
    (syntax-rules ()
      [(_ expected actual)
       (or (stupid-extended-equal? expected actual)
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
                    (let ([e-msg (with-output-to-string
                                   (lambda ()
                                     (display-condition e)))])
                      (or (string=? msg e-msg)
                          (begin
                            (newline)
                            (display "!!! expected error message ")
                            (write msg)
                            (display " does not match ")
                            (write e-msg)
                            (newline)
                            #f)))])
           (let ([t ?expr])
             (newline)
             (display "!!! expected error with message ")
             (write msg)
             (display " but got result ")
             (write t)
             (newline)
             #f)))])))
