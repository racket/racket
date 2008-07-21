#!r6rs

(library (tests r6rs test)
  (export test
          test/approx
          test/exn
          test/values
          test/output
          test/unspec
          test/unspec-or-exn
          test/output/unspec
          report-test-results)
  (import (rnrs))

  (define-record-type err
    (fields err-c))

  (define-record-type expected-exception
    (fields))

  (define-syntax test
    (syntax-rules ()
      [(_ expr expected)
       (begin
         ;; (write 'expr) (newline)
         (check-test 'expr
                     (guard (c [#t (make-err c)])
                            expr)
                     expected))]))

  (define-syntax test/approx
    (syntax-rules ()
      [(_ expr expected)
       (test (approx expr) (approx expected))]))

  (define (approx v)
    (let ([n (* (inexact v) 1000.0)])
      (+ (round (real-part n))
         (* (round (imag-part n)) +1i))))

  (define-syntax test/exn
    (syntax-rules ()
      [(_ expr condition)
       (test (guard (c [((condition-predicate condition) c)
                        (make-expected-exception)])
                    expr)
             (make-expected-exception))]))

  (define-syntax test/values
    (syntax-rules ()
      [(_ expr val ...)
       (test (call-with-values
                 (lambda () expr)
               list)
             (list val ...))]))

  (define-syntax test/output
    (syntax-rules ()
      [(_ expr expected str)
       (check-test 'expr
                   (capture-output
                    (lambda ()
                      (check-test 'expr
                                  (guard (c [#t (make-err c)])
                                         expr)
                                  expected)))
                   str)]))

  (define-syntax test/unspec
    (syntax-rules ()
      [(_ expr)
       (test (begin expr 'unspec) 'unspec)]))

  (define-syntax test/unspec-or-exn
    (syntax-rules ()
      [(_ expr condition)
       (test (guard (c [((condition-predicate condition) c)
                        'unspec])
                    (begin expr 'unspec))
             'unspec)]))

  (define-syntax test/output/unspec
    (syntax-rules ()
      [(_ expr str)
       (test/output (begin expr 'unspec) 'unspec str)]))

  (define checked 0)
  (define failures '())

  (define (capture-output thunk)
    (if (file-exists? "tmp-catch-out")
        (delete-file "tmp-catch-out"))
    (dynamic-wind
        (lambda () 'nothing)
        (lambda ()
          (with-output-to-file "tmp-catch-out"
            thunk)
          (call-with-input-file "tmp-catch-out"
            (lambda (p)
              (get-string-n p 1024))))
        (lambda ()
          (if (file-exists? "tmp-catch-out")
              (delete-file "tmp-catch-out")))))
  
  (define (check-test expr got expected)
    (set! checked (+ 1 checked))
    (unless (if (and (real? expected)
                     (nan? expected))
                (nan? got)
                (equal? got expected))
      (set! failures
            (cons (list expr got expected)
                  failures))))

  (define (report-test-results)
    (if (null? failures)
        (begin
          (display checked)
          (display " tests passed\n"))
        (begin
          (display (length failures))
          (display " tests failed:\n\n")
          (for-each (lambda (t)
                      (display "Expression:\n ")
                      (write (car t))
                      (display "\nResult:\n ")
                      (write (cadr t))
                      (display "\nExpected:\n ")
                      (write (caddr t))
                      (display "\n\n"))
                    (reverse failures))
          (display (length failures))
          (display " of ")
          (display checked)
          (display " tests failed.\n")))))

          

