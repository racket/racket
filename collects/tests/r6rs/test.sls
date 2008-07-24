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
          run-test
          report-test-results)
  (import (rnrs))

  (define-record-type err
    (fields err-c))

  (define-record-type expected-exception
    (fields))

  (define-record-type multiple-results
    (fields values))

  (define-syntax test
    (syntax-rules ()
      [(_ expr expected)
       (begin
         ;; (write 'expr) (newline)
         (run-test 'expr
                   (catch-exns (lambda () expr))
                   expected))]))

   (define (catch-exns thunk)
      (guard (c [#t (make-err c)])
        (thunk)))

  (define-syntax test/approx
    (syntax-rules ()
      [(_ expr expected)
       (test (approx expr) (approx expected))]))

  (define (approx v)
    (let ([n (* (inexact v) 1000.0)])
      (+ (round (real-part n))
         (* (round (imag-part n)) (sqrt -1)))))

  (define-syntax test/exn
    (syntax-rules ()
      [(_ expr condition)
       (test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                        (make-expected-exception)])
                    expr)
             (make-expected-exception))]))

  (define-syntax test/values
    (syntax-rules ()
      [(_ expr val ...)
       (run-test 'expr
                 (catch-exns (lambda () 
                               (call-with-values
                                   (lambda () expr)
                                 (lambda results (make-multiple-results results)))))
                 (make-multiple-results (list val ...)))]))

  (define-syntax test/output
    (syntax-rules ()
      [(_ expr expected str)
       (run-test 'expr
                 (capture-output
                  (lambda ()
                    (run-test 'expr
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
       (test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
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
  
  (define (same-result? got expected)
    (cond
     [(and (real? expected) (nan? expected))
      (and (real? got) (nan? got))]
     [(expected-exception? expected)
      (expected-exception? got)]
     [(multiple-results? expected)
      (and (multiple-results? got)
           (= (length (multiple-results-values expected))
              (length (multiple-results-values got)))
           (for-all same-result?
                    (multiple-results-values expected)
                    (multiple-results-values got)))]
     [else (equal? got expected)]))
    
  (define (run-test expr got expected)
    (set! checked (+ 1 checked))
    (unless (same-result? got expected)
      (set! failures
            (cons (list expr got expected)
                  failures))))

  (define (write-result prefix v)
    (if (multiple-results? v)
        (for-each (lambda (v)
                    (write-result prefix v))
                  (multiple-results-values v))
        (begin
          (display prefix)
          (write v))))

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
                      (display "\nResult:")
                      (write-result "\n " (cadr t))
                      (display "\nExpected:")
                      (write-result "\n " (caddr t))
                      (display "\n\n"))
                    (reverse failures))
          (display (length failures))
          (display " of ")
          (display checked)
          (display " tests failed.\n")))))
