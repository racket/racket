(module test-harness mzscheme
  (provide (all-defined))
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "pretty.ss"))
  
  (define print-tests (make-parameter #f))
  (define test-inspector (make-parameter (current-inspector)))
  (define test-inexact-epsilon (make-parameter 0.01))
  
  (define-struct (exn:test exn) ())
  
  (define (install-test-inspector)
    (test-inspector (current-inspector))
    (current-inspector (make-inspector))
    (print-struct #t))
  
  (define (may-print-result result)
    (parameterize ([current-inspector (test-inspector)]
                   [print-struct #t])
      (when (or (eq? (print-tests) (first result))
                (eq? (print-tests) #t))
        (pretty-print result))
      (when (and (eq? (print-tests) 'stop)
                 (eq? (first result) 'bad))
        (raise (make-exn:test (format "test failed: ~a" result)
                              (current-continuation-marks))))))
  
  
  (define test 
    (opt-lambda (result expected [compare equal?])
      (let* ([test-result
              (cond [(or (and (number? result) (not (exact? result)))
                         (and (number? expected) (not (exact? expected))))
                     (< (abs (- result expected)) (test-inexact-epsilon))]
                    [else
                     (parameterize ([current-inspector (test-inspector)])
                       (compare result expected))])]
             [to-print (if test-result 
                           (list 'good result expected)
                           (list 'bad result expected))])
        
        (may-print-result to-print)
        to-print)))
  
  (define (test/pred result pred)
    (let* ([test-result (pred result)]
           [to-print (if test-result
                         (list 'good result test-result)
                         (list 'bad result test-result))])
      (may-print-result to-print)
      to-print))
  
  (define (test/exn thunk expected-exception-msg)
    (unless (and (procedure? thunk)
                 (procedure-arity-includes? thunk 0))
      (error (format
              "the first argument to test/exn should be a function of no arguments (a \"thunk\"), got ~a"
              thunk)))
    (let* ([result
            (with-handlers 
                ([void (lambda (exn) exn)])
              (thunk))]
           [test-result
            (if (and (exn? result)
                     (regexp-match expected-exception-msg (exn-message result)))
                (list 'good result expected-exception-msg)
                (list 'bad result expected-exception-msg))])
      (may-print-result test-result)
      test-result))
  
  (install-test-inspector)
  )


