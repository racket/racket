#lang racket

(provide test
         test-ok check-ok
         test-bad check-bad
         check-not
         check/sort
         with/c)

(require rackunit racket/pretty srfi/67)

(define-syntax-rule (test e ...)
  (test-case (parameterize ([pretty-print-columns 50])
               (pretty-format/write '(test e ...)))
    e ...))
(define-syntax-rule (test-ok e ...) (test (check-ok e ...)))
(define-syntax-rule (test-bad e ...) (test (check-bad e ...)))
(define-syntax-rule (check-ok e ...) (check-not-exn (lambda () e ...)))
(define-syntax-rule (check-bad e ...) (check-exn exn:fail? (lambda () e ...)))

(define (pretty-format/write x)
  (with-output-to-string
    (lambda () (pretty-write x))))

(define-syntax-rule (with/c c e)
  (let () (with-contract value ([value c]) (define value e)) value))

(define-check (check-not compare actual expected)
  (with-check-info*
   (list (make-check-info 'comparison compare)
         (make-check-actual actual)
         (make-check-expected expected))
   (lambda ()
     (let* ([result (compare actual expected)])
       (when result
         (with-check-info*
          (list (make-check-info 'result result))
          (lambda () (fail-check))))))))

(define (check/sort actual expected
                    #:< [<< (<? default-compare)]
                    #:= [== equal?])
  (with-check-info*
   (list (make-check-name 'check/sort)
         (make-check-info '< <<)
         (make-check-info '= ==)
         (make-check-info 'actual actual)
         (make-check-info 'expected expected))
   (lambda ()
     (let* ([actual-sorted (sort actual <<)]
            [actual-length (length actual-sorted)]
            [expected-sorted (sort expected <<)]
            [expected-length (length expected-sorted)])
       (with-check-info*
        (list (make-check-info 'actual-sorted actual-sorted)
              (make-check-info 'expected-sorted expected-sorted))
        (lambda ()
          (unless (= actual-length expected-length)
            (with-check-info*
             (list (make-check-message
                    (format "expected ~a elements, but got ~a"
                            expected-length actual-length)))
             (lambda () (fail-check))))
          (let*-values
              ([(actuals expecteds)
                (for/lists
                    [actuals expecteds]
                    ([actual (in-list actual-sorted)]
                     [expected (in-list actual-sorted)]
                     #:unless (== actual expected))
                  (values actual expected))])
            (unless (and (null? actuals) (null? expecteds))
              (with-check-info*
               (list (make-check-info 'actual-failed actuals)
                     (make-check-info 'expected-failed expecteds))
               (lambda () (fail-check)))))))))))
