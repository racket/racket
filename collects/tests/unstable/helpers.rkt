#lang racket

(provide test
         test-ok check-ok
         test-bad check-bad
         check-not
         with/c)

(require rackunit racket/pretty)

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
