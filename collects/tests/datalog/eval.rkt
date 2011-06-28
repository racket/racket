#lang racket
(require rackunit
         racket/runtime-path
         datalog/parse
         datalog/eval)

(provide eval-tests)

(define-runtime-path here ".")

(define (test-examples examples-dir)
  
  (define (test-example t)
    (define test-rkt (build-path examples-dir (format "~a.rkt" t)))
    (define test-txt (build-path examples-dir (format "~a.txt" t)))
    (test-equal? t
                 (with-input-from-string
                     (with-output-to-string
                         (lambda () (dynamic-require test-rkt #f)))
                   port->lines)
                 (file->lines test-txt)))
  
  (define (test-files d)
    (for ([f (in-list (directory-list d))]
          #:when (regexp-match #rx"rkt$" (path->bytes f)))
      (test-example (path->string (path-replace-suffix f #"")))))
  
  (test-suite
   (path->string examples-dir)
   
   (test-files examples-dir)))

(define eval-tests
  (test-suite
   "eval"
   
   (test-examples (build-path here "examples"))
   (test-examples (build-path here "paren-examples"))))
