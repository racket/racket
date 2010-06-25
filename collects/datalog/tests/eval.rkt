#lang racket
(require rackunit
         racket/runtime-path
         "../parse.rkt"
         "../eval.rkt")

(provide eval-tests)

(define-runtime-path here ".")

(define (test-examples examples-dir)
  
  (define (test-example t)
    (define test-rkt (build-path examples-dir (format "~a.rkt" t)))
    (define test-txt (build-path examples-dir (format "~a.txt" t)))
    (test-equal? t
                 (filter (lambda (l)
                           (not (string=? l "")))
                         (with-input-from-string
                             (with-output-to-string
                                 (lambda () (dynamic-require test-rkt #f)))
                           port->lines))
                 (filter (lambda (l)
                           (not (string=? l "")))
                         (file->lines test-txt))
                 ))
  
  (test-suite
   (path->string examples-dir)
   
   (test-example "ancestor")
   (test-example "bidipath")
   (test-example "laps")
   (test-example "long")
   (test-example "path")
   (test-example "pq")
   (test-example "revpath")
   (test-example "says")
   (test-example "true")
   (test-example "tutorial")))

(define eval-tests
  (test-suite
   "eval"
   
   (test-examples (build-path here "examples"))
   (test-examples (build-path here "paren-examples"))))