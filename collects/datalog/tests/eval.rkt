#lang racket
(require rackunit
         racket/runtime-path
         "../parse.rkt"
         "../eval.rkt")

(provide eval-tests)

(define-runtime-path examples-dir "examples")
(define (test-example t)
  (define test-ss (build-path examples-dir (format "~a.rkt" t)))
  (define test-txt (build-path examples-dir (format "~a.txt" t)))
  (test-equal? t
               (filter (lambda (l)
                         (not (string=? l "")))
                       (file->lines test-txt))
               (filter (lambda (l)
                         (not (string=? l "")))
                       (with-input-from-string
                        (with-output-to-string
                         (lambda () (dynamic-require test-ss #f)))
                        port->lines))))

(define eval-tests
  (test-suite
   "eval"
   
   (test-example "ancestor")
   (test-example "bidipath")
   (test-example "laps")
   (test-example "long")
   (test-example "path")
   (test-example "pq")
   (test-example "revpath")
   (test-example "says")
   (test-example "true")
   (test-example "tutorial")
   
   ))