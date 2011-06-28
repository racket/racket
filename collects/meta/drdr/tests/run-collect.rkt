#lang racket
(require "../run-collect.rkt" 
         "../status.rkt"
         racket/runtime-path
         tests/eli-tester)

(define-runtime-path loud-file "loud.rkt")

(define (run-loud n)
  (run/collect/wait #:env (hash)
                    #:timeout (* 10)
                    (path->string (find-system-path 'exec-file))
                    (list "-t" (path->string loud-file)
                          "--" (number->string n))))

(define (test-run-loud n)
  (test
   #:failure-prefix (number->string n)
   (status-output-log (run-loud n))
   =>
   (for/list ([i (in-range n)])
     ((if (even? i)
          make-stderr
          make-stdout)
      (string->bytes/utf-8
       (number->string i))))))

(test
 (for ([n (in-range 10)])
   (test-run-loud n)))

(run-loud 10)
