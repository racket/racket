#lang racket/base

(provide test)

;; test: (lambda (a?) ((a? a? . -> . boolean?) a? a? . -> . (void))
;; tests to see if the expression is true and prints and error if it's not
(define-syntax test
  (syntax-rules (identity)
    [(_ = actual expected)
     (let ([result (with-handlers ([exn? (λ (x) x)]) actual)])
       (unless (and (not (exn? result)) (= result expected))
         (eprintf "test failed: ~s != ~s\n" result expected)))]))
