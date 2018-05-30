#lang racket/base
(require racket/system
         compiler/find-exe)

;; Make sure that `-e` interleaves expansion and evaluation

(define o (open-output-bytes))
(parameterize ([current-output-port o])
  (unless (system* (find-exe) "-e" "(begin (define-syntax-rule (m) 10) (m))")
    (error "run failed")))
(unless (equal? "10" (read-line (open-input-bytes (get-output-bytes o))))
  (error "output failed"))
