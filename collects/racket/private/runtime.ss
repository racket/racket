#lang scheme/base

(provide configure)

(define (configure config)
  (current-prompt-read  (lambda ()
                          (printf "> ")
                          (read)))
  (print-as-quasiquote #t))
