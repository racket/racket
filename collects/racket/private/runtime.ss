#lang scheme/base
(require mzlib/pconvert)

(provide configure)

(define (configure config)
  (current-prompt-read  (lambda ()
                          (printf "r> ")
                          (read)))
  (print-as-quasiquote #t))
