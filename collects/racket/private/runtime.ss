#lang scheme/base
(require mzlib/pconvert)

(provide configure)

(define (configure config)
  (print-as-quasiquote #t))
