#lang racket/base

(require racket/unit
         "sendmail-sig.rkt" net/sendmail)

(define-unit-from-context sendmail@ sendmail^)

(provide sendmail@)
