#lang racket/base

(require racket/unit
         "sendmail-sig.rkt" "sendmail.rkt")

(define-unit-from-context sendmail@ sendmail^)

(provide sendmail@)
