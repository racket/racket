#lang racket/base

(require racket/unit
         "pop3-sig.rkt" "pop3.rkt")

(define-unit-from-context pop3@ pop3^)

(provide pop3@)
