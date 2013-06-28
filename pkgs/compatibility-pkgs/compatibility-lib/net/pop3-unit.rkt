#lang racket/base

(require racket/unit
         "pop3-sig.rkt" net/pop3)

(define-unit-from-context pop3@ pop3^)

(provide pop3@)
