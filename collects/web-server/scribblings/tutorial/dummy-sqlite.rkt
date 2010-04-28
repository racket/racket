#lang racket/base

(define sqlite:db? #f)
(define sqlite:open #f)
(define sqlite:exec/ignore #f)
(define sqlite:select #f)
(define sqlite:insert #f)

(provide (all-defined-out))

