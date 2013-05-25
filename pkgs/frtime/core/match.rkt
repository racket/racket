#lang racket/base

(define-struct a-match-fail ())
(define match-fail (make-a-match-fail))

(provide match-fail)
