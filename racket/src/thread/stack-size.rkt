#lang racket/base
(require "check.rkt")

(provide current-thread-initial-stack-size)

;; This parameter doesn't do anything, but it's provided
;; here for compatibility
(define/who current-thread-initial-stack-size
  (make-parameter 64
                  (lambda (v)
                    (check who exact-positive-integer? v)
                    v)))
