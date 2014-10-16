#lang racket/base

(provide syntax-local-match-introduce
         current-match-introducer)

(define current-match-introducer
  (make-parameter
   (lambda (x)
     (error 'syntax-local-match-introduce "not expanding match expander form"))))

(define (syntax-local-match-introduce x)
  (unless (syntax? x)
    (raise-argument-error 'syntax-local-match-introduce "syntax?" x))
  ((current-match-introducer) x))

