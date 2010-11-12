#lang at-exp racket/base
(require scribble/base scribble/manual scribble/core scribble/eval)
(provide eval/require)

(define (eval/require . paths)
  (let* ([e (make-base-eval)])
    (for ([path (in-list paths)])
      (e `(require ,path)))
    e))
