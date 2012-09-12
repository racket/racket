#lang racket/base

(require scribble/eval)

(provide make-math-eval
         make-untyped-math-eval)

(define (make-math-eval)
  (define eval (make-base-eval))
  (eval '(require typed/racket/base))
  (eval '(require math))
  (eval '(require math/scribblings/rename-defines))
  (λ (v)
    (cond [(syntax? v)  (eval #`(rename-defines #,v))]
          [(list? v)  (eval `(rename-defines ,v))]
          [else  (eval v)])))

(define (make-untyped-math-eval)
  (define eval (make-base-eval))
  (eval '(require math))
  (eval '(require (rename-in (except-in plot plot plot3d)
                             [plot-bitmap  plot]
                             [plot3d-bitmap  plot3d])))
  eval)
