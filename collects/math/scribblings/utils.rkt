#lang at-exp racket/base

(require scribble/eval
         scribble/manual)

(provide author-neil
         author-jens-axel
         make-plain-math-eval
         make-math-eval
         make-untyped-math-eval)

(define (author-neil)
  @author{@(author+email "Neil Toronto" "ntoronto@racket-lang.org")})

(define (author-jens-axel)
  @author{@(author+email "Jens Axel Søgaard" "jensaxel@soegaard.net")})

(define (make-plain-math-eval)
  (define eval (make-base-eval))
  (eval '(require typed/racket/base))
  (eval '(require math))
  eval)

(define (make-math-eval)
  (define eval (make-plain-math-eval))
  (eval '(require math/scribblings/rename-defines))
  (λ (v)
    (cond [(syntax? v)  (eval #`(rename-defines #,v))]
          [(list? v)  (eval `(rename-defines ,v))]
          [else  (eval v)])))

(define (make-untyped-math-eval)
  (define eval (make-base-eval))
  (eval '(require math))
  (eval '(require (rename-in (except-in plot plot plot3d)
                             [plot-pict  plot]
                             [plot3d-pict  plot3d])))
  eval)
