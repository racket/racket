#lang scheme/base
(require "model/trace.ss")

#|
(provide expand-only
         expand/hide)

(define (expand-only stx show-list)
  (define (show? id)
    (ormap (lambda (x) (free-identifier=? id x))
           show-list))
  (expand/hiding stx show?))

(define (expand/hide stx hide-list)
  (define (show? id)
    (andmap (lambda (x) (not (free-identifier=? id x)))
            hide-list))
  (expand/hiding stx show?))

(define (expand/hiding stx show?)
  (let-values ([(result deriv) (trace/result stx)])
    (when (exn? result)
      (raise result))
    (let-values ([(_d estx) (hide*/policy deriv show?)])
      estx)))
|#
