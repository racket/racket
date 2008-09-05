#lang scheme/base
(require "model/trace.ss"
         "model/reductions-config.ss"
         "model/reductions.ss")

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
    (let-values ([(_steps _uses stx _exn)
                  (parameterize ((macro-policy show?))
                    (reductions+ deriv))])
      stx)))
