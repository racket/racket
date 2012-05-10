#lang racket/base
(require scribble/manual
         scribble/eval
         (for-syntax racket/base)
         (for-label racket/base
                    racket/contract
                    racket/unit))

(define web-server "Web Server")

(define (warning . x)
  (apply elem (bold "Warning: ") x))

(define (href-link url label)
  (link url label))

(define-syntax (a-dispatcher stx)
  (syntax-case stx ()
   [(_ lib-name lib-desc . rest)
    ;; This macro plays a standard trick for limiting the scope of
    ;; `require'd bindings: it puts the require and the scope of the
    ;; require into a macro, which introduces both together
    #'(begin
       (define-syntax-rule (intro)
         ((... ...)
          (begin
            (require (for-label lib-name))
            (defmodule lib-name
                       "The " (racketmodname lib-name) " module " lib-desc)
            . rest)))
       (intro))]))

(provide (all-from-out scribble/manual)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket/base
                                  racket/contract
                                  racket/unit))
         a-dispatcher
         web-server
         warning
         href-link)
