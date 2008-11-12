#lang scheme/base
(require scribble/manual
         scribble/eval
         (for-label scheme/base
                    scheme/contract
                    scheme/unit))

(define web-server "Web Server")

(define (warning . x)
  (apply elem (bold "Warning: ") x))

(define (href-link url label)
  (link url label))

(provide (all-from-out scribble/manual)
         (all-from-out scribble/eval)
         (for-label (all-from-out scheme/base
                                  scheme/contract
                                  scheme/unit))
         web-server
         warning
         href-link)
