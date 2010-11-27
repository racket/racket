#lang racket/base
(require scribble/eval
         scriblib/gui-eval)

(provide (rename-out [gui-interaction mr-interaction]
                     [gui-interaction-eval mr-interaction-eval]
                     [gui-racketmod+eval mr-racketmod+eval]
                     [gui-racketblock+eval mr-racketblock+eval]
                     [gui-def+int mr-def+int]
                     [gui-defs+int mr-defs+int]
                     [gui-interaction-eval-show mr-interaction-eval-show]))

(define ss-eval (make-base-eval))
(void (interaction-eval #:eval ss-eval (require slideshow/pict)))

(define-syntax-rule (ss-interaction e ...)
  (interaction #:eval ss-eval e ...))
(define-syntax-rule (ss-interaction-eval e ...)
  (interaction-eval #:eval ss-eval e ...))
(define-syntax-rule (ss-racketmod+eval e ...)
  (racketmod+eval #:eval ss-eval e ...))
(define-syntax-rule (ss-racketblock+eval e ...)
  (racketblock+eval #:eval ss-eval e ...))
(define-syntax-rule (ss-def+int e ...)
  (def+int #:eval ss-eval e ...))

(provide ss-interaction
         ss-interaction-eval
         ss-racketmod+eval
         ss-racketblock+eval
         ss-def+int)

