#lang racket/base
(require scribble/eval)

(provide image-examples)

(define img-eval (make-base-eval))
(interaction-eval #:eval img-eval (require 2htdp/image))

(define-syntax-rule 
  (image-examples exp ...)
  (examples #:eval img-eval exp ...))
