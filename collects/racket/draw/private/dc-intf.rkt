#lang scheme/base
(require scheme/class
         "font.ss")

(provide dc<%>)

(define dc<%>
  (interface ()
    draw-text))
