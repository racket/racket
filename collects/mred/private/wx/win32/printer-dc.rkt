#lang racket/base
(require racket/class
         racket/draw/dc)

(provide printer-dc%)

(define dc-backend%
  (class default-dc-backend%
    (init [parent #f])
    
    (super-new)))

(define printer-dc%
  (dc-mixin dc-backend%))
