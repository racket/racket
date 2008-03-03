#lang scheme/base


(provide (struct-out binding)
         (struct-out def-binding)
         (struct-out def-stx-binding))

(define-struct binding (name) #:inspector #f)
(define-struct (def-binding binding) (ty) #:inspector #f)
(define-struct (def-stx-binding binding) () #:inspector #f)