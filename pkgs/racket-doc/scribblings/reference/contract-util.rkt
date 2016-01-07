#lang racket/base
(require scribble/manual)

(provide add-use-sources declare-exporting-ctc)

(define-syntax-rule
  (add-use-sources (x y ...))
  (x y ...
     #:use-sources
     (racket/contract/private/base
      racket/contract/private/misc
      racket/contract/private/provide
      racket/contract/private/guts
      racket/contract/private/prop
      racket/contract/private/blame
      racket/contract/private/ds
      racket/contract/private/opt
      racket/contract/private/basic-opters
      
      racket/contract/private/box
      racket/contract/private/hash
      racket/contract/private/vector
      racket/contract/private/struct-dc)))

(define-syntax-rule
  (declare-exporting-ctc mod)
  (add-use-sources (declare-exporting mod racket/contract racket)))
