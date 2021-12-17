#lang racket/base
(require "../compile/serialize-property.rkt")

(provide (struct-out like-ambiguous-binding))

(struct like-ambiguous-binding ()
  #:authentic
  #:transparent
  #:property prop:serialize
  (lambda (b ser-push! state)
    (ser-push! 'tag '#:like-ambiguous-binding)))
