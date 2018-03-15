#lang racket/base
(require "../compile/serialize-property.rkt")

(provide (struct-out provided)
         provided-as-binding
         provided-as-protected?
         provided-as-transformer?
         
         deserialize-provided)

;; Wrapper for provides that are protected or syntax
(struct provided (binding protected? syntax?)
  #:authentic
  #:transparent
  #:property prop:serialize
  (lambda (p ser-push! state)
    (ser-push! 'tag '#:provided)
    (ser-push! (provided-binding p))
    (ser-push! (provided-protected? p))
    (ser-push! (provided-syntax? p))))

(define (provided-as-binding v)
  (if (provided? v) (provided-binding v) v))
(define (provided-as-protected? v)
  (and (provided? v) (provided-protected? v)))
(define (provided-as-transformer? v)
  (and (provided? v) (provided-syntax? v)))

(define (deserialize-provided binding protected? syntax?)
  (provided binding protected? syntax?))
