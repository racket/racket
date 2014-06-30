#lang racket/base
(require scribble/base
         scribble/manual)
(provide language-info-ref
         language-info-def)

(define-syntax-rule 
  (language-info-ref id)
  (language-info-ref/proc (racket id) 'id))

(define (language-info-ref/proc rkt-id sym)
  (define str (format "~s" sym))
  (elemref str rkt-id))

(define-syntax-rule
  (language-info-def id . args)
  (language-info-def/proc 'id . args))

(define (language-info-def/proc sym arg0 . args)
  (define str (format "~s" sym))
  (list (elemtag str arg0) args))

