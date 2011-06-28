#lang racket/base

(require "../private/typed-renaming.rkt")
(provide renamer)

(define (renamer id #:alt [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))
