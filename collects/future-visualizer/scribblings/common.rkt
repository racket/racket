#lang racket/base

(require (for-label racket/base)
         scribble/manual
         scribble/decode)

(provide (for-label (all-from-out racket/base))
         (all-from-out scribble/manual)
         guideintro
         refsecref)

(define (refsecref s)
  (secref #:doc '(lib "scribblings/reference/reference.scrbl") s))

(define (guidesecref s)
  (secref #:doc '(lib "scribblings/guide/guide.scrbl") s))

(define Guide
  (other-manual '(lib "scribblings/guide/guide.scrbl")))

(define (guideintro tag . s)
  (apply margin-note
         (decode-content (append (list (guidesecref tag) " in " Guide " introduces ")))))
