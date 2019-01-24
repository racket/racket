#lang racket/base

(require scribble/manual
         scribble/struct
         scribble/decode
         (only-in "../inside/utils.rkt" cpp)
         (for-syntax racket/base)
         scribble/racket
         (for-label racket/base
                    racket/contract
                    (except-in ffi/unsafe ->)
                    ffi/unsafe/cvector
                    ffi/vector
                    (only-in ffi/unsafe [-> ->>])))

(provide cpp
         InsideRacket InsideRacket-doc
         guide.scrbl
         reference.scrbl
         ->>
         tech-place
         3m CGC CS
         (all-from-out scribble/manual)
         (for-label (all-from-out racket/base
                                  racket/contract
                                  ffi/unsafe
                                  ffi/unsafe/cvector
                                  ffi/vector)))

(define InsideRacket-doc '(lib "scribblings/inside/inside.scrbl"))

(define InsideRacket
  (other-manual InsideRacket-doc))

(define guide.scrbl
  '(lib "scribblings/guide/guide.scrbl"))

(define reference.scrbl
  '(lib "scribblings/reference/reference.scrbl"))

(define-syntax ->>
  (make-element-id-transformer
    (lambda (stx)
      #'(racketlink ->> #:style "plainlink" (racketkeywordfont "->")))))

(define (tech-place)
  (tech "place" #:doc '(lib "scribblings/reference/reference.scrbl")))

(define (CGC) (tech #:doc guide.scrbl "CGC"))
(define (3m) (tech #:doc guide.scrbl "3m"))
(define (CS) (tech #:doc guide.scrbl "CS"))
