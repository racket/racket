#lang racket/base

(require racket/list racket/contract unstable/latent-contract/defthing
         "math.rkt"
         "contract.rkt"
         "ticks.rkt"
         "plot-element.rkt")

(provide (all-defined-out))

(define ((x-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 2)  (values ts far-ts empty empty)]
          [(= (vector-length r) 3)  (values ts far-ts empty empty empty empty)]
          [else  (raise-type-error 'x-ticks-fun "2- or 3-vector of ivls" r)])))

(define ((y-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 2)  (values empty empty ts far-ts)]
          [(= (vector-length r) 3)  (values empty empty ts far-ts empty empty)]
          [else  (raise-type-error 'y-ticks-fun "2- or 3-vector of ivls" r)])))

(define ((z-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 3)  (values empty empty empty empty ts far-ts)]
          [else  (raise-type-error 'z-ticks-fun "3-vector of ivls" r)])))

(defproc (x-ticks [ts (listof tick?)] [#:far? far? boolean? #f]) nonrenderer?
  (nonrenderer #f #f (x-ticks-fun ts far?)))

(defproc (y-ticks [ts (listof tick?)] [#:far? far? boolean? #f]) nonrenderer?
  (nonrenderer #f #f (y-ticks-fun ts far?)))

(defproc (z-ticks [ts (listof tick?)] [#:far? far? boolean? #f]) nonrenderer?
  (nonrenderer #f #f (z-ticks-fun ts far?)))

(defproc (invisible-rect [x-min (or/c rational? #f)] [x-max (or/c rational? #f)]
                         [y-min (or/c rational? #f)] [y-max (or/c rational? #f)]
                         ) nonrenderer?
  (nonrenderer (vector (ivl x-min x-max) (ivl y-min y-max)) #f #f))

(defproc (invisible-rect3d [x-min (or/c rational? #f)] [x-max (or/c rational? #f)]
                           [y-min (or/c rational? #f)] [y-max (or/c rational? #f)]
                           [z-min (or/c rational? #f)] [z-max (or/c rational? #f)]
                           ) nonrenderer?
  (nonrenderer (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f #f))
