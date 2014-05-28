#lang racket/base
(require "private/main.rkt"
         racket/contract
         racket/class
         racket/draw)
(provide 
 (except-out (all-from-out "private/main.rkt")
             pict->bitmap
             pict->argb-pixels
             argb-pixels->pict
             colorize
             pin-under pin-over disk)
 (contract-out
  [colorize (-> pict? 
                (or/c string? 
                      (is-a?/c color%)
                      (list/c byte? byte? byte?))
                pict?)]
                
  [pict->bitmap (->* (pict?)
                     ((or/c 'unsmoothed 'smoothed 'aligned))
                     (is-a?/c bitmap%))]
  [pict->argb-pixels (->* (pict?) 
                          ((or/c 'unsmoothed 'smoothed 'aligned))
                          (and/c bytes? multiple-of-four-bytes?))]
  [argb-pixels->pict (-> (and/c bytes? multiple-of-four-bytes?) 
                         exact-nonnegative-integer?
                         pict?)]
  [pin-under
   (->i ([base pict?]
         [dx/fp (or/c real? pict?)]
         [dy/f (dx/fp)
               (if (real? dx/fp)
                   real?
                   (-> pict? pict? (values real? real?)))]
         [pict pict?])
        [result pict?])]
  [pin-over
   (->i ([base pict?]
         [dx/fp (or/c real? pict?)]
         [dy/f (dx/fp)
               (if (real? dx/fp)
                   real?
                   (-> pict? pict? (values real? real?)))]
         [pict pict?])
        [result pict?])]
  [disk (->* ((and/c rational? (not/c negative?))) (#:draw-border? any/c) pict?)]))

(define (multiple-of-four-bytes? b)
  (zero? (modulo (bytes-length b) 4)))
  
(require "private/play-pict.rkt")
(provide
 (contract-out
  [fade-pict (->* ((real-in 0.0 1.0) pict? pict?) (#:combine (-> pict? pict? pict?)) pict?)]
  [slide-pict (-> pict? pict? pict? pict? (real-in 0.0 1.0) pict?)]
  [fade-around-pict (-> (real-in 0.0 1.0) pict? (-> pict? pict?) pict?)]
  [sequence-animations (->* () #:rest (listof (-> (real-in 0.0 1.0) pict?))
                            (-> (real-in 0.0 1.0) pict?))]
  [reverse-animations (->* () #:rest (listof (-> (real-in 0.0 1.0) pict?))
                           (-> (real-in 0.0 1.0) pict?))]
  [fast-start (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-end (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-edges (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-middle (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [split-phase (-> (real-in 0.0 1.0) (values (real-in 0.0 1.0) (real-in 0.0 1.0)))]))
