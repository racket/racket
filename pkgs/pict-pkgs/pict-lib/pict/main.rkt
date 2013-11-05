#lang racket/base
(require "private/main.rkt"
         racket/contract
         racket/class
         racket/draw)
(provide 
 (except-out (all-from-out "private/main.rkt")
                     pict->bitmap)
 (contract-out 
  [pict->bitmap (->* (pict?)
                     ((or/c 'unsmoothed 'smoothed 'aligned))
                     (is-a?/c bitmap%))]))
  
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
