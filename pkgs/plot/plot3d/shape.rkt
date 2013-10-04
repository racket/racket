#lang racket/base

(require racket/match
         "../common/math.rkt")

(provide (all-defined-out))

(struct shape (alpha center layer) #:transparent)

(struct polygon shape (vs normal pen-color pen-width pen-style brush-color brush-style) #:transparent)
(struct rectangle shape (rect pen-color pen-width pen-style brush-color brush-style) #:transparent)
(struct line shape (v1 v2 pen-color pen-width pen-style) #:transparent)
(struct text shape (anchor angle dist str font-size font-family color outline?) #:transparent)
(struct glyph shape (symbol size pen-color pen-width pen-style brush-color brush-style) #:transparent)
(struct tick-glyph shape (radius angle pen-color pen-width pen-style) #:transparent)
(struct arrow-glyph shape (start end pen-color pen-width pen-style) #:transparent)
(struct shapes shape (list) #:transparent)

(define (draw-before? cs1 cs2)
  (match-define (cons s1 (vector x1 y1 z1)) cs1)
  (match-define (cons s2 (vector x2 y2 z2)) cs2)
  (define l1 (shape-layer s1))
  (define l2 (shape-layer s2))
  (or (l1 . > . l2)
      (and (l1 . = . l2)
           (or (y1 . > . y2)
               (and (y1 . = . y2)
                    (if (z1 . = . z2)
                        (and (polygon? s1) (not (polygon? s2)))
                        (z1 . < . z2)))))))

(define (depth-sort s+cs)
  (sort s+cs draw-before?))
