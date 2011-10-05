#lang racket/base

(require racket/match
         "../common/vector.rkt"
         "../common/math.rkt")

(provide (all-defined-out))

(struct shape (alpha center) #:transparent)

(struct polygon shape (vs pen-color pen-width pen-style brush-color brush-style) #:transparent)
(struct line shape (v1 v2 pen-color pen-width pen-style) #:transparent)
(struct text shape (anchor angle str font-size font-family color) #:transparent)
(struct glyph shape (symbol size pen-color pen-width pen-style brush-color brush-style) #:transparent)
(struct tick-glyph shape (radius angle pen-color pen-width pen-style) #:transparent)
(struct shapes shape (list) #:transparent)

(define (shape-normal s)
  (cond [(polygon? s)  (surface-normal (polygon-vs s))]
        [else          default-normal]))

(define (shape-coords s)
  (cond [(polygon? s)  (polygon-vs s)]
        [(line? s)     (list (line-v1 s) (line-v2 s))]
        [else          (list (shape-center s))]))

(define (draw-before? s1 s2)
  (match-define (vector x1 y1 z1) (shape-center s1))
  (match-define (vector x2 y2 z2) (shape-center s2))
  (or (y1 . > . y2)
      (and (y1 . = . y2)
           (if (z1 . = . z2)
               (and (polygon? s1) (not (polygon? s2)))
               (z1 . < . z2)))))

(define (depth-sort shapes)
  (sort shapes draw-before?))
