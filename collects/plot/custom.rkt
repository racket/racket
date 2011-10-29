#lang racket/base

(require racket/contract racket/match racket/list
         "common/contract-doc.rkt"
         "common/format.rkt"
         ;"common/math.rkt"
         ;"common/contract.rkt"
         ;"common/format.rkt"
         ;"common/plot-element.rkt"
         ;"common/area.rkt"
         ;"common/axis-transform.rkt"
         ;"common/utils.rkt"
         ;"common/marching-squares.rkt"
         ;"common/marching-cubes.rkt"
         ;"plot2d/clip.rkt"
         ;"plot3d/clip.rkt"
         )

(require "common/ticks.rkt")
(provide (all-from-out "common/ticks.rkt"))

(require "common/parameters.rkt")
(provide (all-from-out "common/parameters.rkt"))

(require "common/sample.rkt")
(provide (all-from-out "common/sample.rkt"))

(require "common/draw.rkt")
(provide maybe-apply/list)

(require "common/legend.rkt")
(provide (all-from-out "common/legend.rkt"))

(require "common/plot-element.rkt")
(provide (all-from-out "common/plot-element.rkt"))

(require "common/marching-squares.rkt")
(provide (all-from-out "common/marching-squares.rkt"))

(provide (all-defined-out))

(define function->sampler (make-function->sampler plot-x-transform))
(define inverse->sampler (make-function->sampler plot-y-transform))
(define 2d-function->sampler (make-2d-function->sampler plot-x-transform plot-y-transform))
(define 3d-function->sampler
  (make-3d-function->sampler plot-x-transform plot-y-transform plot-z-transform))

(defproc (contour-ticks [z-min real?] [z-max real?]
                        [levels (or/c 'auto exact-positive-integer? (listof real?))]
                        [intervals? boolean?]) (listof tick?)
  (define epsilon (expt 10 (- (digits-for-range z-min z-max))))
  (match-define (ticks layout format) (plot-z-ticks))
  (define ts
    (cond [(eq? levels 'auto)  (filter pre-tick-major? (layout z-min z-max (plot-z-max-ticks)))]
          [else  (define zs (cond [(list? levels)  (filter (λ (z) (<= z-min z z-max)) levels)]
                                  [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
                 (map (λ (z) (pre-tick z #t)) zs)]))
  (define all-ts
    (cond [intervals?
           (let* ([ts  (cond [((abs (- z-min (pre-tick-value (first ts)))) . < . epsilon)  ts]
                             [else  (cons (pre-tick z-min #t) ts)])]
                  [ts  (cond [((abs (- z-max (pre-tick-value (last ts)))) . < . epsilon)  ts]
                             [else  (append ts (list (pre-tick z-max #t)))])])
             ts)]
          [else
           (let* ([ts  (cond [((abs (- z-min (pre-tick-value (first ts)))) . >= . epsilon)  ts]
                             [else  (rest ts)])]
                  [ts  (cond [((abs (- z-max (pre-tick-value (last ts)))) . >= . epsilon)  ts]
                             [else  (take ts (- (length ts) 1))])])
             ts)]))
  (match-define (list (pre-tick zs majors) ...) all-ts)
  (define labels (format z-min z-max all-ts))
  (map tick zs majors labels))
