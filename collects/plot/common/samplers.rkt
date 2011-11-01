#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         "parameters.rkt"
         "sample.rkt"
         "ticks.rkt"
         "format.rkt"
         "contract-doc.rkt")

(provide (all-defined-out))

(defthing function->sampler ((real? . -> . real?) . -> . sampler/c)
  (make-function->sampler plot-x-transform))

(defthing inverse->sampler ((real? . -> . real?) . -> . sampler/c)
  (make-function->sampler plot-y-transform))

(defthing 2d-function->sampler ((real? real? . -> . real?) . -> . 2d-sampler/c)
  (make-2d-function->sampler plot-x-transform plot-y-transform))

(defthing 3d-function->sampler ((real? real? real? . -> . real?) . -> . 3d-sampler/c)
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
