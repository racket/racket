#lang racket/base

(require racket/list racket/contract racket/match unstable/latent-contract/defthing
         "math.rkt"
         "ticks.rkt"
         "contract.rkt"
         "parameters.rkt"
         "sample.rkt")

(provide (all-defined-out))

(struct plot-element (bounds-rect bounds-fun ticks-fun) #:transparent)
(struct nonrenderer plot-element () #:transparent)
(struct renderer2d plot-element (render-proc) #:transparent)
(struct renderer3d plot-element (render-proc) #:transparent)

(defcontract bounds-fun/c ((vectorof ivl?) . -> . (vectorof ivl?)))
(defcontract ticks-fun/c ((vectorof ivl?) . -> . any))

;; ===================================================================================================
;; Common field values

(defthing default-ticks-fun ticks-fun/c
  (λ (r)
    (match r
      [(vector (ivl xa xb) (ivl ya yb))
       (values ((plot-x-ticks) xa xb) ((plot-x-far-ticks) xa xb)
               ((plot-y-ticks) ya yb) ((plot-y-far-ticks) ya yb))]
      [(vector (ivl xa xb) (ivl ya yb) (ivl za zb))
       (values ((plot-x-ticks) xa xb) ((plot-x-far-ticks) xa xb)
               ((plot-y-ticks) ya yb) ((plot-y-far-ticks) ya yb)
               ((plot-z-ticks) za zb) ((plot-z-far-ticks) za zb))]
      [_  (raise-type-error 'default-ticks-fun "2- or 3-vector of ivl" r)])))

(defproc (function-bounds-fun [f sampler/c] [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (match-define (vector xi yi) r)
    (cond [(ivl-known? xi)
           (match-define (sample xs ys y-min y-max) (f xi samples))
           (vector xi (ivl y-min y-max))]
          [else  r])))

(defproc (inverse-bounds-fun [f sampler/c] [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (match-define (vector xi yi) r)
    (cond [(ivl-known? yi)
           (match-define (sample ys xs x-min x-max) (f yi samples))
           (vector (ivl x-min x-max) yi)]
          [else  r])))

(defproc (function-interval-bounds-fun [f1 sampler/c] [f2 sampler/c]
                                       [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (rect-join ((function-bounds-fun f1 samples) r)
               ((function-bounds-fun f2 samples) r))))

(defproc (inverse-interval-bounds-fun [f1 sampler/c] [f2 sampler/c]
                                      [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (rect-join ((inverse-bounds-fun f1 samples) r)
               ((inverse-bounds-fun f2 samples) r))))

(defproc (surface3d-bounds-fun [f 2d-sampler/c] [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (match-define (vector xi yi zi) r)
    (cond [(and (ivl-known? xi) (ivl-known? yi))
           (match-define (2d-sample xs ys zss z-min z-max)
             (f (vector xi yi) (vector samples samples)))
           (vector xi yi (ivl z-min z-max))]
          [else  r])))

;; ===================================================================================================
;; Fixpoint computation of bounding rectangles

;; The reasoning in the following comments is in terms of a lattice comprised of rectangles,
;; rect-meet and rect-join. Think of rect-meet like a set intersection; rect-join like a set union.

;; Attempts to comptute a fixpoint of, roughly, the bounds functions for the given plot elements.
;; More precisely, starting with the given plot bounds, it attempts to compute a fixpoint of
;; (apply-bounds* elems), overridden at every iteration by the plot bounds (if given). Because a
;; fixpoint doesn't always exist, or only exists in the limit, it stops after max-iters.
(define (bounds-fixpoint elems given-bounds-rect [max-iters 4])
  (let/ec break
    ;; Shortcut eval: if the plot bounds are all given, the code below just returns them anyway
    (when (rect-known? given-bounds-rect) (break given-bounds-rect))
    ;; A list of elements' known bounds rects
    (define elem-bounds-rects (filter values (map plot-element-bounds-rect elems)))
    ;; The minimum bounding rectangle
    (define min-bounds-rect
      (cond [(empty? elem-bounds-rects)  given-bounds-rect]
            [else  (rect-join given-bounds-rect
                              (rect-meet given-bounds-rect
                                         (apply rect-join elem-bounds-rects)))]))
    ;; Objective: find the fixpoint of F starting at min-bounds-rect
    (define (F bounds-rect) (rect-meet given-bounds-rect (apply-bounds* elems bounds-rect)))
    ;; Iterate joint bounds to (hopefully) a fixpoint
    (define-values (bounds-rect area delta-area)
      (for/fold ([bounds-rect  min-bounds-rect]
                 [area  (rect-area min-bounds-rect)]
                 [delta-area  #f]
                 ) ([n  (in-range max-iters)])
        ;(printf "bounds-rect = ~v~n" bounds-rect)
        ;; Get new bounds from the elements' bounds functions
        (define new-bounds-rect (F bounds-rect))
        (define new-area (rect-area new-bounds-rect))
        (define new-delta-area (and area new-area (- new-area area)))
        (cond
          ;; Shortcut eval: if the bounds haven't changed, we have a fixpoint
          [(equal? bounds-rect new-bounds-rect)  (break bounds-rect)]
          ;; If the area grew more this iteration than last, it may not converge, so stop now
          [(and delta-area new-delta-area (new-delta-area . > . delta-area))  (break bounds-rect)]
          ;; All good - one more iteration
          [else  (values new-bounds-rect new-area new-delta-area)])))
    bounds-rect))

;; Applies the bounds functions of multiple plot elements, in parallel, and returns the smallest
;; bounds containing all the new bounds. This function is monotone and increasing regardless of
;; whether any element's bounds function is. If iterating it is bounded, a fixpoint exists.
(define (apply-bounds* elems bounds-rect)
  (apply rect-join bounds-rect (for/list ([elem  (in-list elems)])
                                 (apply-bounds elem bounds-rect))))

;; Applies the plot element's bounds function. Asks this question: If these are your allowed bounds,
;; what bounds will you try to use?
(define (apply-bounds elem bounds-rect)
  (match-define (plot-element elem-bounds-rect elem-bounds-fun _) elem)
  ;(printf "elem-bounds-rect = ~v~n" elem-bounds-rect)
  (let* ([bounds-rect  (if elem-bounds-fun (elem-bounds-fun bounds-rect) bounds-rect)]
         [bounds-rect  (if elem-bounds-rect (rect-join elem-bounds-rect bounds-rect) bounds-rect)])
    bounds-rect))
