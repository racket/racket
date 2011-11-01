#lang racket/base

(require racket/list racket/contract racket/match
         "math.rkt"
         "ticks.rkt"
         "contract.rkt"
         "contract-doc.rkt"
         "parameters.rkt"
         "sample.rkt")

(provide (all-defined-out))

(struct plot-element (bounds-rect bounds-fun ticks-fun) #:transparent)
(struct non-renderer plot-element () #:transparent)
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
       (values (default-x-ticks xa xb) (default-x-far-ticks xa xb)
               (default-y-ticks ya yb) (default-y-far-ticks ya yb))]
      [(vector (ivl xa xb) (ivl ya yb) (ivl za zb))
       (values (default-x-ticks xa xb) (default-x-far-ticks xa xb)
               (default-y-ticks ya yb) (default-y-far-ticks ya yb)
             (default-z-ticks za zb) (default-z-far-ticks za zb))]
      [_  (raise-type-error 'default-ticks-fun "2- or 3-vector of ivl" r)])))

(defproc (function-bounds-fun [f sampler/c] [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (match-define (vector xi yi) r)
    (cond [(ivl-known? xi)
           (match-define (ivl x-min x-max) xi)
           (match-define (list xs ys) (f x-min x-max samples))
           (define rys (filter regular? ys))
           (cond [(not (empty? rys))  (vector xi (ivl (apply min* rys) (apply max* rys)))]
                 [else  r])]
          [else  r])))

(defproc (inverse-bounds-fun [f sampler/c] [samples exact-nonnegative-integer?]) bounds-fun/c
  (λ (r)
    (match-define (vector xi yi) r)
    (cond [(ivl-known? yi)
           (match-define (ivl y-min y-max) yi)
           (match-define (list ys xs) (f y-min y-max samples))
           (define rxs (filter regular? xs))
           (cond [(not (empty? rxs))  (vector (ivl (apply min* rxs) (apply max* rxs)) yi)]
                 [else  r])]
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
           (match-define (ivl x-min x-max) xi)
           (match-define (ivl y-min y-max) yi)
           (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
           (define zs (filter regular? (2d-sample->list zss)))
           (cond [(not (empty? zs)) (vector xi yi (ivl (apply min* zs) (apply max* zs)))]
                 [else  r])]
          [else  r])))

;; ===================================================================================================
;; Fixpoint computation of bounding rectangles

;; The reasoning in the following comments is in terms of a lattice comprised of rectangles,
;; rect-meet and rect-join. Think of rect-meet like a set intersection; rect-join like a set union.

;; Attempts to comptute a fixpoint of, roughly, the bounds functions for the given plot elements.
;; More precisely, starting with the given plot bounds, it attempts to compute a fixpoint of
;; (apply-bounds* elems), overridden at every iteration by the plot bounds (if given). Because a
;; fixpoint doesn't always exist, or only exists in the limit, it stops after max-iters.
(define (bounds-fixpoint elems plot-bounds-rect [max-iters 4])
  (let/ec break
    ;; Shortcut eval: if the plot bounds are all known, the code below just returns them anyway
    (when (rect-known? plot-bounds-rect) (break plot-bounds-rect))
    ;; Objective: find the fixpoint of F starting at plot-bounds-rect
    (define (F bounds-rect) (rect-meet plot-bounds-rect (apply-bounds* elems bounds-rect)))
    ;; Iterate joint bounds to (hopefully) a fixpoint
    (for/fold ([bounds-rect plot-bounds-rect]) ([n  (in-range max-iters)])
      ;(printf "bounds-rect = ~v~n" bounds-rect)
      ;; Get new bounds from the elements' bounds functions
      (define new-bounds-rect (F bounds-rect))
      ;; Shortcut eval: if the bounds haven't changed, we have a fixpoint
      (cond [(equal? bounds-rect new-bounds-rect)  (break bounds-rect)]
            [else  new-bounds-rect]))))

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
  (let ([elem-bounds-rect  (cond [elem-bounds-rect  (rect-meet bounds-rect elem-bounds-rect)]
                                 [else  bounds-rect])])
    (cond [elem-bounds-fun  (elem-bounds-fun elem-bounds-rect)]
          [else  elem-bounds-rect])))
