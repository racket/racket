#lang racket/base

(require racket/list racket/contract racket/match
         "math.rkt"
         "vector.rkt"
         "contract.rkt"
         "contract-doc.rkt"
         "parameters.rkt"
         "sample.rkt")

(provide (all-defined-out))

(struct renderer (bounds-rect bounds-fun ticks-fun) #:transparent)
(struct renderer2d renderer (render-proc) #:transparent)
(struct renderer3d renderer (render-proc) #:transparent)

;; ===================================================================================================
;; Common field values

(define (null-bounds-fun r) r)
(define (null-ticks-fun r) (apply values (make-list (vector-length r) empty)))
(define (null-render-proc area) empty)

(define null-renderer2d (renderer2d (unknown-rect 2) null-bounds-fun null-ticks-fun null-render-proc))
(define null-renderer3d (renderer3d (unknown-rect 3) null-bounds-fun null-ticks-fun null-render-proc))

(define (default-ticks-fun r)
  (apply values (for/list ([i  (in-vector r)]
                           [f  (in-list (list default-x-ticks default-y-ticks default-z-ticks))])
                  (match-define (ivl a b) i)
                  (f a b))))

(define ((function-bounds-fun f samples) r)
  (match-define (vector xi yi) r)
  (cond [(ivl-known? xi)
         (match-define (ivl x-min x-max) xi)
         (match-define (list xs ys) (f x-min x-max samples))
         (define rys (filter regular? ys))
         (cond [(not (empty? rys))  (vector xi (ivl (apply min* rys) (apply max* rys)))]
               [else  r])]
        [else  r]))

(define ((inverse-bounds-fun f samples) r)
  (match-define (vector xi yi) r)
  (cond [(ivl-known? yi)
         (match-define (ivl y-min y-max) yi)
         (match-define (list ys xs) (f y-min y-max samples))
         (define rxs (filter regular? xs))
         (cond [(not (empty? rxs))  (vector (ivl (apply min* rxs) (apply max* rxs)) yi)]
               [else  r])]
        [else  r]))

(define ((function-interval-bounds-fun f1 f2 samples) r)
  (rect-join ((function-bounds-fun f1 samples) r)
             ((function-bounds-fun f2 samples) r)))

(define ((inverse-interval-bounds-fun f1 f2 samples) r)
  (rect-join ((inverse-bounds-fun f1 samples) r)
             ((inverse-bounds-fun f2 samples) r)))

(define ((surface3d-bounds-fun f samples) r)
  (match-define (vector xi yi zi) r)
  (cond [(and (ivl-known? xi) (ivl-known? yi))
         (match-define (ivl x-min x-max) xi)
         (match-define (ivl y-min y-max) yi)
         (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
         (define zs (filter regular? (2d-sample->list zss)))
         (cond [(not (empty? zs)) (vector xi yi (ivl (apply min* zs) (apply max* zs)))]
               [else  r])]
        [else  r]))

;; ===================================================================================================
;; Fixpoint computation of bounding rectangles

;; The reasoning in the following comments is in terms of a lattice comprised of rectangles,
;; rect-meet and rect-join. Think of rect-meet like a set intersection; rect-join like a set union.

;; Attempts to comptute a fixpoint of, roughly, the bounds functions for the given renderers.
;; More precisely, starting with the given plot bounds, it attempts to compute a fixpoint of
;; (renderer-apply-bounds* rs), overridden at every iteration by the plot bounds (if given).
;; Because a fixpoint doesn't always exist, or only exists in the limit, it stops after max-iters.
(define (renderer-bounds-fixpoint rends plot-bounds-rect [max-iters 4])
  (let/ec break
    ;; Shortcut eval: if the plot bounds are all known, the code below just returns them anyway
    (when (rect-known? plot-bounds-rect) (break plot-bounds-rect))
    ;; Objective: find the fixpoint of F (meeted with plot-bounds-rect) starting at plot-bounds-rect
    (define F (renderer-apply-bounds* rends))
    ;; Iterate joint bounds to (hopefully) a fixpoint
    (for/fold ([bounds-rect plot-bounds-rect]) ([n  (in-range max-iters)])
      ;(printf "bounds-rect = ~v~n" bounds-rect)
      ;; Get new bounds from the renderers' bounds functions, limit them to plot bounds (when given)
      (define new-bounds-rect (rect-meet plot-bounds-rect (F bounds-rect)))
      ;; Shortcut eval: if the bounds haven't changed, we have a fixpoint
      (cond [(equal? bounds-rect new-bounds-rect)  (break bounds-rect)]
            [else  new-bounds-rect]))))

;; Applies the bounds functions of multiple renderers, in parallel, and returns the smallest bounds
;; containing all the new bounds. This function is monotone and increasing regardless of whether any
;; renderer's bounds function is. If iterating it is bounded, a fixpoint exists.
(define ((renderer-apply-bounds* rends) bounds-rect)
  (apply rect-join bounds-rect (for/list ([rend  (in-list rends)])
                                 (renderer-apply-bounds rend bounds-rect))))

;; Applies the renderer's bounds function. Asks this question: If these are your allowed bounds, what
;; bounds will you try to draw in?
(define (renderer-apply-bounds rend bounds-rect)
  (match-define (renderer rend-bounds-rect rend-bounds-fun _) rend)
  (rend-bounds-fun (rect-meet bounds-rect rend-bounds-rect)))
