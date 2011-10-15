#lang racket/base

(require racket/match racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "renderer.rkt")

(provide renderer2d-bounds-fixpoint
         function-bounds-fun
         inverse-bounds-fun
         function-interval-bounds-fun
         inverse-interval-bounds-fun)

;; ===================================================================================================
;; Fixpoint computation of bounds for multiple renderers

;; Attempts to comptute a fixpoint of, roughly, the bounds functions for the given renderers.
;; More precisely, starting with the given plot bounds, it attempts to compute a fixpoint of
;; apply-bounds*, overridden at every iteration by the plot bounds (if given).
;; Because a fixpoint doesn't always exist, or may only exist in the limit, it stops after max-iters.
(define (renderer2d-bounds-fixpoint renderers plot-x-min plot-x-max plot-y-min plot-y-max
                                    [max-iters 2])
  (let/ec break
    ;; Shortcut eval: if the plot bounds are all specified, the code below just returns them
    (when (and plot-x-min plot-x-max plot-y-min plot-y-max)
      (break plot-x-min plot-x-max plot-y-min plot-y-max))
    ;; Iterate joint bounds to (hopefully) a fixed point
    (for/fold ([x-min plot-x-min]
               [x-max plot-x-max]
               [y-min plot-y-min]
               [y-max plot-y-max]) ([n  (in-range max-iters)])
      ;(printf "bounds = ~v ~v ~v ~v~n" x-min x-max y-min y-max)
      ;; Get new bounds from the renderers' bounds functions
      (define-values (new-x-min new-x-max new-y-min new-y-max)
        (let-values ([(new-x-min new-x-max new-y-min new-y-max)
                      (apply-bounds* renderers x-min x-max y-min y-max)])
          ;; Override by plot bounds
          (values (if plot-x-min plot-x-min new-x-min) (if plot-x-max plot-x-max new-x-max)
                  (if plot-y-min plot-y-min new-y-min) (if plot-y-max plot-y-max new-y-max))))
      ;; Shortcut eval: if the bounds haven't changed, another iteration won't change them
      (cond [(and (equal? new-x-min x-min) (equal? new-x-max x-max)
                  (equal? new-y-min y-min) (equal? new-y-max y-max))
             (break new-x-min new-x-max new-y-min new-y-max)]
            [else  (values new-x-min new-x-max new-y-min new-y-max)]))))

;; Applies the bounds functions of multiple renderers, in parallel. Returns the smallest rectangle
;; that contains all the new bounds.
;; This function is monotone because renderer2d-apply-bounds is monotone. If iterating it is bounded,
;; a fixed point exists.
(define (apply-bounds* renderers x-min x-max y-min y-max)
  (define-values (x-mins x-maxs y-mins y-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs) ([renderer  (in-list renderers)])
      (renderer2d-apply-bounds renderer x-min x-max y-min y-max)))
  (values (apply maybe-min x-mins) (apply maybe-max x-maxs)
          (apply maybe-min y-mins) (apply maybe-max y-maxs)))

;; ===================================================================================================
;; Bounds functions

(define ((function-bounds-fun f samples) x-min x-max y-min y-max)
  (cond [(and x-min x-max)  (match-define (list xs ys) (f x-min x-max samples))
                            (define rys (filter regular? ys))
                            (cond [(empty? rys)  (values x-min x-max y-min y-max)]
                                  [else  (values x-min x-max (apply min* rys) (apply max* rys))])]
        [else  (values x-min x-max y-min y-max)]))

(define ((inverse-bounds-fun f samples) x-min x-max y-min y-max)
  (cond [(and y-min y-max)  (match-define (list ys xs) (f y-min y-max samples))
                            (define rxs (filter regular? xs))
                            (cond [(empty? rxs)  (values x-min x-max y-min y-max)]
                                  [else  (values (apply min* rxs) (apply max* rxs) y-min y-max)])]
        [else  (values x-min x-max y-min y-max)]))

(define ((function-interval-bounds-fun f1 f2 samples) x-min x-max y-min y-max)
  (cond [(and x-min x-max)
         (match-define (list x1s y1s) (f1 x-min x-max samples))
         (match-define (list x2s y2s) (f2 x-min x-max samples))
         (define rys (filter regular? (append y1s y2s)))
         (cond [(empty? rys)  (values x-min x-max y-min y-max)]
               [else  (values x-min x-max (apply min* rys) (apply max* rys))])]
        [else  (values x-min x-max y-min y-max)]))

(define ((inverse-interval-bounds-fun f1 f2 samples) x-min x-max y-min y-max)
  (cond [(and y-min y-max)
         (match-define (list y1s x1s) (f1 y-min y-max samples))
         (match-define (list y2s x2s) (f2 y-min y-max samples))
         (define rxs (filter regular? (append x1s x2s)))
         (cond [(empty? rxs)  (values x-min x-max y-min y-max)]
               [else  (values (apply min* rxs) (apply max* rxs) y-min y-max)])]
        [else  (values x-min x-max y-min y-max)]))
