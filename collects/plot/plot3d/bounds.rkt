#lang racket/base

(require racket/match racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/sample.rkt"
         "renderer.rkt")

(provide renderer3d-bounds-fixpoint
         surface3d-bounds-fun)

;; ===================================================================================================
;; Fixpoint computation of bounds for multiple renderers

;; Attempts to comptute a fixpoint of, roughly, the bounds functions for the given renderers.
;; More precisely, starting with the given plot bounds, it attempts to compute a fixpoint of
;; apply-bounds*, overridden at every iteration by the plot bounds (if given).
;; Because a fixpoint doesn't always exist, or may only exist in the limit, it stops after max-iters.
(define (renderer3d-bounds-fixpoint renderers plot-x-min plot-x-max plot-y-min plot-y-max
                                    plot-z-min plot-z-max [max-iters 2])
  (let/ec break
    ;; Shortcut eval: if the plot bounds are all specified, the code below just returns them
    (when (and plot-x-min plot-x-max plot-y-min plot-y-max plot-z-min plot-z-max)
      (break plot-x-min plot-x-max plot-y-min plot-y-max plot-z-min plot-z-max))
    ;; Iterate joint bounds to (hopefully) a fixed point
    (for/fold ([x-min plot-x-min]
               [x-max plot-x-max]
               [y-min plot-y-min]
               [y-max plot-y-max]
               [z-min plot-z-min]
               [z-max plot-z-max]) ([n  (in-range max-iters)])
      ;(printf "bounds = ~v ~v ~v ~v ~v ~v~n" x-min x-max y-min y-max z-min z-max)
      ;; Get new bounds from the renderers' bounds functions
      (define-values (new-x-min new-x-max new-y-min new-y-max new-z-min new-z-max)
        (let-values ([(new-x-min new-x-max new-y-min new-y-max new-z-min new-z-max)
                      (apply-bounds* renderers x-min x-max y-min y-max z-min z-max)])
          ;; Override by plot bounds
          (values (if plot-x-min plot-x-min new-x-min) (if plot-x-max plot-x-max new-x-max)
                  (if plot-y-min plot-y-min new-y-min) (if plot-y-max plot-y-max new-y-max)
                  (if plot-z-min plot-z-min new-z-min) (if plot-z-max plot-z-max new-z-max))))
      ;; Shortcut eval: if the bounds haven't changed, another iteration won't change them
      (cond [(and (equal? new-x-min x-min) (equal? new-x-max x-max)
                  (equal? new-y-min y-min) (equal? new-y-max y-max)
                  (equal? new-z-min z-min) (equal? new-z-max z-max))
             (break new-x-min new-x-max new-y-min new-y-max new-z-min new-z-max)]
            [else  (values new-x-min new-x-max new-y-min new-y-max new-z-min new-z-max)]))))

;; Applies the bounds functions of multiple renderers, in parallel. Returns the smallest rectangle
;; that contains all the new bounds.
;; This function is monotone because renderer2d-apply-bounds is monotone. If iterating it is bounded,
;; a fixed point exists.
(define (apply-bounds* renderers x-min x-max y-min y-max z-min z-max)
  (define-values (x-mins x-maxs y-mins y-maxs z-mins z-maxs)
    (for/lists (x-mins x-maxs y-mins y-maxs z-mins z-maxs) ([renderer  (in-list renderers)])
      (renderer3d-apply-bounds renderer x-min x-max y-min y-max z-min z-max)))
  (values (apply maybe-min x-mins) (apply maybe-max x-maxs)
          (apply maybe-min y-mins) (apply maybe-max y-maxs)
          (apply maybe-min z-mins) (apply maybe-max z-maxs)))

;; ===================================================================================================
;; Bounds functions

(define ((surface3d-bounds-fun f samples) x-min x-max y-min y-max z-min z-max)
  (cond [(and x-min x-max y-min y-max)
         (match-define (list xs ys zss) (f x-min x-max samples y-min y-max samples))
         (define zs (filter regular? (2d-sample->list zss)))
         (cond [(empty? zs)  (values x-min x-max y-min y-max z-min z-max)]
               [else  (values x-min x-max y-min y-max (apply min* zs) (apply max* zs))])]
        [else  (values x-min x-max y-min y-max z-min z-max)]))
