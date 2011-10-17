#lang racket/base

(require racket/list racket/match racket/contract
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/math.rkt"
         "../common/ticks.rkt"
         "../common/parameters.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; 2D plot renderers

(struct renderer2d (render-proc ticks-fun bounds-fun x-min x-max y-min y-max) #:transparent
  #:guard
  (λ (render-proc ticks-fun bounds-fun x-min x-max y-min y-max _)
    (when (and x-min x-max (x-min . > . x-max))
      (error 'renderer2d "expected x-min <= x-max; got x-min = ~e and x-max = ~e" x-min x-max))
    (when (and y-min y-max (y-min . > . y-max))
      (error 'renderer2d "expected y-min <= y-max; got y-min = ~e and y-max = ~e" y-min y-max))
    (values render-proc ticks-fun bounds-fun x-min x-max y-min y-max)))

(define (null-2d-render-proc area) empty)
(define (null-2d-ticks-fun x-min x-max y-min y-max) (values empty empty))
(define (null-2d-bounds-fun x-min x-max y-min y-max) (values x-min x-max y-min y-max))

(define null-renderer2d
  (renderer2d null-2d-render-proc null-2d-ticks-fun null-2d-bounds-fun #f #f #f #f))

;; ===================================================================================================
;; Bounds functions

(define (renderer2d-out-of-bounds? renderer x-min x-max y-min y-max)
  (match-define (renderer2d _1 _2 _3 rx-min rx-max ry-min ry-max) renderer)
  (or (and rx-max x-min (rx-max . < . x-min)) (and rx-min x-max (rx-min . > . x-max))
      (and ry-max y-min (ry-max . < . y-min)) (and ry-min y-max (ry-min . > . y-max))))

;; Applies the renderer's bounds function, if the renderer's bounds intersect the given bounds.
;; This function is monotone regardless of whether the bounds function is monotone.
(define (renderer2d-apply-bounds renderer x-min x-max y-min y-max)
  (cond [(renderer2d-out-of-bounds? renderer x-min x-max y-min y-max)
         (values x-min x-max y-min y-max)]
        [else
         (match-define (renderer2d _1 _2 bounds-fun rx-min rx-max ry-min ry-max) renderer)
         (define-values (new-x-min new-x-max new-y-min new-y-max)
           (bounds-fun (maybe-max rx-min x-min) (maybe-min rx-max x-max)
                       (maybe-max ry-min y-min) (maybe-min ry-max y-max)))
         (values (maybe-min x-min new-x-min) (maybe-max x-max new-x-max)
                 (maybe-min y-min new-y-min) (maybe-max y-max new-y-max))]))

;; ===================================================================================================
;; Tick functions

(defproc (default-2d-ticks-fun [x-min real?] [x-max real?] [y-min real?] [y-max real?]
           ) (values (listof tick?) (listof tick?))
  (values (default-x-ticks x-min x-max)
          (default-y-ticks y-min y-max)))
