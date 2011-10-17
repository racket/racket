#lang racket/base

(require racket/list racket/match racket/contract
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/math.rkt"
         "../common/ticks.rkt"
         "../common/parameters.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; 3D plot renderers

(struct renderer3d (render-proc ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max)
  #:transparent
  #:guard
  (λ (render-proc ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max _)
    (when (and x-min x-max (x-min . > . x-max))
      (error 'renderer3d "expected x-min <= x-max; got x-min = ~e and x-max = ~e" x-min x-max))
    (when (and y-min y-max (y-min . > . y-max))
      (error 'renderer3d "expected y-min <= y-max; got y-min = ~e and y-max = ~e" y-min y-max))
    (when (and z-min z-max (z-min . > . z-max))
      (error 'renderer3d "expected z-min <= z-max; got z-min = ~e and z-max = ~e" z-min z-max))
    (values render-proc ticks-fun bounds-fun x-min x-max y-min y-max z-min z-max)))

(define (null-3d-render-proc area) empty)
(define (null-3d-ticks-fun x-min x-max y-min y-max z-min z-max) (values empty empty empty))
(define (null-3d-bounds-fun x-min x-max y-min y-max z-min z-max)
  (values x-min x-max y-min y-max z-min z-max))

(define null-renderer3d
  (renderer3d null-3d-render-proc null-3d-ticks-fun null-3d-bounds-fun #f #f #f #f #f #f))

;; ===================================================================================================
;; Bounds functions

(define (renderer3d-out-of-bounds? renderer x-min x-max y-min y-max z-min z-max)
  (match-define (renderer3d _1 _2 _3 rx-min rx-max ry-min ry-max rz-min rz-max) renderer)
  (or (and rx-max x-min (rx-max . < . x-min)) (and rx-min x-max (rx-min . > . x-max))
      (and ry-max y-min (ry-max . < . y-min)) (and ry-min y-max (ry-min . > . y-max))
      (and rz-max z-min (rz-max . < . z-min)) (and rz-min z-max (rz-min . > . z-max))))

;; Applies the renderer's bounds function, if the renderer's bounds intersect the given bounds.
;; This function is monotone regardless of whether the bounds function is monotone.
(define (renderer3d-apply-bounds renderer x-min x-max y-min y-max z-min z-max)
  (cond
    [(renderer3d-out-of-bounds? renderer x-min x-max y-min y-max z-min z-max)
     (values x-min x-max y-min y-max z-min z-max)]
    [else
     (match-define (renderer3d _1 _2 bounds-fun rx-min rx-max ry-min ry-max rz-min rz-max) renderer)
     (define-values (new-x-min new-x-max new-y-min new-y-max new-z-min new-z-max)
       (bounds-fun (maybe-max rx-min x-min) (maybe-min rx-max x-max)
                   (maybe-max ry-min y-min) (maybe-min ry-max y-max)
                   (maybe-max rz-min z-min) (maybe-min rz-max z-max)))
     (values (maybe-min x-min new-x-min) (maybe-max x-max new-x-max)
             (maybe-min y-min new-y-min) (maybe-max y-max new-y-max)
             (maybe-min z-min new-z-min) (maybe-max z-max new-z-max))]))

;; ===================================================================================================
;; Tick functions

(defproc (default-3d-ticks-fun
           [x-min real?] [x-max real?]
           [y-min real?] [y-max real?]
           [z-min real?] [z-max real?]
           ) (values (listof tick?) (listof tick?) (listof tick?))
  (values (default-x-ticks x-min x-max)
          (default-y-ticks y-min y-max)
          (default-z-ticks z-min z-max)))
