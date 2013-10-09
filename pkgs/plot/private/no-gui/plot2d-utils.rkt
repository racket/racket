#lang racket/base

(require racket/list racket/class racket/match
         "../common/math.rkt"
         "../common/plot-element.rkt"
         "../common/format.rkt")

(provide (all-defined-out))

(define (get-renderer-list renderer-tree)
  (for/list ([r  (flatten (list renderer-tree))])
    (match r
      [(nonrenderer bounds-rect bounds-fun ticks-fun)
       (renderer2d bounds-rect bounds-fun ticks-fun #f)]
      [_  r])))

(define (get-bounds-rect renderer-list x-min x-max y-min y-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  (define plot-bounds-rect (bounds-fixpoint renderer-list given-bounds-rect))
  (when (or (not (rect-rational? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector x-ivl y-ivl) plot-bounds-rect)
    (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a"
           (ivl->plot-label x-ivl) (ivl->plot-label y-ivl)))
  (rect-inexact->exact plot-bounds-rect))

(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks)
    (for/lists (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks
                            ) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else       (values empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))))

(define (plot-area area renderer-list)
  (send area start-plot)
  
  (define legend-entries
    (flatten (for/list ([rend  (in-list renderer-list)])
               (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
               (send area start-renderer (if rend-bounds-rect
                                             (rect-inexact->exact rend-bounds-rect)
                                             (unknown-rect 2)))
               (if render-proc (render-proc area) empty))))
  
  (send area end-renderers)
  
  (when (not (empty? legend-entries))
    (send area draw-legend legend-entries))
  
  (send area end-plot))

