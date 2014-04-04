#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/math.rkt")
(provide equal?*
         ;; Flonums
         flblend flsum fldistance
         ;; Reals
         maybe-inexact->exact
         min* max* blend atan2 sum real-modulo distance
         floor-log/base ceiling-log/base
         polar->cartesian 3d-polar->3d-cartesian
         ;; Vectors
         vcross vcross2 v+ v- vneg v* v/ vmag^2 vmag vnormalize vdot vcos-angle vrational? v= vcenter)

;; Intervals
(provide (contract-out (struct ivl ([min (or/c real? #f)] [max (or/c real? #f)]))
                       [ivl-meet  (->* () () #:rest (listof ivl?) ivl?)]
                       [ivl-join  (->* () () #:rest (listof ivl?) ivl?)])
         empty-ivl unknown-ivl rational-ivl?
         (activate-contract-out
          ivl-empty? ivl-known? ivl-rational? ivl-singular? ivl-length ivl-center ivl-zero-length?
          ivl-inexact->exact ivl-contains? ivl-translate bounds->intervals clamp-real))

;; Rectangles
(provide (contract-out [rect-meet (->* () () #:rest (listof (vectorof ivl?)) (vectorof ivl?))]
                       [rect-join (->* () () #:rest (listof (vectorof ivl?)) (vectorof ivl?))])
         (activate-contract-out
          empty-rect unknown-rect bounding-rect rational-rect?
          rect-empty? rect-known? rect-rational? rect-area rect-center rect-zero-area? rect-singular?
          rect-inexact->exact rect-translate rect-contains?))
