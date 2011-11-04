#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/math.rkt")
(provide equal?*
         ;; Flonums
         nan? infinite? special-real?
         flblend flatan2 flsum flmodulo fldistance
         ;; Reals
         regular-real?
         min* max* degrees->radians radians->degrees blend atan2 sum real-modulo distance
         floor-log/base ceiling-log/base
         polar->cartesian 3d-polar->3d-cartesian
         ;; Vectors
         vcross vcross2 v+ v- vneg v* v/ vmag^2 vmag vnormalize vdot vcos-angle vregular? v= vcenter)

;; Intervals
(provide (contract-out (struct ivl ([min (or/c real? #f)] [max (or/c real? #f)]))
                       [ivl-meet  (->* () () #:rest (listof ivl?) ivl?)]
                       [ivl-join  (->* () () #:rest (listof ivl?) ivl?)])
         empty-ivl unknown-ivl regular-ivl?
         (activate-contract-out
          ivl-empty? ivl-known? ivl-regular? ivl-singular? ivl-length ivl-center ivl-zero-length?
          ivl-inexact->exact ivl-contains? bounds->intervals))

;; Rectangles
(provide (contract-out [rect-meet (->* () () #:rest (listof (vectorof ivl?)) (vectorof ivl?))]
                       [rect-join (->* () () #:rest (listof (vectorof ivl?)) (vectorof ivl?))])
         (activate-contract-out
          empty-rect unknown-rect bounding-rect
          rect-empty? rect-known? rect-regular? rect-area rect-center rect-zero-area? rect-singular?
          rect-inexact->exact rect-contains?))
