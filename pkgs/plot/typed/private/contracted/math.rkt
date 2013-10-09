#lang typed/racket/base

(require "../common/types.rkt")

;; Misc
(require/typed/provide
 plot/private/common/math
 [polar->cartesian  (Real Real -> (Vector Real Real))]
 [3d-polar->3d-cartesian  (Real Real Real -> (Vector Real Real Real))]
 [ceiling-log/base  (Integer Real -> Integer)]
 [floor-log/base  (Integer Real -> Integer)]
 [maybe-inexact->exact  ((U #f Real) -> (U #f Real))]
 )

;; Vectors
(require/typed/provide
 plot/private/common/math
 [v+  ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [v-  ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [vneg  ((Vectorof Real) -> (Vectorof Real))]
 [v*  ((Vectorof Real) Real -> (Vectorof Real))]
 [v/  ((Vectorof Real) Real -> (Vectorof Real))]
 [v=  ((Vectorof Real) (Vectorof Real) -> Boolean)]
 [vcross  ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [vcross2  ((Vectorof Real) (Vectorof Real) -> Real)]
 [vdot  ((Vectorof Real) (Vectorof Real) -> Real)]
 [vmag^2  ((Vectorof Real) -> Real)]
 [vnormalize  ((Vectorof Real) -> (Vectorof Real))]
 [vcenter  ((Listof (Vectorof Real)) -> (Vectorof Real))]
 [vrational?  ((Vectorof Real) -> Boolean)]
 [vcos-angle  ((Vectorof Real) (Vectorof Real) -> Real)]
 )

;; Intervals
(require/typed/provide
 plot/private/common/math
 [empty-ivl  ivl]
 [unknown-ivl  ivl]
 [rational-ivl?  (Any -> Boolean)]
 [ivl-empty?  (ivl -> Boolean)]
 [ivl-known?  (ivl -> Boolean)]
 [ivl-rational?  (ivl -> Boolean)]
 [ivl-singular?  (ivl -> Boolean)]
 [ivl-length  (ivl -> (U #f Real))]
 [ivl-center  (ivl -> (U #f Real))]
 [ivl-zero-length?  (ivl -> Boolean)]
 [ivl-inexact->exact  (ivl -> ivl)]
 [ivl-contains?  (ivl Real -> Boolean)]
 [ivl-meet  (ivl * -> ivl)]
 [ivl-join  (ivl * -> ivl)]
 [ivl-translate  (ivl Real -> ivl)]
 [bounds->intervals  ((Listof Real) -> (Listof ivl))]
 [clamp-real  (Real ivl -> Real)]
 )

(define-type Rect (Vectorof ivl))
(provide Rect)

;; Rectangles
(require/typed/provide
 plot/private/common/math
 [empty-rect  (Natural -> Rect)]
 [unknown-rect  (Natural -> Rect)]
 [bounding-rect  ((Listof Rect) -> Rect)]
 [rect-empty?  (Rect -> Boolean)]
 [rect-known?  (Rect -> Boolean)]
 [rect-rational?  (Rect -> Boolean)]
 [rational-rect?  (Any -> Boolean)]
 [rect-area  (Rect -> (U #f Real))]
 [rect-center  (Rect -> (Vectorof Real))]
 [rect-zero-area?  (Rect -> Boolean)]
 [rect-singular?  (Rect -> Boolean)]
 [rect-inexact->exact  (Rect -> Rect)]
 [rect-contains?  (Rect (Vectorof Real) -> Boolean)]
 [rect-meet  (Rect * -> Rect)]
 [rect-join  (Rect * -> Rect)]
 [rect-translate  (Rect (Vectorof Real) -> Rect)]
 )
