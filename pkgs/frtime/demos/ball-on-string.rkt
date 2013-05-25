#lang frtime
(require frtime/animation)

(define-values (pos vel)
 (letrec ([pos (posn-integral vel)]
          [vel (posn/ (posn- mouse-pos (inf-delay pos)) 400.0)])
   (values pos vel)))

(display-shapes
 (list
  (make-line mouse-pos pos "gray")
  (make-circle pos (+ 10 (/ 400 (+ 40.0 (posn-diff mouse-pos pos)))) "blue")))
