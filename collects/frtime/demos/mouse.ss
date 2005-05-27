(require (lib "animation.ss" "frtime"))

(display-shapes
 (list
  (make-circle mouse-pos 10 "blue")
  (make-graph-string (make-posn 20 20)
                     (format "(~a, ~a)"
                             (posn-x mouse-pos)
                             (posn-y mouse-pos))
                     "black")))