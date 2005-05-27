(module push-pull-ball (lib "frtime.ss" "frtime")
  
  (require (lib "etc.ss" "frtime")
           (lib "animation.ss" "frtime"))
  
  (define radius (new-cell 20))
  
  (define pos1
    (rec pos
      (until (make-posn 200 200)
             (if (> (posn-diff pos mouse-pos) radius)
                 (posn+ pos
                        (posn* (normalize (posn- mouse-pos pos))
                               (- (posn-diff pos mouse-pos) (sub1 radius))))
                 pos))))

  (define pos2
    (rec pos
      (until (make-posn 100 100)
             (if (< (posn-diff pos pos1) (* 2 radius))
                 (posn+ pos
                        (posn* (normalize (posn- pos1 pos))
                               (- (posn-diff pos pos1) (add1 (* 2 radius)))))
                 pos))))

  (display-shapes
   (list
    (make-circle pos1 radius "blue")
    (make-circle pos2 radius "blue"))))
