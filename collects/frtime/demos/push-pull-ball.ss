(require (lib "etc.ss" "frtime")
         (lib "animation.ss" "frtime"))

(define radius (new-cell 20))

(define pos1
  (rec pos
    (until (make-posn 200 200)
           (delay-by
            (let ([brnch (posn+ pos
                                (posn* (normalize (posn- mouse-pos pos))
                                       (- (posn-diff pos mouse-pos) (sub1 radius))))])
              (if (> (posn-diff pos mouse-pos) radius)
                  brnch
                  pos))
            0))))

(define pos2
  (rec pos
    (until (make-posn 100 100)
           (delay-by
            (let ([brnch (posn+ pos
                                (posn* (normalize (posn- pos1 pos))
                                       (- (posn-diff pos pos1) (add1 (* 2 radius)))))])
              (if (< (posn-diff pos pos1) (* 2 radius))
                  brnch
                  pos))
            0))))

(display-shapes
 (list
  (make-circle pos1 radius "blue")
  (make-circle pos2 radius "blue")))
