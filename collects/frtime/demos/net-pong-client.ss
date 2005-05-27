(require (lib "animation.ss" "frtime")
         (lib "erl.ss" "frtime"))

(define server (new-cell (make-tid 1178 'frtime-heart)))

(define pos1
  (let* ([paddle-radius 20]
         [paddle1-pos (make-posn (clip (posn-x mouse-pos) 30 170) (clip (posn-y mouse-pos) 30 370))]
         [_ (bind 'paddle1-pos ((changes paddle1-pos) . ==> . (lambda (p) (list (posn-x p) (posn-y p)))))]
         [pong (switch (left-clicks
                        . ==> .
                        (lambda (_)
                          (hold (remote-reg (value-now server) 'pong)
                                (list 300 300 100 100 0 0))))
                       (list 300 300 100 100 0 0))]
         [paddle2-pos (make-posn (first pong) (second pong))]
         [pos1 (make-posn (third pong) (fourth pong))]
         [p1-score (list-ref pong 4)]
         [p2-score (list-ref pong 5)])
    (display-shapes
     (list (make-circle pos1 10 "blue")
           (make-circle paddle1-pos paddle-radius "black")
           (make-circle paddle2-pos paddle-radius "black")
           (make-graph-string (make-posn 30 30) (number->string p2-score) "black")
           (make-graph-string (make-posn 350 30) (number->string p1-score) "black")
           (make-line (make-posn 0 150) (make-posn 0 250) "red")
           (make-line (make-posn 399 150) (make-posn 399 250) "red")))))
