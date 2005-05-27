(require
 (lib "animation.ss" "frtime")
 (lib "erl.ss" "frtime")
 (all-except (lib "match.ss") match))

(define client (new-cell (make-tid 1179 'frtime-heart)))

(define pos1
  (let ([paddle-radius 20]
        [neg-x (lambda (v) (make-posn (- (posn-x v)) (posn-y v)))]
        [neg-y (lambda (v) (make-posn (posn-x v) (- (posn-y v))))]
        [paddle2-pos (make-posn (clip (posn-x mouse-pos) 230 370) (clip (posn-y mouse-pos) 30 370))]
        [paddle1-pos (switch (left-clicks . ==> .
                                          (lambda (_)
                                            (hold ((remote-reg
                                                    (value-now client)
                                                    'paddle1-pos)
                                                   . ==> .
                                                   (lambda (l) (make-posn (first l) (second l))))
                                                  (make-posn 30 200))))
                             (make-posn 30 200))]
        [collide (match-lambda
                   [(_ mp p)
                    (let ([u (normalize (posn- mp p))])
                      (lambda (v)
                        (posn- v (posn* u (* 2 (posn-dot v u))))))])])
    (letrec ([pos1 (switch
                    ((merge-e
                      (when-e (> (posn-x pos1) 500))
                      (when-e (< (posn-x pos1) -100))
                      (when-e (> (posn-y pos1) 500))
                      (when-e (< (posn-y pos1) -100))) . ==> . (lambda (dummy) (posn+ (make-posn 100 100) (posn-integral vel1))))
                    (posn+ (make-posn 100 100) (posn-integral vel1)))]
             [vel1 (accum-b
                    (merge-e
                     ((merge-e
                       (when-e (> (posn-x pos1) 390))
                       (when-e (< (posn-x pos1) 10))) . -=> . neg-x)
                     ((merge-e
                       (when-e (> (posn-y pos1) 390))
                       (when-e (< (posn-y pos1) 10))) . -=> . neg-y)
                     ((merge-e
                       (snapshot-e (when-e (< (posn-diff pos1 paddle1-pos)
                                              (+ 10 paddle-radius))) paddle1-pos pos1)
                       (snapshot-e (when-e (< (posn-diff pos1 paddle2-pos)
                                              (+ 10 paddle-radius))) paddle2-pos pos1))
                      . ==> . collide))
                    (make-posn .29 .23))])
      (let ([p1-score (accum-b
                       (merge-e
                        ((key #\r) . -=> . (lambda (x) 0))
                        ((snapshot-e (when-e (< (posn-x pos1) 10)) (posn-y pos1))
                         . =#=> .
                         (match-lambda
                           [(_ y) (if (and (> y 150) (< y 250))
                                      add1
                                      nothing)])))
                       0)]
            [p2-score (accum-b
                       (merge-e
                        ((key #\r) . -=> . (lambda (x) 0))
                        ((snapshot-e (when-e (> (posn-x pos1) 390)) (posn-y pos1))
                         . =#=> .
                         (match-lambda
                           [(_ y) (if (and (> y 150) (< y 250))
                                      add1
                                      nothing)])))
                       0)])
        (display-shapes
         (list (make-circle pos1 10 "blue")
               (make-circle paddle1-pos paddle-radius "black")
               (make-circle paddle2-pos paddle-radius "black")
               (make-graph-string (make-posn 30 30) (number->string p2-score) "black")
               (make-graph-string (make-posn 350 30) (number->string p1-score) "black")
               (make-graph-string (make-posn 120 30) (number->string (posn-len vel1)) "black")
               (make-line (make-posn 0 150) (make-posn 0 250) "red")
               (make-line (make-posn 399 150) (make-posn 399 250) "red")))
        (bind 'pong (changes (list (posn-x paddle2-pos) (posn-y paddle2-pos)
                                   (posn-x pos1) (posn-y pos1)
                                   p1-score p2-score)))))))
