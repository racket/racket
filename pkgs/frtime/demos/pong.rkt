#lang frtime
(require
 frtime/animation
 frtime/gui
 (only-in racket/match match-lambda))

(define paddle-radius (make-slider "Paddle radius" 10 30 20))
(define key-control-speed (* 0.01 (make-slider "Key control speed" 1 50 25)))

(define (neg-x p)
  (make-posn (- (posn-x p)) (posn-y p)))

(define (neg-y p)
  (make-posn (posn-x p) (- (posn-y p))))

(define paddle2-pos
  (make-posn (clip (posn-x mouse-pos) 230 370) (clip (posn-y mouse-pos) 30 370)))

(define (collide paddle-pos ball-pos)
  (let ([u (normalize (posn- paddle-pos ball-pos))])
    (lambda (v)
      (posn- v (posn* u (* 2 (posn-dot v u)))))))

(define-values (paddle1-pos ball-pos ball-vel)
  (letrec ([paddle1-pos (make-posn
                         (clip (+ 150
                                  (integral (hold
                                             (merge-e
                                              (key-strokes
                                               . =#=> .
                                               (lambda (key)
                                                 (snapshot (paddle1-pos key-control-speed)
                                                   (let ([x (posn-x paddle1-pos)])
                                                     (case key
                                                       [(release) 0]
                                                       [(numpad4) (when (> x 30) (- key-control-speed))]
                                                       [(numpad1 numpad7) (when (> x 30) (- (/ key-control-speed (sqrt 2))))]
                                                       [(numpad6) (when (< x 170) key-control-speed)]
                                                       [(numpad3 numpad9) (when (< x 170) (/ key-control-speed (sqrt 2)))])))))
                                              ((when-e (>= (posn-x paddle1-pos) 170)) . -=> . 0)
                                              ((when-e (<= (posn-x paddle1-pos)  30)) . -=> . 0))
                                             0)))
                               30 170)
                         (clip (+ 150
                                  (integral (hold
                                             (merge-e
                                              (key-strokes
                                               . =#=> .
                                               (lambda (key)
                                                 (snapshot (paddle1-pos key-control-speed)
                                                   (let ([y (posn-y paddle1-pos)])
                                                     (case key
                                                       [(release) 0]
                                                       [(numpad8) (when (> y 30) (- key-control-speed))]
                                                       [(numpad7 numpad9) (when (> y 30) (- (/ key-control-speed (sqrt 2))))]
                                                       [(numpad2) (when (< y 370) key-control-speed)]
                                                       [(numpad1 numpad3) (when (< y 370) (/ key-control-speed (sqrt 2)))])))))
                                              ((when-e (>= (posn-y paddle1-pos) 370)) . -=> . 0)
                                              ((when-e (<= (posn-y paddle1-pos)  30)) . -=> . 0))
                                             0)))
                               30 370))]
           [pos1 (inf-delay
                  (switch
                   ((merge-e
                     (when-e (> (posn-x pos1) 500))
                     (when-e (< (posn-x pos1) -100))
                     (when-e (> (posn-y pos1) 500))
                     (when-e (< (posn-y pos1) -100))) . -=> . (posn+ (make-posn 100 100) (posn-integral vel1)))
                   (posn+ (make-posn 200 200) (posn-integral vel1))))]
           [vel1 (accum-b
                  (merge-e
                   ((when-e (> (posn-x pos1) 390)) . -=> . neg-x)
                   ((when-e (< (posn-x pos1)  10)) . -=> . neg-x)
                   ((when-e (> (posn-y pos1) 390)) . -=> . neg-y)
                   ((when-e (< (posn-y pos1)  10)) . -=> . neg-y)
                   (map-e (lambda (_)
                            (snapshot/apply collide paddle1-pos pos1))
                          (when-e (< (posn-diff pos1 paddle1-pos)
                                     (+ 10 paddle-radius))))
                   (map-e (lambda (_)
                            (snapshot/apply collide paddle2-pos pos1))
                          (when-e (< (posn-diff pos1 paddle2-pos)
                                     (+ 10 paddle-radius)))))
                  (make-posn .29 .23))])
    (values paddle1-pos pos1 vel1)))

(define (mk-score x-pred)
  (accum-b
   (merge-e
    ((key #\r) . -=> . (lambda (x) 0))
    ((snapshot-e (when-e (x-pred (posn-x ball-pos))) (posn-y ball-pos))
     . =#=> .
     (match-lambda
       [(list _ y) (when (and (> y 150) (< y 250))
                     add1)])))
   0))

(define p1-score (mk-score (lambda (x) (< x 10))))
(define p2-score (mk-score (lambda (x) (> x 390))))

(display-shapes
 (list 
  (make-line (make-posn 200 0) (make-posn 200 399) "gray")
  (make-circle ball-pos 10 "blue")
  (make-circle paddle1-pos paddle-radius "black")
  (make-circle paddle2-pos paddle-radius "black")
  (make-graph-string (make-posn 30 30) (number->string p2-score)
                     (if (= p2-score (delay-by p2-score 600)) "black" "red" "black"))
  (make-graph-string (make-posn 350 30) (number->string p1-score)
                     (if (= p1-score (delay-by p1-score 600)) "black" "red" "black"))
  (make-line (make-posn 0 150) (make-posn 0 250) "red")
  (make-line (make-posn 399 150) (make-posn 399 250) "red")))
