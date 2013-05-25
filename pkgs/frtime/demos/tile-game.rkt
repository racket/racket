;; tile game by Dave Tucker
#lang frtime
(require frtime/animation
         frtime/gui
         racket/class)

(define-struct tile (row col num color))
(define-struct state (tiles blank-row blank-col))

(define hue (list-ref '(0 1 4 5) (make-choice "Color: " '("Black" "Blue" "Red" "Magenta"))))
(define reverse-keys? (make-check-box "Reverse keys? "))
(define animate? (make-check-box "Animate? " #t))
(define animation-speed (list-ref '(250 500 750) (make-choice "Animation speed: " '("Fast" "Medium" "Slow"))))
(define smoothness (list-ref '(10 20 50) (make-choice "Animation smoothness: " '("Smooth" "Normal" "Coarse"))))

(set-cell! fine-timer-granularity smoothness)

(define ((component i) bit)
  (if (not (zero? (bitwise-and hue bit)))
      1
      (/ i 16)))

(define initial-tiles
  (build-list 15
              (lambda (i)
                (make-tile (quotient i 4)
                           (remainder i 4)
                           i
                           (apply make-rgb (map (component i) '(4 2 1)))))))

(define config
  (accum-b (merge-e
            (left-clicks
             . ==> .
             (lambda (ev)
               (lambda (st)
                 (let* ([r (quotient (send ev get-y) 100)]
                        [c (quotient (send ev get-x) 100)]
                        [st1 (caar st)]
                        [br (state-blank-row st1)]
                        [bc (state-blank-col st1)])
                   (cond
                     [(and (= r br) (not (= c bc)))
                      `((,(make-state
                           (let ([dir (quotient (- c bc) (abs (- c bc)))])
                             (map (lambda (t)
                                    (if (and (= (tile-row t) r)
                                             (or (<= (+ bc dir) (tile-col t) c)
                                                 (<= c (tile-col t) (+ bc dir))))
                                        (make-tile (tile-row t)
                                                   (- (tile-col t) dir)
                                                   (tile-num t)
                                                   (tile-color t))
                                        t))
                                  (state-tiles st1)))
                           r
                           c)
                         ,(value-now milliseconds))
                        ,(first st))]
                     [(and (not (= r br)) (= c bc))
                      `((,(make-state
                           (let ([dir (quotient (- r br) (abs (- r br)))])
                             (map (lambda (t)
                                    (if (and (= (tile-col t) c)
                                             (or (<= (+ dir br) (tile-row t) r)
                                                 (<= r (tile-row t) (+ dir br))))
                                        (make-tile (- (tile-row t) dir)
                                                   (tile-col t)
                                                   (tile-num t)
                                                   (tile-color t))
                                        t))
                                  (state-tiles st1)))
                           r
                           c)
                         ,(value-now milliseconds))
                        ,(first st))]
                     [#t st])))))
              (key-strokes
               . ==> .
               (lambda (ev)
                 (lambda (st)
                   (let/ec k
                     (let*-values ([(st1) (caar st)]
                                   [(br) (state-blank-row st1)]
                                   [(bc) (state-blank-col st1)]
                                   [(r c)
                                    (let ([ev (if (value-now reverse-keys?)
                                                  (case ev
                                                    [(left) 'right]
                                                    [(right) 'left]
                                                    [(up)    'down]
                                                    [(down)    'up])
                                                  ev)])
                                      (case ev
                                        [(left) (if (< bc 3)
                                                    (values br (add1 bc))
                                                    (k st))]
                                        [(right) (if (> bc 0)
                                                     (values br (sub1 bc))
                                                     (k st))]
                                        [(up) (if (< br 3)
                                                  (values (add1 br) bc)
                                                  (k st))]
                                        [(down) (if (> br 0)
                                                    (values (sub1 br) bc)
                                                    (k st))]
                                        [else (k st)]))])
                       `((,(make-state (map (lambda (t)
                                              (if (and (= (tile-row t) r) (= (tile-col t) c))
                                                  (make-tile br bc (tile-num t) (tile-color t))
                                                  t))
                                            (state-tiles st1))
                                       r
                                       c)
                          ,(value-now milliseconds))
                         ,(first st))))))))
             (let ([init-state (make-state initial-tiles 3 3)]
                   [init-time (- (value-now milliseconds) 1000)])
               (list (list init-state init-time)
                     (list init-state init-time)))))

(define (tile->shape t)
  (make-rect (make-posn (+ 1 (* (tile-col t) 100))
                        (+ 1 (* (tile-row t) 100)))
             98
             98
             (tile-color t)))

(define (tile-pos t)
  (make-posn (+ 1 (* (tile-col t) 100))
             (+ 1 (* (tile-row t) 100))))

(define (linear-comb x1 x2 frac)
  (+ (* frac x1) (* (- 1 frac) x2)))

(define (blend-posns p1 p2 frac)
  (make-posn (linear-comb (posn-x p1) (posn-x p2) frac)
             (linear-comb (posn-y p1) (posn-y p2) frac)))

(define (tiles->shape t0 t1 frac)
  (make-rect (blend-posns (tile-pos t0) (tile-pos t1) frac)
             98
             98
             (tile-color t0)))

(display-shapes
 (if animate?
     (map (lambda (t1 t2)
            (tiles->shape t1 t2 (min 1 (/ (- milliseconds (cadar config))
                                          animation-speed))))
          (state-tiles (caar config))
          (state-tiles (caadr config)))
     (map tile->shape (state-tiles (caar config)))))
