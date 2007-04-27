;; testing world 

;; World = Nat 

(define world0 10)

(define (world->image w)
  (place-image (circle 3 'solid 'red)
               50 w
               (empty-scene 100 100)))

(define (world->next w) 
  (if (>= (+ w 3) 100)
      100
      (+ w 1)))

(define (world->steer w ke)
  (cond
    [(char? ke) w]
    [(symbol=? ke 'left) 10]
    [(symbol=? ke 'right) 90]
    [else w]))

;; run world run 

(big-bang 100 100 .1 world0)

(on-redraw world->image)
(on-tick-event world->next)
(on-key-event world->steer)