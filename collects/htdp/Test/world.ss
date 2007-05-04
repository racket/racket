;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname world) (read-case-sensitive #t) (htdp-settings #7(#t constructor repeating-decimal #f #t none #f)))
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

(big-bang 100 100 .1 world0 true) ;; get ready to create images 

(on-redraw world->image)
(on-tick-event world->next)
(on-key-event world->steer)
