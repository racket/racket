#lang scheme

(require htdp/world)

(with-handlers ((exn? (lambda (x) #t)))
  (place-image (circle 3 'solid 'red) 1.2 3.14 (empty-scene 100 100)))

(define (ms w x y e)
  (if (eq? e 'button-down) (list x y) w))
(define (rd w) 
  (local ((define mt (empty-scene 300 300))
          (define x1 (first w))
          (define y1 (second w))
          (define tx (text (format "(~s,~s)" x1 y1) 22 'red))
          (define cr (circle 3 'solid 'red))
          (define m1 (place-image tx 50 50 mt))
          (define m2 (place-image cr x1 y1 m1)))
    m2))
(big-bang 300 300 1 (list 50 50))
(on-redraw rd)
(on-mouse-event ms)
