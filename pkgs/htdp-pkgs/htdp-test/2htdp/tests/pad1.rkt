#lang racket/gui

(require 2htdp/image 2htdp/universe "test-aux.rkt")

(pad=? "left" "left")

;; constants 
(define width  1200)
(define height 300)
(define center-x (quotient width 2))
(define center-y (quotient height 2))
(define x0 0)

;; graphical constants 
(define label "press down, press a, press rshift")
(define (mt)
  (overlay/align 'left 'top 
                 (text label 22 'red)
                 (add-line 
                  (add-line (empty-scene width height 'lightblue)
                            center-x 0
                            center-x height
                            'blue)
                  0 center-y 
                  width center-y
                  'blue)))

(define dot (circle 3 'solid 'red))

(define (render w)
  (define x (transform-x (real-part w)))
  (define y (transform-y (imag-part w))) 
  (place-image dot x y (mt)))

(define ((transform center) delta)
  (+ center delta))

(define transform-x (transform center-x))
(define transform-y (transform center-y))

(define (phandler x k)
  (case (string->symbol k)
    [(up    w)      (- x 0+10i)]
    [(down  s)      (+ x 0+10i)]
    [(left  a)      (- x 10)]
    [(right d)      (+ x 10)]
    [(| |)          x0]
    [(shift)        (conjugate x)]
    [(rshift)       (stop-with (conjugate x))]))

(define ((key-handler tag) x k)
  (displayln `(,tag ,k))
  x)

(define-syntax-rule 
  (run txt clause ...) 
  (begin (set! label (string-append txt label))
         (big-bang x0 (to-draw render) (on-pad phandler) clause ... )))

(testing
  (= -10-10i (run ""))
  (= -10-10i (run "press l, " (on-key (key-handler 'key))))
  (= -10-10i (run "press l, " (on-key (key-handler 'key)) (on-release (key-handler 'release))))
  (= -10-10i (run "press l, " (on-release (key-handler 'release)))))

