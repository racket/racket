#lang racket

(module drop-on-message racket
  (require 2htdp/universe 2htdp/image)

  ;; Distinct from other tests:
  (define PORT-NO 19201)

  (define (u)
    (universe 0
              (on-new (lambda (u w) (make-bundle (+ u 1) '() '())))
              (on-tick (lambda (w) (make-bundle w '() '())) 1 3)
              (on-msg (lambda (u w m) (make-bundle (- u 1) '() (list w))))
              (state #t)
              (port PORT-NO)))
  
  (define (w n)
    (big-bang 3
              [to-draw (lambda (w) (overlay (text (number->string w) 22 'black) (circle 100 'solid 'red)))]
              [on-tick (lambda (w) (if (<= w 1) (make-package 0 n) (- w 1)))]
              [register LOCALHOST]
              [port PORT-NO]
              [name n]))
  
  (launch-many-worlds (u) (w "one") (w "two")))

(require (submod "." drop-on-message))

(module drop-bad racket
  (require 2htdp/universe 2htdp/image)
  
  ;; Distinct from other tests:
  (define PORT-NO 19205)

  (define *world* #false)

  (define (u)
    (universe '()
              (on-new (lambda (u w) (make-bundle (cons w u) '() '())))
              (on-tick (lambda (w) w) 1 5)
              (on-msg (lambda (u w m) 
                        ;; set *world* to the first world that comes around, reuse
                        (unless *world* (set! *world* w))
                        (make-bundle u '() (list *world*))))
              (port PORT-NO)))
  
  (define (w n)
    (big-bang 3
              [to-draw (lambda (w) (overlay (text (number->string w) 22 'black) (circle 100 'solid 'red)))]
              [on-tick (lambda (w) (if (= w 1) (make-package 0 n) (- w 1)))]
              [register LOCALHOST]
              [port PORT-NO]
              [name n]))
  
  (launch-many-worlds (u) (w "one") (w "two") (w "three")))

; (require (submod "." drop-bad))
