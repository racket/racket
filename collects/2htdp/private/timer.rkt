#lang racket/gui

;; The module provides a timer mixing for world and universe.

;; The interface ensures that super class provides start and stop method, 
;; BUT if fails to ensure that the class comes with a _tick_ field. 
;; plus a call back for clock ticks. The super-init call sets the 
;; tick field, which the super-class uses to define the callback.


(require "check-aux.rkt" "stop.rkt")

(provide clock-mixin start-stop<%>)

(define start-stop<%> (interface () start! ptock pptock name-of-tick-handler stop!))

(define clock-mixin
  (mixin (start-stop<%>) ()
    (inherit ptock)
    (init-field [on-tick #f])
    (field [rate  0]
           [limit #f]
           [tick  void]
           [tick# 0]
           [timer (new timer% [notify-callback (lambda () (set! tick# (+ tick# 1)) (ptock))])])
    (match on-tick
      [`(,handler ,r ,l) 
       (set! limit l)
       (set! rate r)
       (set! tick (first on-tick))]
      [`(,handler ,r) 
       (set! rate r)
       (set! tick handler)]
      [(? procedure? handler)
       (set! rate RATE)
       (set! tick handler)]
      [else (void)])
    (define/override (start!)
      (unless (<= rate 0)
        (send timer start (number->integer (* 1000 rate) 'big-bang/universe 'clock-rate)))
      (super start!))
    (define/override (stop! w)
      (send timer stop)
      (super stop! w))
    (define/override (pptock w)
      (if (and limit (> tick# limit))
          (make-stop-the-world w)
          (tick w)))
    (define/override (name-of-tick-handler)
      (object-name tick))
    (super-new)))
