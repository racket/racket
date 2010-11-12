#lang racket/gui

;; The module provides a timer mixing for world and universe.

;; The interface ensures that super class provides start and stop method, 
;; BUT if fails to ensure that the class comes with a _tick_ field. 
;; plus a call back for clock ticks. The super-init call sets the 
;; tick field, which the super-class uses to define the callback.


(require "check-aux.ss")

(provide clock-mixin start-stop<%>)

(define start-stop<%> (interface () start! ptock pptock stop!))

(define clock-mixin
  (mixin (start-stop<%>) ()
    (inherit ptock)
    (init-field [on-tick #f])
    (field [rate  0]
           [tick  void]
           [timer (new timer% [notify-callback (lambda () (ptock))])])
    (cond
      [(cons? on-tick)      (set! rate (second on-tick))
                            (set! tick (first on-tick))]
      [(procedure? on-tick) (set! rate RATE)
                            (set! tick on-tick)]
      [else (void)])
    (define/override (start!)
      (unless (<= rate 0)
        (send timer start (number->integer (* 1000 rate) 'big-bang/universe 'clock-rate)))
      (super start!))
    (define/override (stop! w)
      (send timer stop)
      (super stop! w))
    (define/override (pptock w)
      (tick w))
    (super-new)))
