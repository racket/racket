#lang scheme/gui

;; The module provides a timer mixing for world and universe.

;; The interface ensures that super class provides start and stop method,
;; plus a call back for clock ticks. The super-init call provides the 
;; on-tick parameter, which the super-class uses to define the callback.


(require "check-aux.ss")

(provide clock-mixin start-stop<%>)

(define start-stop<%> (interface () start! ptock stop!))

;; T = (U (World -> World) (list (World -> World) Nat))
;; X [(list (World -> World) Nat) -> X] [(World -> World) -> X] -> [T -> X]
(define (selector default lchoice pchoice)
  (lambda (on-tick)
    (cond
      [(cons? on-tick) (lchoice on-tick)]
      [(procedure? on-tick) (pchoice on-tick)]
      [else default])))

(define clock-mixin
  (mixin (start-stop<%>) ()
    (inherit ptock)
    (init-field [on-tick #f])
    (field [rate  ((selector 0 second (lambda _ RATE)) on-tick)]
           [timer (new timer% [notify-callback (lambda () (ptock))])])
    (define/override (start!)
      (unless (<= rate 0)
        (send timer start (number->integer (* 1000 rate) 'big-bang/universe 'clock-rate)))
      (super start!))
    (define/override (stop! w)
      (send timer stop)
      (super stop! w))
    (super-new [tick ((selector void first (lambda (x) x)) on-tick)])))
