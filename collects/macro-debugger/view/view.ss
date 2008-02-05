
#lang scheme/base
(require scheme/class
         scheme/pretty
         scheme/gui
         framework/framework
         "interfaces.ss"
         "frame.ss"
         "prefs.ss"
         "../model/trace.ss")
(provide (all-defined-out))

(define macro-stepper-frame%
  (macro-stepper-frame-mixin
   (frame:standard-menus-mixin
    (frame:basic-mixin frame%))))

;; Main entry points

(define (make-macro-stepper)
  (let ([f (new macro-stepper-frame%
                (config (new macro-stepper-config/prefs%)))])
    (send f show #t)
    (send f get-widget)))

(define (go stx)
  (let ([stepper (make-macro-stepper)])
    (send stepper add-deriv (trace stx))
    stepper))

(define (go/deriv deriv)
  (let* ([f (new macro-stepper-frame%)]
         [w (send f get-widget)])
    (send w add-deriv deriv)
    (send f show #t)
    w))

(define (go/trace events)
  (let* ([w (make-macro-stepper)])
    (send w add-trace events)
    w))
