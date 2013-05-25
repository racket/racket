#lang frtime
(require frtime/gui/simple)

(current-widget-parent (new ft-frame% (width 400) (stretchable-width #t)))

(define-values-rec
  [gauge-value (min 15 (- seconds 
                          (hold 
                           (map-e (lambda (_) (value-now seconds))
                                  reset)
                           (value-now seconds))))]
  [gauge (mode widget ft-gauge% 
               (label "Timer") 
               (value gauge-value) 
               (range 15)
               (stretchable-width #t))]
  [msg (mode widget ft-message%
             (label (number->string gauge-value))
             (stretchable-width #t))]
  [reset (mode value-e ft-button% (label "Reset"))])


(send (current-widget-parent) show #t)
