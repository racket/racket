(require "../simple.ss")

(current-widget-parent (new ft-frame% (label "Timer") (width 400) (height 100)))

(define tenths (quotient milliseconds 100))

(define-values-rec
  [range (* 10 (mode value-b ft-slider%
                     (label "Range: ")
                     (min-value 10)
                     (max-value 30)
                     (init-value 10)))]
  [gauge-value (min range (- tenths
                             (hold 
                              (map-e (lambda (_) (value-now tenths))
                                     reset)
                              (value-now tenths))))]
  [gauge (mode widget ft-gauge% 
               (label "Elapsed: ")
               (value gauge-value)
               (range range))]
  [msg (mode widget ft-message%
             (label (number->string (quotient gauge-value 10)))
             (stretchable-width #t))]
  [reset (mode value-e ft-button% (label "Reset"))])


(send (current-widget-parent) show #t)
