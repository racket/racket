
(module view mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss"
           "frame.ss"
           "prefs.ss"
           "../model/trace.ss")
  (provide (all-defined))

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
  )
