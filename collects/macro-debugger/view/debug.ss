
#lang scheme/base
(require scheme/pretty
         scheme/class
         "debug-format.ss"
         "prefs.ss"
         "view.ss")
(provide debug-file)

(define (widget-mixin %)
  (class %
    (define/override (top-interaction-kw? x)
      (eq? (syntax-e x) '#%top-interaction))
    (super-new)))

(define stepper-frame%
  (class macro-stepper-frame%
    (define/override (get-macro-stepper-widget%)
      (widget-mixin (super get-macro-stepper-widget%)))
    (super-new)))

(define (make-stepper)
  (let ([f (new macro-stepper-frame%
                (config (new macro-stepper-config/prefs%)))])
    (send f show #t)
    (send f get-widget)))

(define (debug-file file)
  (let-values ([(events msg ctx) (load-debug-file file)])
    (pretty-print msg)
    (pretty-print ctx)
    (let* ([w (make-stepper)])
      (send w add-trace events)
      w)))
