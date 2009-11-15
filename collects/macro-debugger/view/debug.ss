#lang scheme/base
(require scheme/pretty
         scheme/class
         (rename-in unstable/class-iop
                    [send/i send:])
         "interfaces.ss"
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
  (define director (new macro-stepper-director%))
  (send director new-stepper))

(define (debug-file file)
  (let-values ([(events msg ctx) (load-debug-file file)])
    (pretty-print msg)
    (pretty-print ctx)
    (let* ([w (make-stepper)])
      (send: w widget<%> add-trace events)
      w)))
