#lang scheme/base
(require scheme/class
         (rename-in unstable/class-iop
                    [send/i send:])
         scheme/pretty
         scheme/gui
         framework/framework
         "interfaces.ss"
         "frame.ss"
         "prefs.ss"
         "../model/trace.ss")
(provide macro-stepper-director%
         macro-stepper-frame%
         go)

(define macro-stepper-director%
  (class* object% (director<%>)
    (define stepper-frames (make-hasheq))

    ;; Flags is a subset(list) of '(no-obsolete no-new-traces)

    (define/private (add-stepper! s flags)
      (hash-set! stepper-frames s flags))
    (define/public (remove-stepper! s)
      (hash-remove! stepper-frames s))

    (define/public (add-obsoleted-warning)
      (hash-for-each stepper-frames
                     (lambda (stepper-frame flags)
                       (unless (memq 'no-obsolete flags)
                         (send: stepper-frame stepper-frame<%> add-obsoleted-warning)))))
    (define/public (add-trace events)
      (hash-for-each stepper-frames
                     (lambda (stepper-frame flags)
                       (unless (memq 'no-new-traces flags)
                         (send: (send: stepper-frame stepper-frame<%> get-widget) widget<%>
                                add-trace events)))))
    (define/public (add-deriv deriv)
      (hash-for-each stepper-frames
                     (lambda (stepper-frame flags)
                       (unless (memq 'no-new-traces flags)
                         (send: (send: stepper-frame stepper-frame<%> get-widget) widget<%>
                                add-deriv deriv)))))

    (define/public (new-stepper [flags '()])
      (define stepper-frame (new-stepper-frame))
      (define stepper (send: stepper-frame stepper-frame<%> get-widget))
      (send stepper-frame show #t)
      (add-stepper! stepper-frame flags)
      stepper)

    (define/public (new-stepper-frame)
      (new macro-stepper-frame%
           (config (new macro-stepper-config/prefs%))
           (director this)))

    (super-new)))

(define macro-stepper-frame%
  (macro-stepper-frame-mixin
   (frame:standard-menus-mixin
    (frame:basic-mixin frame%))))

;; Main entry points

(define (go stx)
  (define director (new macro-stepper-director%))
  (define stepper (send: director director<%> new-stepper))
  (send: director director<%> add-deriv (trace stx))
  (void))
