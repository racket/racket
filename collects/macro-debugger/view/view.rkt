#lang racket/base
(require racket/class
         racket/gui/base
         framework
         unstable/class-iop
         "interfaces.rkt"
         "frame.rkt"
         "prefs.rkt")
(provide macro-stepper-director%
         macro-stepper-frame%)

(define macro-stepper-director%
  (class* object% (director<%>)
    (field [stepper-frames (make-hasheq)])

    ;; Flags is a subset(list) of '(no-obsolete no-new-traces)

    (define/private (add-stepper! s flags)
      (hash-set! stepper-frames s flags))
    (define/public (remove-stepper! s)
      (hash-remove! stepper-frames s))

    (define/public (add-obsoleted-warning)
      (for ([(stepper-frame flags) (in-hash stepper-frames)])
        (unless (memq 'no-obsolete flags)
          (send/i stepper-frame stepper-frame<%> add-obsoleted-warning))))
    (define/public (add-trace events)
      (for ([(stepper-frame flags) (in-hash stepper-frames)])
        (unless (memq 'no-new-traces flags)
          (send/i (send/i stepper-frame stepper-frame<%> get-widget) widget<%>
                  add-trace events))))
    (define/public (add-deriv deriv)
      (for ([(stepper-frame flags) (in-hash stepper-frames)])
        (unless (memq 'no-new-traces flags)
          (send/i (send/i stepper-frame stepper-frame<%> get-widget) widget<%>
                  add-deriv deriv))))

    ;; PRE: current thread = current eventspace's handler thread
    (define/public (new-stepper [flags '()])
      (unless (eq? (current-thread)
                   (eventspace-handler-thread (current-eventspace)))
        (error 'macro-stepper-director
               "new-stepper method called from wrong thread"))
      (define stepper-frame (new-stepper-frame))
      (define stepper (send/i stepper-frame stepper-frame<%> get-widget))
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
