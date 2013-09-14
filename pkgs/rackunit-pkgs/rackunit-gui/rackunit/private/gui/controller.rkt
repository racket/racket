#lang racket/base
(require racket/class
         unstable/class-iop
         unstable/gui/notify
         rackunit/private/base
         "interfaces.rkt"
         "model.rkt")
(provide controller%)

(define controller%
  (class* object% (controller<%>)
    (super-new)

    ;; model-shown : (notify-box (U model<%> #f))
    ;; The model currently displayed in the Details view, of #f is none.
    (define-notify selected-model (new notify-box% (value #f)))

    ;; locked? : (notify-box boolean)
    (define-notify locked? (new notify-box% (value #f)))

    ;; view : #f or view<%>
    (define view #f)

    ;; check-ready : -> void
    (define/private (check-ready)
      (unless view
        (error 'rackunit "The RackUnit GUI is no longer running."))
      (when (get-locked?)
        (error 'rackunit "The RackUnit GUI is locked and not accepting tests.")))

    ;; create-model : test suite<%>/#f -> result<%>
    (define/public (create-model test parent)
      (define _ (check-ready))
      (define result
        (cond [(rackunit-test-case? test)
               (new case-result%
                    (controller this)
                    (test test)
                    (name (or (rackunit-test-case-name test)
                              "<unnamed test-case>"))
                    (parent parent))]
              [(rackunit-test-suite? test)
               (new suite-result%
                    (controller this)
                    (test test)
                    (name (or (rackunit-test-suite-name test)
                              "<unnamed test-suite>"))
                    (parent parent))]))
      (send/i view view<%> create-view-link result parent)
      result)

    ;; on-model-status-change : model<%> -> void
    (define/public (on-model-status-change model)
      (let ([view view]) ;; view field is async. mutable!
        (when view (send view queue-for-update model)))
      (let [(parent (send model get-parent))]
        (when parent (send parent on-child-status-change model))))

    ;; register-view : view<%> -> void
    (define/public (register-view v)
      (set! view v))

    ;; on-view-shutdown : -> void
    (define/public (on-view-shutdown)
      (set! view #f))
    ))
