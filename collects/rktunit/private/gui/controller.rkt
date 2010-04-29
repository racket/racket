#lang racket/base
(require racket/class
         unstable/class-iop
         unstable/gui/notify
         "../base.rkt"
         "interfaces.rkt"
         "model.rkt"
         "view.rkt")
(provide controller%)

(define controller%
  (class* object% (controller<%>)
    (init display-window)
    (super-new)

    ;; model-shown : (notify-box (U model<%> #f))
    ;; The model currently displayed in the Details view, of #f is none.
    (define-notify selected-model (new notify-box% (value #f)))

    (define view
      (new view%
           (controller this)
           (parent (send display-window get-area-container))))

    ;; create-model : test suite<%>/#f -> result<%>
    (define/public (create-model test parent)
      (define result
        (cond [(rktunit-test-case? test)
               (new case-result%
                    (controller this)
                    (test test)
                    (name (or (rktunit-test-case-name test)
                              "<unnamed test-case>"))
                    (parent parent))]
              [(rktunit-test-suite? test)
               (new suite-result%
                    (controller this)
                    (test test)
                    (name (or (rktunit-test-suite-name test)
                              "<unnamed test-suite>"))
                    (parent parent))]))
      (send/i view view<%> create-view-link result parent)
      result)

    ;; on-model-status-change : model<%> -> void
    (define/public (on-model-status-change model)
      (send view queue-for-update model)
      (let [(parent (send model get-parent))]
        (when parent (send parent on-child-status-change model))))
    ))
