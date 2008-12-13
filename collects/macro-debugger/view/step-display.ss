
#lang scheme/base
(require scheme/class
         scheme/unit
         scheme/list
         scheme/match
         scheme/gui
         framework/framework
         syntax/boundmap
         "interfaces.ss"
         "prefs.ss"
         "extensions.ss"
         "warning.ss"
         "hiding-panel.ss"
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/deriv-find.ss"
         "../model/deriv-parser.ss"
         "../model/trace.ss"
         "../model/reductions-config.ss"
         "../model/reductions.ss"
         "../model/steps.ss"
         "../util/notify.ss"
         "cursor.ss"
         "debug-format.ss")
#;
(provide step-display%
         step-display<%>)
(provide (all-defined-out))
;; Struct for one-by-one stepping

(define-struct (prestep protostep) ())
(define-struct (poststep protostep) ())

(define (prestep-term1 s) (state-term (protostep-s1 s)))
(define (poststep-term2 s) (state-term (protostep-s1 s)))


(define step-display<%>
  (interface ()
    ;; add-syntax
    add-syntax

    ;; add-step
    add-step

    ;; add-error
    add-error

    ;; add-final
    add-final

    ;; add-internal-error
    add-internal-error))

(define step-display%
  (class* object% (step-display<%>)

    (init-field config)
    (init-field ((sbview syntax-widget)))
    (super-new)

    (define/public (add-internal-error part exn stx events)
      (send sbview add-text
            (if part
                (format "Macro stepper error (~a)" part)
                "Macro stepper error"))
      (when (exn? exn)
        (send sbview add-text " ")
        (send sbview add-clickback "[details]"
              (lambda _ (show-internal-error-details exn events))))
      (send sbview add-text ".  ")
      (when stx (send sbview add-text "Original syntax:"))
      (send sbview add-text "\n")
      (when stx (send sbview add-syntax stx)))

    (define/private (show-internal-error-details exn events)
      (case (message-box/custom "Macro stepper internal error"
                                (format "Internal error:\n~a" (exn-message exn))
                                "Show error"
                                "Dump debugging file"
                                "Cancel")
        ((1) (queue-callback
              (lambda ()
                (raise exn))))
        ((2) (queue-callback
              (lambda ()
                (let ([file (put-file)])
                  (when file
                    (write-debug-file file exn events))))))
        ((3 #f) (void))))

    (define/public (add-error exn)
      (send sbview add-error-text (exn-message exn))
      (send sbview add-text "\n"))

    (define/public (add-step step
                             #:binders binders)
      (cond [(step? step)
             (show-step step binders)]
            [(misstep? step)
             (show-misstep step binders)]
            [(prestep? step)
             (show-prestep step binders)]
            [(poststep? step)
             (show-poststep step binders)]))

    (define/public (add-syntax stx
                               #:binders binders
                               #:definites definites)
      (send sbview add-syntax stx
            #:alpha-table binders
            #:definites (or definites null)))

    (define/public (add-final stx error
                              #:binders binders
                              #:definites definites)
      (when stx
        (send sbview add-text "Expansion finished\n")
        (send sbview add-syntax stx
              #:alpha-table binders
              #:definites (or definites null)))
      (when error
        (add-error error)))

    ;; show-lctx : Step -> void
    (define/private (show-lctx step binders)
      (define state (protostep-s1 step))
      (define lctx (state-lctx state))
      (when (pair? lctx)
        (send sbview add-text "\n")
        (for-each (lambda (bf)
                    (send sbview add-text 
                          "while executing macro transformer in:\n")
                    (insert-syntax/redex (bigframe-term bf)
                                         (bigframe-foci bf)
                                         binders
                                         (state-uses state)
                                         (state-frontier state)))
                  (reverse lctx))))

    ;; separator : Step -> void
    (define/private (separator step)
      (insert-step-separator (step-type->string (protostep-type step))))

    ;; separator/small : Step -> void
    (define/private (separator/small step)
      (insert-step-separator/small
       (step-type->string (protostep-type step))))
    
    ;; show-step : Step -> void
    (define/private (show-step step binders)
      (show-state/redex (protostep-s1 step) binders)
      (separator step)
      (show-state/contractum (step-s2 step) binders)
      (show-lctx step binders))

    (define/private (show-state/redex state binders)
      (insert-syntax/redex (state-term state)
                           (state-foci state)
                           binders
                           (state-uses state)
                           (state-frontier state)))

    (define/private (show-state/contractum state binders)
      (insert-syntax/contractum (state-term state)
                                (state-foci state)
                                binders
                                (state-uses state)
                                (state-frontier state)))

    ;; show-prestep : Step -> void
    (define/private (show-prestep step binders)
      (separator/small step)
      (show-state/redex (protostep-s1 step) binders)
      (show-lctx step binders))

    ;; show-poststep : Step -> void
    (define/private (show-poststep step binders)
      (separator/small step)
      (show-state/contractum (protostep-s1 step) binders)
      (show-lctx step binders))

    ;; show-misstep : Step -> void
    (define/private (show-misstep step binders)
      (define state (protostep-s1 step))
      (show-state/redex state binders)
      (separator step)
      (send sbview add-error-text (exn-message (misstep-exn step)))
      (send sbview add-text "\n")
      (when (exn:fail:syntax? (misstep-exn step))
        (for-each (lambda (e)
                    (send sbview add-syntax e
                          #:alpha-table binders
                          #:definites (or (state-uses state) null)))
                  (exn:fail:syntax-exprs (misstep-exn step))))
      (show-lctx step binders))

    ;; insert-syntax/color : syntax syntaxes identifiers syntaxes string -> void
    (define/private (insert-syntax/color stx foci binders definites frontier hi-color)
      (define highlight-foci? (send config get-highlight-foci?))
      (define highlight-frontier? (send config get-highlight-frontier?))
      (send sbview add-syntax stx
            #:definites (or definites null)
            #:alpha-table binders
            #:hi-colors (list hi-color
                              "WhiteSmoke")
            #:hi-stxss (list (if highlight-foci? foci null)
                             (if highlight-frontier? frontier null))))

    ;; insert-syntax/redex : syntax syntaxes identifiers syntaxes -> void
    (define/private (insert-syntax/redex stx foci binders definites frontier)
      (insert-syntax/color stx foci binders definites frontier "MistyRose"))

    ;; insert-syntax/contractum : syntax syntaxes identifiers syntaxes -> void
    (define/private (insert-syntax/contractum stx foci binders definites frontier)
      (insert-syntax/color stx foci binders definites frontier "LightCyan"))

    ;; insert-step-separator : string -> void
    (define/private (insert-step-separator text)
      (send sbview add-text "\n    ")
      (send sbview add-text
            (make-object image-snip% 
                         (build-path (collection-path "icons")
                                     "red-arrow.bmp")))
      (send sbview add-text "  ")
      (send sbview add-text text)
      (send sbview add-text "\n\n"))

    ;; insert-as-separator : string -> void
    (define/private (insert-as-separator text)
      (send sbview add-text "\n    ")
      (send sbview add-text text)
      (send sbview add-text "\n\n"))

    ;; insert-step-separator/small : string -> void
    (define/private (insert-step-separator/small text)
      (send sbview add-text "    ")
      (send sbview add-text
            (make-object image-snip% 
                         (build-path (collection-path "icons")
                                     "red-arrow.bmp")))
      (send sbview add-text "  ")
      (send sbview add-text text)
      (send sbview add-text "\n\n"))
    ))
