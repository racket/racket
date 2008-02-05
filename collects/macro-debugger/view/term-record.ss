
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
         (prefix-in s: "../syntax-browser/widget.ss")
         (prefix-in s: "../syntax-browser/params.ss")
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/deriv-find.ss"
         "../model/deriv-parser.ss"
         "../model/trace.ss"
         "../model/reductions.ss"
         "../model/hide.ss"
         "../model/steps.ss"
         "debug-format.ss"
         "cursor.ss"
         "../util/notify.ss")

(provide term-record%)

;; Struct for one-by-one stepping

(define-struct (prestep protostep) (foci1 e1))
(define-struct (poststep protostep) (foci2 e2))

(define (prestep-term1 s) (context-fill (protostep-ctx s) (prestep-e1 s)))
(define (poststep-term2 s) (context-fill (protostep-ctx s) (poststep-e2 s)))

;; TermRecords

(define term-record%
  (class object%
    (init-field stepper)
    (init-field [events #f])

    (define config (send stepper get-config))
    (define sbview (send stepper get-view))

    (init-field [raw-deriv #f])
    (define raw-deriv-oops #f)

    (define deriv #f)
    (define deriv-hidden? #f)
    (define binders #f)

    (define synth-deriv #f)
    (define synth-warnings null)
    (define synth-estx #f)
    (define synth-oops #f)

    (define raw-steps #f)
    (define raw-steps-estx #f)
    (define definites #f)
    (define error #f)
    (define raw-steps-oops #f)

    (define steps #f)

    (define steps-position #f)

    (super-new)

    (define-syntax define-guarded-getters
      (syntax-rules ()
        [(define-guarded-getters guard (method expr) ...)
         (begin (define/public (method) guard expr) ...)]))

    (define-guarded-getters (recache-deriv!)
      [get-deriv deriv]
      [get-deriv-hidden? deriv-hidden?]
      [get-binders binders])
    (define-guarded-getters (recache-synth!)
      [get-synth-deriv synth-deriv]
      [get-synth-warnings synth-warnings]
      [get-synth-estx synth-estx]
      [get-synth-oops synth-oops])
    (define-guarded-getters (recache-raw-steps!)
      [get-definites definites]
      [get-error error]
      [get-raw-steps-oops raw-steps-oops])
    (define-guarded-getters (recache-steps!)
      [get-steps steps])

    ;; invalidate-steps! : -> void
    ;; Invalidates cached parts that depend on reductions config
    (define/public (invalidate-steps!)
      (set! steps #f))

    ;; invalidate-raw-steps! : -> void
    (define/public (invalidate-raw-steps!)
      (invalidate-steps!)
      (set! raw-steps #f)
      (set! raw-steps-estx #f)
      (set! definites #f)
      (set! error #f)
      (set! raw-steps-oops #f))

    ;; invalidate-synth! : -> void
    ;; Invalidates cached parts that depend on macro-hiding policy
    (define/public (invalidate-synth!)
      (invalidate-raw-steps!)
      (set! synth-deriv #f)
      (set! synth-warnings null)
      (set! synth-oops #f)
      (set! synth-estx #f))

    ;; invalidate-deriv! : -> void
    (define/public (invalidate-deriv!)
      (invalidate-synth!)
      (set! deriv #f)
      (set! deriv-hidden? #f)
      (set! binders #f))

    ;; recache! : -> void
    (define/public (recache!)
      (recache-steps!))

    ;; recache-raw-deriv! : -> void
    (define/private (recache-raw-deriv!)
      (unless (or raw-deriv raw-deriv-oops)
        (with-handlers ([(lambda (e) #t)
                         (lambda (e)
                           (set! raw-deriv-oops e))])
          (set! raw-deriv
                (parse-derivation
                 (events->token-generator events))))))

    ;; recache-deriv! : -> void
    (define/private (recache-deriv!)
      (unless (or deriv deriv-hidden?)
        (recache-raw-deriv!)
        (when raw-deriv
          (let ([process (send stepper get-preprocess-deriv)])
            (let ([d (process raw-deriv)])
              (when (not d)
                (set! deriv-hidden? #t))
              (when d
                (let ([alpha-table (make-module-identifier-mapping)])
                  (for-each (lambda (id)
                              (module-identifier-mapping-put! alpha-table id id))
                            (extract-all-fresh-names d))
                  (set! deriv d)
                  (set! binders alpha-table))))))))

    ;; recache-synth! : -> void
    (define/private (recache-synth!)
      (unless (or synth-deriv synth-oops)
        (recache-deriv!)
        (when deriv
          (set! synth-warnings null)
          (let ([show-macro? (send stepper get-show-macro?)]
                [force-letrec? (send config get-force-letrec-transformation?)])
            (with-handlers ([(lambda (e) #t)
                             (lambda (e)
                               (set! synth-oops e))])
              (let ()
                (define-values (synth-deriv* estx*)
                  (if show-macro?
                      (parameterize ((current-hiding-warning-handler
                                      (lambda (tag args)
                                        (set! synth-warnings
                                              (cons (cons tag args)
                                                    synth-warnings))))
                                     (force-letrec-transformation
                                      force-letrec?))
                        (hide/policy deriv show-macro?))
                      (values deriv (wderiv-e2 deriv))))
                (set! synth-deriv synth-deriv*)
                (set! synth-estx estx*)))))))

    ;; recache-raw-steps! : -> void
    (define/private (recache-raw-steps!)
      (unless (or raw-steps raw-steps-oops)
        (recache-synth!)
        (when synth-deriv
          (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (set! raw-steps-oops e))])
            (let-values ([(raw-steps* definites* estx* error*)
                          (reductions+ synth-deriv)])
              (set! raw-steps raw-steps*)
              (set! raw-steps-estx estx*)
              (set! error error*)
              (set! definites definites*))))))

    ;; recache-steps! : -> void
    (define/private (recache-steps!)
      (unless (or steps)
        (recache-raw-steps!)
        (when raw-steps
          (set! steps
                (and raw-steps
                     (let* ([filtered-steps 
                             (if (send config get-show-rename-steps?)
                                 raw-steps
                                 (filter (lambda (x) (not (rename-step? x)))
                                         raw-steps))]
                            [processed-steps
                             (if (send config get-one-by-one?)
                                 (reduce:one-by-one filtered-steps)
                                 filtered-steps)])
                       (cursor:new processed-steps))))
          (restore-position))))

    ;; reduce:one-by-one : (list-of step) -> (list-of step)
    (define/private (reduce:one-by-one rs)
      (let loop ([rs rs])
        (match rs
          [(cons (struct step (d l t c df fr redex contractum e1 e2)) rs)
           (list* (make-prestep d l "Find redex" c df fr redex e1)
                  (make-poststep d l t c df fr contractum e2)
                  (loop rs))]
          [(cons (struct misstep (d l t c df fr redex e1 exn)) rs)
           (list* (make-prestep d l "Find redex" c df fr redex e1)
                  (make-misstep d l t c df fr redex e1 exn)
                  (loop rs))]
          ['()
           null])))

    ;; Navigation

    (define/public-final (has-prev?)
      (and (get-steps) (not (cursor:at-start? (get-steps)))))
    (define/public-final (has-next?)
      (and (get-steps) (not (cursor:at-end? (get-steps)))))

    (define/public-final (navigate-to-start)
      (cursor:move-to-start (get-steps))
      (save-position))
    (define/public-final (navigate-to-end)
      (cursor:move-to-end (get-steps))
      (save-position))
    (define/public-final (navigate-previous)
      (cursor:move-prev (get-steps))
      (save-position))
    (define/public-final (navigate-next)
      (cursor:move-next (get-steps))
      (save-position))

    ;; save-position : -> void
    (define/private (save-position)
      (when (cursor? steps)
        (let ([step (cursor:next steps)])
          (cond [(not step)
                 ;; At end; go to the end when restored
                 (set! steps-position +inf.0)]
                [(protostep? step)
                 (set! steps-position
                       (extract-protostep-seq step))]))))

    ;; restore-position : number -> void
    (define/private (restore-position)
      (define (seek)
        (let ([step (cursor:next steps)])
          (cond [(not step)
                 ;; At end; stop
                 (void)]
                [(protostep? step)
                 (let ([step-pos (extract-protostep-seq step)])
                   (cond [(not step-pos)
                          (cursor:move-next steps)
                          (seek)]
                         [(< step-pos steps-position)
                          (cursor:move-next steps)
                          (seek)]
                         [else (void)]))])))
      (when steps-position
        (seek)))

    ;; extract-protostep-seq : step -> number/#f
    (define/private (extract-protostep-seq step)
      (match (protostep-deriv step)
        [(Wrap mrule (_ _ (Wrap transformation (_ _ _ _ _ _ _ _ seq)) _))
         seq]
        [else #f]))

    ;; Warnings display

    ;; on-get-focus : -> void
    (define/public (on-get-focus)
      (recache-synth!)
      (display-warnings))

    ;; on-lose-focus : -> void
    (define/public (on-lose-focus)
      (when steps (cursor:move-to-start steps))
      (set! steps-position #f))

    ;; display-warnings : -> void
    (define/private (display-warnings)
      (let ([warnings-area (send stepper get-warnings-area)])
        (unless (send config get-suppress-warnings?)
          (for-each (lambda (tag+args)
                      (let ([tag (car tag+args)]
                            [args (cdr tag+args)])
                        (send warnings-area add-warning tag args)))
                    synth-warnings))))

    ;; Rendering

    ;; display-initial-term : -> void
    (define/public (display-initial-term)
      (add-syntax (wderiv-e1 deriv) #f null))

    ;; display-final-term : -> void
    (define/public (display-final-term)
      (recache-synth!)
      (cond [(syntax? synth-estx)
             (add-syntax synth-estx binders definites)]
            [(exn? error)
             (add-error error)]
            [raw-steps-oops
             (add-internal-error "steps" raw-steps-oops #f)]
            [synth-oops
             (add-internal-error "hiding" synth-oops #f)]))

    ;; display-step : -> void
    (define/public (display-step)
      (recache-steps!)
      (cond [steps
             (let ([step (cursor:next steps)])
               (if step
                   (add-step step binders)
                   (add-final raw-steps-estx error binders definites)))]
            [raw-steps-oops
             (add-internal-error "steps" raw-steps-oops (wderiv-e1 deriv))]
            [synth-oops
             (add-internal-error "hiding" synth-oops (wderiv-e1 deriv))]
            [raw-deriv-oops
             (add-internal-error "derivation" raw-deriv-oops #f)]
            [else
             (add-internal-error "derivation" #f)]))

    (define/public (add-internal-error part exn stx)
      (send sbview add-text
            (if part
                (format "Macro stepper error (~a)" part)
                "Macro stepper error"))
      (when (exn? exn)
        (send sbview add-text " ")
        (send sbview add-clickback "[details]"
              (lambda _ (show-internal-error-details exn))))
      (send sbview add-text ".  ")
      (when stx (send sbview add-text "Original syntax:"))
      (send sbview add-text "\n")
      (when stx (send sbview add-syntax stx)))

    (define/private (show-internal-error-details exn)
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

    (define/public (add-step step binders)
      (cond [(step? step)
             (show-step step binders)]
            [(mono? step)
             (show-mono step binders)]
            [(misstep? step)
             (show-misstep step binders)]
            [(prestep? step)
             (show-prestep step binders)]
            [(poststep? step)
             (show-poststep step binders)]))

    (define/public (add-syntax stx binders definites)
      (send sbview add-syntax stx
            '#:alpha-table binders
            '#:definites definites))

    (define/private (add-final stx error binders definites)
      (when stx
        (send sbview add-text "Expansion finished\n")
        (send sbview add-syntax stx
              '#:alpha-table binders
              '#:definites (or definites null)))
      (when error
        (add-error error)))

    ;; show-lctx : Step -> void
    (define/private (show-lctx step binders)
      (define lctx (protostep-lctx step))
      (when (pair? lctx)
        (send sbview add-text "\n")
        (for-each (lambda (bf)
                    (send sbview add-text 
                          "while executing macro transformer in:\n")
                    (insert-syntax/redex (bigframe-term bf)
                                         (bigframe-foci bf)
                                         binders
                                         (protostep-definites step)
                                         (protostep-frontier step)))
                  (reverse lctx))))

    ;; separator : Step -> void
    (define/private (separator step)
      (if (not (mono? step))
          (insert-step-separator (step-type->string (protostep-type step)))
          (insert-as-separator (step-type->string (protostep-type step)))))

    ;; separator/small : Step -> void
    (define/private (separator/small step)
      (insert-step-separator/small
       (step-type->string (protostep-type step))))
    
    ;; show-step : Step -> void
    (define/private (show-step step binders)
      (insert-syntax/redex (step-term1 step)
                           (step-foci1 step)
                           binders
                           (protostep-definites step)
                           (protostep-frontier step))
      (separator step)
      (insert-syntax/contractum (step-term2 step)
                                (step-foci2 step)
                                binders
                                (protostep-definites step)
                                (protostep-frontier step))
      (show-lctx step binders))

    ;; show-mono : Step -> void
    (define/private (show-mono step binders)
      (separator step)
      (insert-syntax/redex (mono-term1 step)
                           null
                           binders
                           (protostep-definites step)
                           (protostep-frontier step))
      (show-lctx step binders))

    ;; show-prestep : Step -> void
    (define/private (show-prestep step binders)
      (separator/small step)
      (insert-syntax/redex (prestep-term1 step)
                           (prestep-foci1 step)
                           binders
                           (protostep-definites step)
                           (protostep-frontier step))
      (show-lctx step binders))

    ;; show-poststep : Step -> void
    (define/private (show-poststep step binders)
      (separator/small step)
      (insert-syntax/contractum (poststep-term2 step)
                                (poststep-foci2 step)
                                binders
                                (protostep-definites step)
                                (protostep-frontier step))
      (show-lctx step binders))

    ;; show-misstep : Step -> void
    (define/private (show-misstep step binders)
      (insert-syntax/redex (misstep-term1 step)
                           (misstep-foci1 step)
                           binders
                           (protostep-definites step)
                           (protostep-frontier step))
      (separator step)
      (send sbview add-error-text (exn-message (misstep-exn step)))
      (send sbview add-text "\n")
      (when (exn:fail:syntax? (misstep-exn step))
        (for-each (lambda (e) (send sbview add-syntax e
                                    '#:alpha-table binders
                                    '#:definites (protostep-definites step)))
                  (exn:fail:syntax-exprs (misstep-exn step))))
      (show-lctx step binders))


    ;; insert-syntax/color : syntax syntaxes identifiers syntaxes string -> void
    (define/private (insert-syntax/color stx foci binders definites frontier hi-color)
      (send sbview add-syntax stx
            '#:definites definites
            '#:alpha-table binders
            '#:hi-color hi-color
            '#:hi-stxs (if (send config get-highlight-foci?) foci null)
            '#:hi2-color "WhiteSmoke"
            '#:hi2-stxs (if (send config get-highlight-frontier?) frontier null)))

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
