
(module gui mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "prefs.ss"
           "warning.ss"
           "hiding-panel.ss"
           (prefix sb: "../syntax-browser/embed.ss")
           "../model/deriv.ss"
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/steps.ss"
           "cursor.ss"
           "util.ss")

  (provide pre-stepper@
           view@
           context-menu-extension@
           browser-extension@

           catch-errors?)

  ;; Debugging parameters / Not user configurable

  (define catch-errors? (make-parameter #t))

  ;; Struct for one-by-one stepping

  (define-struct (prestep protostep) (foci1 e1))
  (define-struct (poststep protostep) (foci2 e2))

  (define (prestep-term1 s) (context-fill (protostep-ctx s) (prestep-e1 s)))
  (define (poststep-term2 s) (context-fill (protostep-ctx s) (poststep-e2 s)))

  ;; Macro Stepper

  (define view@
    (unit
      (import prefs^
              view-base^
              (prefix sb: sb:widget^))
      (export view^)

      (define macro-stepper-config%
        (class object%
          (field/notify width (notify-box/pref pref:width))
          (field/notify height (notify-box/pref pref:height))
          (field/notify macro-hiding? (notify-box/pref pref:macro-hiding?))
          (field/notify show-syntax-properties?
                        (notify-box/pref pref:show-syntax-properties?))
          (field/notify show-hiding-panel?
                        (notify-box/pref pref:show-hiding-panel?))
          (field/notify hide-primitives?
                        (notify-box/pref pref:hide-primitives?))
          (field/notify hide-libs?
                        (notify-box/pref pref:hide-libs?))
          (field/notify highlight-foci?
                        (notify-box/pref pref:highlight-foci?))
          (field/notify show-rename-steps?
                        (notify-box/pref pref:show-rename-steps?))
          (field/notify suppress-warnings?
                        (notify-box/pref pref:suppress-warnings?))
          (field/notify one-by-one?
                        (notify-box/pref pref:one-by-one?))
          (field/notify extra-navigation?
                        (notify-box/pref pref:extra-navigation?))
          (super-new)))

      (define macro-stepper-frame%
        (class base-frame%
          (init (identifier=? (pref:identifier=?)))
          (init-field (config (new macro-stepper-config%)))

          (inherit get-menu%
                   get-menu-item%
                   get-menu-bar
                   get-file-menu
                   get-edit-menu
                   get-help-menu)

          (super-new (label "Macro stepper")
                     (width (send config get-width))
                     (height (send config get-height)))

          (define/override (on-size w h)
            (send config set-width w)
            (send config set-height h)
            (send widget update/preserve-view))

          (define/augment (on-close)
            (send widget shutdown)
            (inner (void) on-close))

          (override/return-false file-menu:create-new?
                                 file-menu:create-open?
                                 file-menu:create-open-recent?
                                 file-menu:create-revert?
                                 file-menu:create-save?
                                 file-menu:create-save-as?
                                 ;file-menu:create-print?
                                 edit-menu:create-undo?
                                 edit-menu:create-redo?
                                 ;edit-menu:create-cut?
                                 ;edit-menu:create-paste?
                                 edit-menu:create-clear?
                                 ;edit-menu:create-find?
                                 ;edit-menu:create-find-again?
                                 edit-menu:create-replace-and-find-again?)

          (define file-menu (get-file-menu))
          (define edit-menu (get-edit-menu))
          (define stepper-menu
            (new (get-menu%) (parent (get-menu-bar)) (label "Stepper")))
          (define help-menu (get-help-menu))

          (define widget
            (new macro-stepper-widget%
                 (parent (send this get-area-container))
                 (config config)))

          (define/public (get-widget) widget)

          ;; Set up menus

          (menu-option/notify-box stepper-menu
                                  "Show syntax properties"
                                  (get-field show-syntax-properties? config))

          ;; FIXME: rewrite with notify-box
          (let ([id-menu
                 (new (get-menu%)
                      (label "Identifier=?")
                      (parent stepper-menu))])
            (for-each (lambda (p)
                        (let ([this-choice
                               (new checkable-menu-item%
                                    (label (car p)) 
                                    (parent id-menu)
                                    (callback
                                     (lambda _ 
                                       (send (send widget get-controller)
                                             on-update-identifier=?
                                             (car p)
                                             (cdr p)))))])
                          (send (send widget get-controller)
                                add-identifier=?-listener
                                (lambda (new-name new-func)
                                  (send this-choice check
                                        (eq? new-name (car p)))))))
                      (sb:identifier=-choices)))
          (when identifier=?
            (let ([p (assoc identifier=? (sb:identifier=-choices))])
              (when p
                (send (send widget get-controller)
                      on-update-identifier=?
                      (car p)
                      (cdr p)))))

          (new (get-menu-item%) (label "Clear selection") (parent stepper-menu)
               (callback
                (lambda _
                  (send (send widget get-controller) select-syntax #f))))
          (new separator-menu-item% (parent stepper-menu))

          (menu-option/notify-box stepper-menu
                                  "Show macro hiding panel"
                                  (get-field show-hiding-panel? config))
          (let ([extras-menu
                 (new (get-menu%)
                      (label "Extra options")
                      (parent stepper-menu))])
            (menu-option/notify-box extras-menu
                                    "Highlight redex/contractum"
                                    (get-field highlight-foci? config))
            (menu-option/notify-box extras-menu
                                    "Include renaming steps"
                                    (get-field show-rename-steps? config))
            (menu-option/notify-box extras-menu
                                    "One term at a time"
                                    (get-field one-by-one? config))
            (menu-option/notify-box extras-menu
                                    "Suppress warnings"
                                    (get-field suppress-warnings? config))
            (menu-option/notify-box extras-menu
                                    "Extra navigation"
                                    (get-field extra-navigation? config))
            (new checkable-menu-item%
                 (label "(Debug) Catch internal errors?")
                 (parent extras-menu)
                 (checked (catch-errors?))
                 (callback
                  (lambda (c e) (catch-errors? (send c is-checked?))))))

          (frame:reorder-menus this)
          ))


      ;; macro-stepper-widget%
      (define macro-stepper-widget%
        (class* object% ()
          (init-field parent)
          (init-field config)

          ;; derivs : (list-of Derivation)
          (define derivs null)

          ;; synth-deriv : Derivation
          (define synth-deriv #f)

          ;; derivs-prefix : (list-of (cons Derivation Derivation))
          (define derivs-prefix null)

          ;; steps : cursor
          (define steps #f)

          ;; alpha-table : module-identifier-mapping[identifier => identifier]
          (define alpha-table (make-module-identifier-mapping))

          ;; saved-position : number/#f
          (define saved-position #f)

          (define warnings-frame #f)

          ;; add-deriv : Derivation -> void
          (define/public (add-deriv d)
            (for-each (lambda (id) (module-identifier-mapping-put! alpha-table id id))
                      (extract-all-fresh-names d))
            (set! derivs (append derivs (list d)))
            (ensure-nav:up+down-shown)
            (if (null? (cdr derivs))
                ;; There is nothing currently displayed
                (refresh/move/cached-prefix)
                (update)))

          (define/private (ensure-nav:up+down-shown)
            (when (and (not (send nav:up is-shown?))
                       (pair? (cdr (append derivs-prefix derivs))))
              (send navigator change-children
                    (lambda (_)
                      (list nav:up
                            nav:start
                            nav:previous
                            nav:next
                            nav:end
                            nav:down)))))

          (define/public (get-controller) sbc)
          (define/public (get-view) sbview)
          (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

          (define area (new vertical-panel% (parent parent)))
          (define supernavigator
            (new horizontal-panel%
                 (parent area)
                 (stretchable-height #f)
                 (alignment '(center center))))
          (define navigator
            (new horizontal-panel%
                 (parent supernavigator)
                 (stretchable-width #f)
                 (stretchable-height #f)
                 (alignment '(left center))))
          (define extra-navigator
            (new horizontal-panel%
                 (parent supernavigator)
                 (stretchable-width #f)
                 (stretchable-height #f)
                 (alignment '(left center))
                 (style '(deleted))))

          (define sbview (new sb:syntax-widget% 
                              (parent area)
                              (macro-stepper this)
                              (pref:props-percentage pref:props-percentage)))
          (define sbc (send sbview get-controller))
          (define control-pane
            (new vertical-panel% (parent area) (stretchable-height #f)))
          (define macro-hiding-prefs
            (new macro-hiding-prefs-widget%
                 (parent control-pane)
                 (stepper this)
                 (config config)))

          (send config listen-show-syntax-properties?
                (lambda (show?) (send sbview show-props show?)))

          (send config listen-show-hiding-panel?
                (lambda (show?) (show-macro-hiding-prefs show?)))

          (send sbc add-selection-listener
                (lambda (stx)
                  (send macro-hiding-prefs set-syntax stx)))

          (send config listen-highlight-foci?
                (lambda (_) (update/preserve-view)))

          (send config listen-show-rename-steps?
                (lambda (_) (refresh)))

          (send config listen-one-by-one?
                (lambda (_) (refresh)))

          (send config listen-extra-navigation?
                (lambda (show?) (show-extra-navigation show?)))

          (define nav:up
            (new button% (label "Previous term") (parent navigator)
                 (style '(deleted))
                 (callback (lambda (b e) (navigate-up)))))

          (define nav:start
            (new button% (label "<-- Start") (parent navigator)
                 (callback (lambda (b e) (navigate-to-start)))))
          (define nav:previous
            (new button% (label "<- Step") (parent navigator)
                 (callback (lambda (b e) (navigate-previous)))))
          (define nav:next
            (new button% (label "Step ->") (parent navigator)
                 (callback (lambda (b e) (navigate-next)))))
          (define nav:end
            (new button% (label "End -->") (parent navigator)
                 (callback (lambda (b e) (navigate-to-end)))))
          
          (define nav:down
            (new button% (label "Next term") (parent navigator) (style '(deleted))
                 (callback (lambda (b e) (navigate-down)))))

          (define nav:zoom
            (new button% (label "Zoom in") (parent extra-navigator)
                 (callback (lambda (b e) (zoom)))))

          (define nav:jump-to
            (new button% (label "Jump to") (parent extra-navigator)
                 (callback (lambda (b e) (jump-to)))))

          (define/public (show-macro-hiding-prefs show?)
            (send area change-children
                  (lambda (children)
                    (if show?
                        (append (remq control-pane children) (list control-pane))
                        (remq control-pane children)))))

          (define/private (show-extra-navigation show?)
            (send supernavigator change-children
                  (lambda (children)
                    (if show?
                        (list navigator extra-navigator)
                        (list navigator)))))

          ;; Navigate

          (define/private (navigate-to-start)
            (cursor:move-to-start steps)
            (update/save-position))
          (define/private (navigate-to-end)
            (cursor:move-to-end steps)
            (update/save-position))
          (define/private (navigate-previous)
            (cursor:move-previous steps)
            (update/save-position))
          (define/private (navigate-next)
            (cursor:move-next steps)
            (update/save-position))
          
          (define/private (navigate-up)
            (let ([d+sd (car derivs-prefix)])
              (set! derivs (cons (car d+sd) derivs))
              (set! synth-deriv (cdr d+sd))
              (set! derivs-prefix (cdr derivs-prefix)))
            (refresh/move/cached-prefix))
          (define/private (navigate-down)
            (let ([d0 (car derivs)])
              (set! derivs-prefix (cons (cons d0 synth-deriv) derivs-prefix))
              (set! derivs (cdr derivs))
              (set! synth-deriv #f))
            (refresh/move/cached-prefix))

          ;; FIXME: selected stx must be in term1; doesn't work in term2
          (define/public (zoom)
            (let* ([selected-syntax (send sbc get-selected-syntax)]
                   [step (and steps (cursor:current steps))]
                   [deriv (and step (protostep-deriv step))])
              (when (and selected-syntax deriv)
                (for-each go/deriv (seek-syntax selected-syntax deriv)))))

          (define/public (jump-to)
            (let* ([selected-syntax (send sbc get-selected-syntax)]
                   [step (and steps (cursor:current steps))]
                   [deriv (and step (protostep-deriv step))])
              (when (and selected-syntax deriv)
                (let ([subderivs (seek-syntax selected-syntax deriv)])
                  (cond [(null? subderivs)
                         (message-box "Macro stepper - Jump to"
                                      "Cannot find selected term in the expansion")]
                        [(and (pair? subderivs) (null? (cdr subderivs)))
                         (jump-to/deriv (car subderivs))]
                        [else
                         (message-box 
                          "Macro stepper - Jump to"
                          "Subterm occurs more than once in the expansion (non-linearity)")])))))

          (define/private (jump-to/deriv subderiv)
            (define all-step-derivs
              (let ([ht (make-hash-table)])
                (for-each (lambda (s) (hash-table-put! ht (protostep-deriv s) #t))
                          (cursor-suffix->list steps))
                ht))
            (define target-deriv
              subderiv
              #;
              (find-deriv
               (lambda (d) (hash-table-get all-step-derivs d (lambda () #f)))
               (lambda (d) #f)
               subderiv))
            (unless target-deriv
              (message-box "Macro stepper - Jump to"
                           "Could not find selected term in the expansion"))
            (when target-deriv
              (let loop ()
                (unless (eq? (protostep-deriv (cursor:current steps)) target-deriv)
                  (cursor:move-next steps)))
              (update/save-position)))

          (define/private (insert-step-separator text)
            (send sbview add-text "\n    ")
            (send sbview add-text
                  (make-object image-snip% 
                               (build-path (collection-path "icons")
                                           "red-arrow.bmp")))
            (send sbview add-text "  ")
            (send sbview add-text text)
            (send sbview add-text "\n\n"))

          (define/private (insert-step-separator/small text)
            (send sbview add-text "    ")
            (send sbview add-text
                  (make-object image-snip% 
                               (build-path (collection-path "icons")
                                           "red-arrow.bmp")))
            (send sbview add-text "  ")
            (send sbview add-text text)
            (send sbview add-text "\n\n"))


          ;; update/preserve-view : -> void
          (define/public (update/preserve-view)
            (define text (send sbview get-text))
            (define start-box (box 0))
            (define end-box (box 0))
            (send text get-visible-position-range start-box end-box)
            (update)
            (send text scroll-to-position (unbox start-box) #f (unbox end-box)))
          
          (define/private (update:show-prefix)
            ;; Show the final terms from the cached synth'd derivs
            (for-each (lambda (d+sd)
                        (let ([e2 (lift/deriv-e2 (cdr d+sd))])
                          (if e2
                              (send sbview add-syntax e2 #:alpha-table alpha-table)
                              (send sbview add-text "Error\n"))))
                      (reverse derivs-prefix)))
          
          (define/private (update:show-current-step)
            (when steps
              (let ([step (cursor:current steps)])
                (cond [(step? step)
                       (update:show-step step)]
                      [(misstep? step)
                       (update:show-misstep step)]
                      [(prestep? step)
                       (update:show-prestep step)]
                      [(poststep? step)
                       (update:show-poststep step)]
                      [(not step)
                       (update:show-final)]))))
          
          (define/private (update:show-lctx step)
            (define lctx (protostep-lctx step))
            (when (pair? lctx)
              (send sbview add-text "\n")
              (for-each (lambda (bf)
                          (send sbview add-text 
                                "while executing macro transformer in:\n")
                          (insert-syntax/redex (bigframe-term bf)
                                               (bigframe-foci bf)
                                               (protostep-definites step)))
                        (reverse lctx))))

          (define/private (update:separator step)
            (insert-step-separator (step-type->string (protostep-type step))))

          (define/private (update:separator/small step)
            (insert-step-separator/small
             (step-type->string (protostep-type step))))

          (define/private (update:show-step step)
            (insert-syntax/redex (step-term1 step) (step-foci1 step)
                                 (protostep-definites step)
                                 (protostep-frontier step))
            (update:separator step)
            (insert-syntax/contractum (step-term2 step) (step-foci2 step)
                                      (protostep-definites step)
                                      (protostep-frontier step))
            (update:show-lctx step))

          (define/private (update:show-prestep step)
            (update:separator/small step)
            (insert-syntax/redex (prestep-term1 step)
                                 (prestep-foci1 step)
                                 (protostep-definites step))
            (update:show-lctx step))

          (define/private (update:show-poststep step)
            (update:separator/small step)
            (insert-syntax/contractum (poststep-term2 step)
                                      (poststep-foci2 step)
                                      (protostep-definites))
            (update:show-lctx step))
          
          (define/private (update:show-misstep step)
            (insert-syntax/redex (misstep-term1 step)
                                 (misstep-foci1 step)
                                 (protostep-definites step))
            (update:separator step)
            (send sbview add-text (exn-message (misstep-exn step)))
            (send sbview add-text "\n")
            (when (exn:fail:syntax? (misstep-exn step))
              (for-each (lambda (e) (send sbview add-syntax e
                                          #:alpha-table alpha-table
                                          #:definites (protostep-definites step)))
                        (exn:fail:syntax-exprs (misstep-exn step))))
            (update:show-lctx step))

          (define/private (update:show-final)
            (let ([result (lift/deriv-e2 synth-deriv)])
              (when result
                (send sbview add-text "Expansion finished\n")
                (send sbview add-syntax result #:alpha-table alpha-table))
              (unless result
                (send sbview add-text "Error\n"))))

          (define/private (update:show-suffix)
            (when (pair? derivs)
              (for-each (lambda (suffix-deriv)
                          (send sbview add-syntax
                                (lift/deriv-e1 suffix-deriv)
                                #:alpha-table alpha-table))
                        (cdr derivs))))

          ;; update/save-position : -> void
          (define/private (update/save-position)
            (save-position)
            (update))

          ;; update : -> void
          ;; Updates the terms in the syntax browser to the current step
          (define/private (update)
            (define text (send sbview get-text))
            (define position-of-interest 0)
            (send text begin-edit-sequence)
            (send sbview erase-all)

            (update:show-prefix)
            (send sbview add-separator)
            (set! position-of-interest (send text last-position))
            (update:show-current-step)
            (send sbview add-separator)
            (update:show-suffix)
            (send text end-edit-sequence)
            (send text scroll-to-position
                  position-of-interest
                  #f
                  (send text last-position)
                  'start)
            (enable/disable-buttons))

          ;; insert-syntax/redex : syntax syntaxes identifiers syntaxes -> void
          (define/private (insert-syntax/redex stx foci definites frontier)
            (if (send config get-highlight-foci?)
                (send sbview add-syntax stx
                      #:hi-stxs foci #:hi-color "MistyRose"
                      #:alpha-table alpha-table
                      #:definites definites
                      #:hi2-stxs frontier #:hi2-color "WhiteSmoke")
                (send sbview add-syntax stx
                      #:alpha-table alpha-table
                      #:definites definites)))

          ;; insert-syntax/contractum : syntax syntaxes identifiers syntaxes -> void
          (define/private (insert-syntax/contractum stx foci definites frontier)
            (if (send config get-highlight-foci?)
                (send sbview add-syntax stx
                      #:hi-stxs foci #:hi-color "LightCyan"
                      #:alpha-table alpha-table
                      #:definites definites
                      #:hi2-stxs frontier #:hi2-color "WhiteSmoke")
                (send sbview add-syntax stx
                      #:alpha-table alpha-table
                      #:definites definites)))

          ;; enable/disable-buttons : -> void
          (define/private (enable/disable-buttons)
            (send nav:start enable (and steps (cursor:can-move-previous? steps)))
            (send nav:previous enable (and steps (cursor:can-move-previous? steps)))
            (send nav:next enable (and steps (cursor:can-move-next? steps)))
            (send nav:end enable (and steps (cursor:can-move-next? steps)))
            (send nav:up enable (and (pair? derivs-prefix)))
            (send nav:down enable (and (pair? derivs))))

          ;; --

          ;; refresh/move/cached-prefix : -> void
          ;; Resynth current derivation,
          ;; Create reductions for current derivation,
          ;; Show first step
          (define/private (refresh/move/cached-prefix)
            (clear-saved-position)
            (if (pair? derivs)
                (refresh)
                (begin (set! synth-deriv #f)
                       (set! steps #f)
                       (update))))

          ;; refresh/resynth-prefix : -> void
          ;; Resynth all of the derivations in prefix and refresh
          (define/public (refresh/resynth-prefix)
            (with-handlers ([(lambda (e) (catch-errors?))
                             (lambda (e)
                               (message-box "Error"
                                            "Internal error in macro stepper (prefixes)")
                               (send sbview erase-all))])
              (let ([ds (map car derivs-prefix)])
                (let ([sds (map (lambda (d) (synthesize d)) ds)])
                  (set! derivs-prefix (map cons ds sds)))))
            (refresh))

          ;; refresh : -> void
          (define/public (refresh)
            (let ([deriv (car derivs)])
              (let ([d (synthesize deriv)])
                (let ([rseq (reduce d)])
                  (set! synth-deriv d)
                  (set! steps (cursor:new rseq)))))
            (restore-position)
            (update))

          ;; update-saved-position : num -> void
          (define/private (update-saved-position pos)
            (when pos (set! saved-position pos)))

          ;; clear-saved-position : -> void
          (define/private (clear-saved-position)
            (set! saved-position #f))

          ;; save-position : -> void
          (define/private (save-position)
            (when steps
              (let ([step (cursor:current steps)])
                (cond [(not step)
                       ;; At end; go to the end when restored
                       (update-saved-position +inf.0)]
                      [(protostep? step)
                       (update-saved-position
                        (extract-protostep-seq step))]))))

          ;; restore-position : number -> void
          (define/private (restore-position)
            (define (advance)
              (let ([step (cursor:current steps)])
                (cond [(not step)
                       ;; At end; stop
                       (void)]
                      [(protostep? step)
                       (let ([step-pos (extract-protostep-seq step)])
                         (cond [(not step-pos)
                                (cursor:move-next steps)
                                (advance)]
                               [(< step-pos saved-position)
                                (cursor:move-next steps)
                                (advance)]
                               [else (void)]))])))
            (when saved-position
              (when steps
                (advance))))

          (define/private (extract-protostep-seq step)
            (match (protostep-deriv step)
              [(AnyQ mrule (_ _ (AnyQ transformation (_ _ _ _ _ _ seq)) _))
               seq]
              [else #f]))

          ;; synthesize : Derivation -> Derivation
          (define/private (synthesize deriv)
            (let ([show-macro? (get-show-macro?)])
              (if show-macro?
                  (with-handlers ([(lambda (e) (catch-errors?))
                                   (lambda (e) (disable-hiding) deriv)])
                    (parameterize ((current-hiding-warning-handler
                                    (lambda (tag message)
                                      (unless (send config get-suppress-warnings?)
                                        (unless warnings-frame
                                          (set! warnings-frame (new warnings-frame%)))
                                        (send warnings-frame add-warning tag)))))
                      (let-values ([(d s) (hide/policy deriv show-macro?)])
                        d)))
                  deriv)))

          (define/private (disable-hiding)
            (message-box
             "Macro Debugger Internal Error"
             (string-append
              "This expansion triggers an internal error in the macro hiding code. "
              "Trying again with macro hiding disabled."))
            (queue-callback (lambda () (send config set-macro-hiding? #f))))

          ;; reduce : Derivation -> ReductionSequence
          (define/private (reduce d)
            (with-handlers ([(lambda (e) (catch-errors?))
                             (lambda (e)
                               (message-box 
                                "Error"
                                "Internal error in macro stepper (reductions)")
                               (set! synth-deriv #f)
                               #f)])
              (reduce:sequence d)))

          (define/private (reduce:sequence d)
            (define raw-seq (reductions d))
            (define filtered-seq
              (if (send config get-show-rename-steps?)
                  raw-seq
                  (filter (lambda (x) (not (rename-step? x))) raw-seq)))
            (if (send config get-one-by-one?)
                (reduce:one-by-one filtered-seq)
                filtered-seq))

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

          (define/private (foci x) (if (list? x) x (list x)))

          ;; Hiding policy

          (define/private (get-show-macro?)
            (and (send config get-macro-hiding?)
                 (send macro-hiding-prefs get-show-macro?)))

          ;; --

          (define/public (shutdown)
            (when warnings-frame (send warnings-frame show #f)))

          ;; Initialization
          
          (super-new)
          (send sbview show-props (send config get-show-syntax-properties?))
          (show-macro-hiding-prefs (send config get-show-hiding-panel?))
          (show-extra-navigation (send config get-extra-navigation?))
          (refresh/move/cached-prefix)
          ))

      ;; Main entry points

      (define (make-macro-stepper)
        (let ([f (new macro-stepper-frame%)])
          (send f show #t)
          (send f get-widget)))

      (define (go stx)
        (let ([stepper (make-macro-stepper)])
          (send stepper add-deriv (trace stx))))

      (define (go/deriv deriv)
        (let* ([f (new macro-stepper-frame%)]
               [w (send f get-widget)])
          (send w add-deriv deriv)
          (send f show #t)
          w))

      ))
  
  ;; Extensions
  
  (define keymap-extension@
    (unit
      (import (prefix pre: sb:keymap^))
      (export sb:keymap^)
      
      (define syntax-keymap%
        (class pre:syntax-keymap%
          (init-field macro-stepper)
          (inherit-field controller)
          (inherit add-function)
          
          (super-new)
          
          (define/public (get-hiding-panel)
            (send macro-stepper get-macro-hiding-prefs))
          
          (add-function "hiding:show-macro"
                        (lambda (i e)
                          (send* (get-hiding-panel)
                            (add-show-identifier)
                            (refresh))))
          
          (add-function "hiding:hide-macro"
                        (lambda (i e)
                          (send* (get-hiding-panel)
                            (add-hide-identifier)
                            (refresh))))))))
  
  (define context-menu-extension@
    (unit
      (import (prefix pre: sb:context-menu^))
      (export sb:context-menu^)
      
      (define context-menu%
        (class pre:context-menu%
          (inherit-field keymap)
          (inherit add-separator)
          
          (field [show-macro #f]
                 [hide-macro #f])
          
          (define/override (after-selection-items)
            (super after-selection-items)
            (add-separator)
            (set! show-macro
                  (new menu-item% (label "Show this macro") (parent this)
                       (callback (lambda (i e)
                                   (send keymap call-function "hiding:show-macro" i e)))))
            (set! hide-macro
                  (new menu-item% (label "Hide this macro") (parent this)
                       (callback (lambda (i e)
                                   (send keymap call-function "hiding:hide-macro" i e)))))
            (void))
          
          (define/override (on-demand)
            (define hiding-panel (send keymap get-hiding-panel))
            (define controller (send keymap get-controller))
            (define stx (send controller get-selected-syntax))
            (define id? (identifier? stx))
            (define show-macro? (send hiding-panel get-show-macro?))
            (define transparent? (and id? (show-macro? stx)))
            (define opaque? (and id? (not (show-macro? stx))))
            (send show-macro enable (and id? (not transparent?)))
            (send hide-macro enable (and id? (not opaque?)))
            (super on-demand))

          (super-new)))))
  
  (define browser-extension@
    (unit
      (import (prefix pre: sb:widget^)
              sb:keymap^)
      (export sb:widget^)
      
      (define syntax-widget%
        (class pre:syntax-widget%
          (init-field macro-stepper)
          
          (define/override (make-keymap text)
            (new syntax-keymap%
                 (editor text)
                 (widget this)
                 (macro-stepper macro-stepper)))

          (define/override (show-props show?)
            (super show-props show?)
            (send macro-stepper update/preserve-view))

          (super-new)))))
  
  ;; Linking
  
  (define context-menu@
    (compound-unit
      (import)
      (link [((SB:MENU : sb:context-menu^)) sb:widget-context-menu@]
            [((V:MENU : sb:context-menu^)) context-menu-extension@ SB:MENU])
      (export V:MENU)))
  
  (define keymap@
    (compound-unit
      (import [MENU : sb:context-menu^]
              [SNIP : sb:snip^])
      (link [((SB:KEYMAP : sb:keymap^)) sb:widget-keymap@ MENU SNIP]
            [((V:KEYMAP : sb:keymap^)) keymap-extension@ SB:KEYMAP])
      (export V:KEYMAP)))

  (define widget@
    (compound-unit
      (import [KEYMAP : sb:keymap^])
      (link [((SB:WIDGET : sb:widget^)) sb:widget@ KEYMAP]
            [((V:WIDGET : sb:widget^)) browser-extension@ SB:WIDGET KEYMAP])
      (export V:WIDGET)))
  
  (define pre-stepper@
    (compound-unit
      (import [BASE : view-base^])
      (link [((PREFS : prefs^)) prefs@]
            [((MENU : sb:context-menu^)) context-menu@]
            [((KEYMAP : sb:keymap^)) keymap@ MENU SNIP]
            [((SNIP : sb:snip^)) sb:global-snip@]
            [((WIDGET : sb:widget^)) widget@ KEYMAP]
            [((VIEW : view^)) view@ PREFS BASE WIDGET])
      (export VIEW)))
  )
