
(module gui mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "prefs.ss"
           "warning.ss"
           "hiding-panel.ss"
           (prefix sb: "../syntax-browser/embed.ss")
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/hiding-policies.ss"
           "../model/steps.ss"
           "cursor.ss"
           "util.ss")

  (provide catch-errors?
           pre-stepper@
           view@
           context-menu-extension@
           browser-extension@)

  ;; Configuration

  (define catch-errors? (make-parameter #t))
  (define show-rename-steps? (make-parameter #f))

  ;; Macro Stepper

  (define view@
    (unit/sig view^
      (import prefs^
              view-base^
              (sb : sb:widget^))
      
      (define (default-policy)
        (let ([p (new-hiding-policy)])
          (set-hiding-policy-opaque-kernel! p (pref:hide-primitives?))
          (set-hiding-policy-opaque-libs!   p (pref:hide-libs?))
          p))
      
      (define macro-stepper-frame%
        (class base-frame%
          (init (policy (default-policy))
                (macro-hiding? (pref:macro-hiding?))
                (show-hiding-panel? (pref:show-hiding-panel?))
                (identifier=? (pref:identifier=?))
                (width (pref:width))
                (height (pref:height)))
          (inherit get-menu%
                   get-menu-item%
                   get-menu-bar
                   get-file-menu
                   get-edit-menu
                   get-help-menu)
          
          (super-new (label "Macro stepper")
                     (width width)
                     (height height))

          (define/override (on-size w h)
            (send widget update/preserve-view))
          
          (define/augment (on-close)
            (pref:width (send this get-width))
            (pref:height (send this get-height))
            (send widget shutdown)
            (preferences:save)
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
          (define syntax-menu
            (new (get-menu%) (parent (get-menu-bar)) (label "Syntax")))
          (define stepper-menu
            (new (get-menu%) (parent (get-menu-bar)) (label "Stepper")))
          (define help-menu (get-help-menu))
          
          (define (mk-register-action menu)
            (lambda (label callback)
              (if label
                  (new (get-menu-item%)
                       (label label) (parent menu) (callback (lambda _ (callback))))
                  (new separator-menu-item% (parent menu)))))
          
          (define widget
            (new macro-stepper-widget%
                 (parent (send this get-area-container))
                 (policy policy)
                 (macro-hiding? macro-hiding?)
                 (show-hiding-panel? show-hiding-panel?)))
          (define/public (get-widget) widget)

          (begin
            (new (get-menu-item%) (label "Show/hide syntax properties") (parent syntax-menu)
                 (callback (lambda _ (send (send widget get-view) toggle-props))))
            (define id-menu
              (new (get-menu%) (label "Identifier=?") (parent syntax-menu)))
            (for-each (lambda (p)
                        (let ([this-choice
                               (new checkable-menu-item%
                                    (label (car p)) 
                                    (parent id-menu)
                                    (callback (lambda _ 
                                                (send (send widget get-controller)
                                                      on-update-identifier=?
                                                      (car p)
                                                      (cdr p)))))])
                          (send (send widget get-controller)
                                add-identifier=?-listener
                                (lambda (new-name new-func)
                                  (send this-choice check (eq? new-name (car p)))))))
                      (sb:identifier=-choices))
            (new (get-menu-item%) (label "Clear selection") (parent syntax-menu)
                 (callback
                  (lambda _ (send (send widget get-controller) select-syntax #f))))
            (new (get-menu-item%)
                 (label "Show/hide macro hiding configuration")
                 (parent stepper-menu)
                 (callback (lambda _ (send widget show/hide-macro-hiding-prefs)))))

          (begin
            (when identifier=?
              (let ([p (assoc identifier=? (sb:identifier=-choices))])
                (when p
                  (send (send widget get-controller)
                        on-update-identifier=?
                        (car p)
                        (cdr p))))))
          
          (frame:reorder-menus this)
          ))

      ;; macro-stepper-widget%
      (define macro-stepper-widget%
        (class* object% ()
          (init-field parent)
          (init policy)
          (init macro-hiding?)
          (init show-hiding-panel?)

          ;; derivs : (list-of Derivation)
          (define derivs null)

          ;; synth-deriv : Derivation
          (define synth-deriv #f)

          ;; derivs-prefix : (list-of (cons Derivation Derivation))
          (define derivs-prefix null)

          (define steps #f)

          (define warnings-frame #f)

          (define/public (add-deriv d)
            (set! derivs (append derivs (list d)))
            (when (and (not (send updown-navigator is-shown?))
                       (pair? (cdr (append derivs-prefix derivs))))
              (send super-navigator add-child updown-navigator)
              (send updown-navigator show #t))
            (if (null? (cdr derivs))
                ;; There is nothing currently displayed
                (refresh)
                (update)))

          (define/public (get-controller) sbc)
          (define/public (get-view) sbview)
          (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

          (define area (new vertical-panel% (parent parent)))
          (define super-navigator
            (new horizontal-panel%
                 (parent area)
                 (stretchable-height #f)
                 (alignment '(center center))))
          (define navigator
            (new horizontal-panel%
                 (parent super-navigator)
                 (stretchable-height #f)
                 (alignment '(center center))))
          (define updown-navigator
            (new horizontal-panel%
                 (parent super-navigator)
                 (style '(deleted))
                 (stretchable-height #f)
                 (alignment '(center center))))

          (define sbview (new sb:syntax-widget% 
                              (parent area)
                              (macro-stepper this)
                              (pref:props-percentage pref:props-percentage)))
          (define sbc (send sbview get-controller))
          (define control-pane
            (new vertical-panel% (parent area) (stretchable-height #f)))
          (define macro-hiding-prefs
            (new macro-hiding-prefs-widget%
                 (policy policy)
                 (parent control-pane)
                 (stepper this)
                 (enabled? macro-hiding?)))
          (send sbc add-selection-listener
                (lambda (stx) (send macro-hiding-prefs set-syntax stx)))
          (unless show-hiding-panel?
            (show/hide-macro-hiding-prefs))
          
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
          
          (define nav:up
            (new button% (label "Previous term") (parent updown-navigator)
                 (callback (lambda (b e) (navigate-up)))))
          (define nav:down
            (new button% (label "Next term") (parent updown-navigator)
                 (callback (lambda (b e) (navigate-down)))))

          (define/public (show/hide-macro-hiding-prefs)
            (send area change-children
                  (lambda (children)
                    (if (memq control-pane children)
                        (remq control-pane children)
                        (append children (list control-pane))))))
          
          ;; Navigate

          (define/private (navigate-to-start)
            (cursor:move-to-start steps)
            (update))
          (define/private (navigate-to-end)
            (cursor:move-to-end steps)
            (update))
          (define/private (navigate-previous)
            (cursor:move-previous steps)
            (update))
          (define/private (navigate-next)
            (cursor:move-next steps)
            (update))
          
          (define/private (navigate-up)
            (let ([d+sd (car derivs-prefix)])
              (set! derivs (cons (car d+sd) derivs))
              (set! synth-deriv (cdr d+sd))
              (set! derivs-prefix (cdr derivs-prefix)))
            (refresh))
          (define/private (navigate-down)
            (let ([d0 (car derivs)])
              (set! derivs-prefix (cons (cons d0 synth-deriv) derivs-prefix))
              (set! derivs (cdr derivs))
              (set! synth-deriv #f))
            (refresh))

          (define/private (insert-step-separator text)
            (send sbview add-text "\n    ")
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

          ;; update : -> void
          ;; Updates the terms in the syntax browser to the current step
          (define/private (update)
            (define text (send sbview get-text))
            (define position-of-interest 0)
            (send text begin-edit-sequence)
            (send sbview erase-all)
            (when (pair? derivs-prefix)
              ;; Show the final terms from the cached synth'd derivs
              (for-each (lambda (d+sd)
                          (let ([e2 (lift/deriv-e2 (cdr d+sd))])
                            (if e2
                                (send sbview add-syntax e2)
                                (send sbview add-text "Error\n"))))
                        (reverse derivs-prefix))
              (send sbview add-separator))
            (set! position-of-interest (send text last-position))
            (when steps
              (let ([step (cursor:current steps)])
                (unless step
                  (let ([result (lift/deriv-e2 synth-deriv)])
                    (when result
                      (send sbview add-text "Expansion finished\n")
                      (send sbview add-syntax result))
                    (unless result
                      (send sbview add-text "Error\n"))))
                (when (step? step)
                  (when (pair? (step-lctx step))
                    (for-each (lambda (bc)
                                (send sbview add-text "While executing macro transformer in:\n")
                                (insert-syntax/redex (cdr bc) (car bc)))
                              (step-lctx step))
                    (send sbview add-text "\n"))
                  (insert-syntax/redex (step-e1 step) (foci (step-redex step)))
                  (insert-step-separator (step-note step))
                  (insert-syntax/contractum (step-e2 step) (foci (step-contractum step))))
                (when (misstep? step)
                  (insert-syntax/redex (misstep-e1 step) (foci (misstep-redex step)))
                  (insert-step-separator "Error")
                  (send sbview add-text (exn-message (misstep-exn step)))
                  (send sbview add-text "\n")
                  (when (exn:fail:syntax? (misstep-exn step))
                    (for-each (lambda (e) (send sbview add-syntax e))
                              (exn:fail:syntax-exprs (misstep-exn step)))))))
            (when (and (pair? derivs) (pair? (cdr derivs)))
              (send sbview add-separator)
              (for-each (lambda (suffix-deriv)
                          (send sbview add-syntax (lift/deriv-e1 suffix-deriv)))
                        (cdr derivs)))
            (send text end-edit-sequence)
            (send text scroll-to-position
                  position-of-interest
                  #f
                  (send text last-position)
                  'start)
            (enable/disable-buttons))

          ;; insert-syntax/redex : syntax syntaxes -> void
          (define/private (insert-syntax/redex stx foci)
            (send sbview add-syntax stx foci "MistyRose"))
          
          ; insert-syntax/contractum : syntax syntaxes -> void
          (define/private (insert-syntax/contractum stx foci)
            (send sbview add-syntax stx foci "LightCyan"))
          
          ;; enable/disable-buttons : -> void
          (define/private (enable/disable-buttons)
            (send nav:start enable (and steps (cursor:can-move-previous? steps)))
            (send nav:previous enable (and steps (cursor:can-move-previous? steps)))
            (send nav:next enable (and steps (cursor:can-move-next? steps)))
            (send nav:end enable (and steps (cursor:can-move-next? steps)))
            (send nav:up enable (and (pair? derivs-prefix)))
            (send nav:down enable 
                  (and (pair? derivs))))
          ;; --
          
          ;; refresh/resynth : -> void
          ;; Resynth all of the derivations in prefix and refresh
          (define/public (refresh/resynth)
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
          ;; Resynth current derivation,
          ;; Create reductions for current derivation,
          ;; Show first step
          (define/private (refresh)
            (if (pair? derivs)
                (refresh/nontrivial)
                (begin (set! synth-deriv #f)
                       (set! steps #f)
                       (update))))

          ;; refresh/nontrivial : -> void
          (define/private (refresh/nontrivial)
            (let ([deriv (car derivs)])
              (let ([d (synthesize deriv)])
                (let ([s (cursor:new (reduce d))])
                  (set! synth-deriv d)
                  (set! steps s))))
            (update))

          ;; synthesize : Derivation -> Derivation
          (define/private (synthesize deriv)
            (let ([show-macro? (get-show-macro?)])
              (if show-macro?
                  (with-handlers ([(lambda (e) (catch-errors?))
                                   (lambda (e) (no-synthesize deriv))])
                    (parameterize ((current-hiding-warning-handler
                                    (lambda (tag message)
                                      (unless warnings-frame
                                        (set! warnings-frame (new warnings-frame%)))
                                      (send warnings-frame add-warning tag)
                                      #;
                                      (send warnings-frame add-text
                                            (format "Warning: ~a~n" message)))))
                      (let-values ([(d s) (hide/policy deriv show-macro?)])
                        d)))
                  deriv)))

          (define/private (no-synthesize deriv)
            (message-box
             "Macro Debugger"
             (string-append
              "This expansion triggers an error in the macro hiding code. "
              "Trying again with macro hiding disabled."))
            (send macro-hiding-prefs enable-hiding #f)
            (synthesize deriv))
          
          ;; reduce : Derivation -> ReductionSequence
          (define/private (reduce d)
            (with-handlers ([(lambda (e) (catch-errors?))
                             (lambda (e)
                               (message-box 
                                "Error"
                                "Internal error in macro stepper (reductions)")
                               (set! synth-deriv #f)
                               (set! steps #f))])
              (if (show-rename-steps?)
                  (reductions d)
                  (filter (lambda (x) (not (rename-step? x)))
                          (reductions d)))))
          
          (define/private (foci x) (if (list? x) x (list x)))

          ;; Hiding policy

          (define/private (get-policy)
            (and (send macro-hiding-prefs get-enabled?)
                 (send macro-hiding-prefs get-policy)))

          (define/private (get-show-macro?)
            (let ([policy (get-policy)])
              (and policy (lambda (id) (policy-show-macro? policy id)))))

          ;; --

          (define/public (shutdown)
            (let ([policy (get-policy)])
              (pref:macro-hiding? (and policy #t))
              (pref:hide-primitives? (and policy (hiding-policy-opaque-kernel policy)))
              (pref:hide-libs? (and policy (hiding-policy-opaque-libs policy))))
            (pref:show-hiding-panel? (send control-pane is-shown?))
            (when warnings-frame (send warnings-frame show #f)))

          ;; Initialization
          
          (super-new)
          (refresh)))

      ;; Main entry points

      (define make-macro-stepper
        (case-lambda
          [(policy hiding?)
           (let ([f (new macro-stepper-frame%
                         (policy policy)
                         (macro-hiding? hiding?))])
             (send f show #t)
             (send f get-widget))]
          [(policy)
           (make-macro-stepper policy #t)]
          [()
           (make-macro-stepper (new-hiding-policy) #f)]))

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
  
  
  (define context-menu-extension@
    (unit/sig sb:context-menu^
      (import (pre : sb:context-menu^))
      
      (define context-menu%
        (class pre:context-menu%
          (init-field macro-stepper)
          (inherit-field controller)
          (inherit add-separator)
          
          (define/private (get-prefs-panel)
            (send macro-stepper get-macro-hiding-prefs))

          (define show-macro #f)
          (define hide-macro #f)
          (define remove-macro #f)
          
          (define/override (after-selection-items)
            (super after-selection-items)
            (add-separator)
            (set! show-macro
                  (new menu-item% (label "Show this macro") (parent this)
                       (callback (lambda _ (do-show)))))
            (set! hide-macro
                  (new menu-item% (label "Hide this macro") (parent this)
                       (callback (lambda _ (do-hide)))))
            #;(set! remove-macro
                    (new menu-item% (label "Remove macro from policy") (parent this)
                         (callback (lambda _ (do-remove)))))
            (void))
          
          (define/private (do-show)
            (send* (get-prefs-panel)
              (add-show-identifier)
              (refresh)))
            
          (define/private (do-hide)
            (send* (get-prefs-panel)
              (add-hide-identifier)
              (refresh)))
          
          (define/override (on-demand)
            (define-values (opaque transparent)
              (let ([policy (send (get-prefs-panel) get-policy)])
                (values (hiding-policy-opaque-ids policy)
                        (hiding-policy-transparent-ids policy))))
            (define stx (send controller get-selected-syntax))
            (define id? (identifier? stx))
            (define transparent?
              (and id? (module-identifier-mapping-get transparent stx (lambda () #f))))
            (define opaque?
              (and id? (module-identifier-mapping-get opaque stx (lambda () #f))))
            (send show-macro enable (and id? (not transparent?)))
            (send hide-macro enable (and id? (not opaque?)))
            #;(send remove-macro enable (and id? (or opaque? transparent?)))
            (super on-demand))

          (super-new)))))
  
  (define browser-extension@
    (unit/sig sb:widget^
      (import (pre : sb:widget^)
              sb:context-menu^)
      
      (define syntax-widget%
        (class pre:syntax-widget%
          (init-field macro-stepper)
          
          (define/override (make-context-menu)
            (new context-menu%
                 (widget this)
                 (macro-stepper macro-stepper)))
          (super-new)))))
  
  (define pre-stepper@
    (compound-unit/sig
      (import [BASE : view-base^])
      (link [PREFS : prefs^ (prefs@)]
            [SBKEYMAP : sb:keymap^ (sb:keymap@)]
            [SBMENU : sb:context-menu^ (sb:context-menu@ SBSNIP)]
            [SBSNIP : sb:snip^ (sb:global-snip@)]
            [SBWMENU : sb:context-menu^ (sb:widget-context-menu-extension@ SBMENU)]
            [VMENU : sb:context-menu^ (context-menu-extension@ SBWMENU)]
            [SBWIDGET : sb:widget^ (sb:widget@ SBKEYMAP SBWMENU)]
            [VWIDGET : sb:widget^ (browser-extension@ SBWIDGET VMENU)]
            [VIEW : view^ (view@ PREFS BASE VWIDGET)])
      (export (open VIEW))))
    
  
  )
