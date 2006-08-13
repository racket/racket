
(module gui mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "boundmap.ss" "syntax")
           (prefix sb: "../syntax-browser/syntax-browser.ss")
           (prefix sb: "../syntax-browser/widget.ss")
           (prefix sb: "../syntax-browser/prefs.ss")
           (prefix sb: "../syntax-browser/partition.ss")
           "../syntax-browser/util.ss"
           "../model/deriv.ss"
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/hiding-policies.ss"
           "cursor.ss"
           "util.ss")

  (provide (all-defined))

  ;; Configuration

  (define catch-errors? (make-parameter #t))

  ;; Macro Stepper

  (define base-frame%
    (frame:standard-menus-mixin frame:basic%)
    #;(frame:standard-menus-mixin (frame:basic-mixin frame%)))

  (define macro-stepper-frame%
    (class base-frame%
      (inherit get-menu%
               get-menu-item%
               get-menu-bar
               get-file-menu
               get-edit-menu
               get-help-menu)

      (super-new (label "Macro stepper")
                 (width (sb:pref:width))
                 (height (sb:pref:height)))

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

      (begin
        (new (get-menu-item%) (label "Show properties") (parent syntax-menu)
             (callback (lambda _ (send (send widget get-view) show-props))))
        (new (get-menu-item%) (label "Hide properties") (parent syntax-menu)
             (callback (lambda _ (send (send widget get-view) hide-props))))
        (define id-menu
          (new (get-menu%) (label "Identifier=?") (parent syntax-menu)))
        (for-each (lambda (p)
                    (new (get-menu-item%) (label (car p)) (parent id-menu)
                         (callback (lambda _ 
                                     (send (send widget get-controller)
                                           on-update-identifier=?
                                           (cdr p))))))
                  (sb:identifier=-choices))
        (new (get-menu-item%) (label "Clear selection") (parent syntax-menu)
             (callback
              (lambda _ (send (send widget get-controller) select-syntax #f)))))

      (define widget
        (new macro-stepper-widget%
             (register-syntax-action (mk-register-action syntax-menu))
             (register-stepper-action (mk-register-action stepper-menu))
             (parent (send this get-area-container))))

      (define/public (get-widget) widget)
      (frame:reorder-menus this)
      ))

  ;; macro-stepper-widget%
  (define macro-stepper-widget%
    (class* object% ()
      (init-field parent)
      (init-field register-syntax-action)
      (init-field register-stepper-action)

      ;; derivs : (list-of Derivation)
      (define derivs null)

      ;; synth-deriv : Derivation
      (define synth-deriv #f)

      ;; derivs-prefix : (list-of (cons Derivation Derivation))
      (define derivs-prefix null)

      (define steps #f)

      (define/public (add-deriv d)
        (set! derivs (append derivs (list d)))
        (when (and (not (send updown-navigator is-shown?))
                   (pair? (cdr (append derivs-prefix derivs))))
          (send super-navigator add-child updown-navigator)
          (send updown-navigator show #t))
        (when (null? (cdr derivs))
          ;; There is nothing currently displayed
          (refresh))
        (update))

      (define/public (get-controller) sbc)
      (define/public (get-view) sbview)

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

      (define sbview (new sb:syntax-widget% (parent area)))
      (define sbc (send sbview get-controller))
      (define control-pane
        (new vertical-panel% (parent area) (stretchable-height #f)))
      (define macro-hiding-prefs
        (new macro-hiding-prefs-widget% (parent control-pane) (stepper this)))
      (send sbc add-selection-listener
            (lambda (stx) (send macro-hiding-prefs set-syntax stx)))

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

      (register-stepper-action "Show/hide macro hiding configuration"
                               (lambda () (show/hide-macro-hiding-prefs)))

      (define/private (show/hide-macro-hiding-prefs)
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
              (send sbview add-text "Normal form\n")
              (send sbview add-syntax (deriv-e2 synth-deriv)))
            (when (step? step)
              (when (pair? (step-lctx step))
                (for-each (lambda (bc)
                            (send sbview add-text "While executing macro transformer in:\n")
                            (send sbview add-syntax (cdr bc) (car bc) "MistyRose"))
                          (step-lctx step))
                (send sbview add-text "\n"))
              (send sbview add-syntax
                    (step-e1 step)
                    (foci (step-redex step))
                    "MistyRose")
              (insert-step-separator (step-note step))
              (send sbview add-syntax
                    (step-e2 step)
                    (foci (step-contractum step))
                    "LightCyan"))
            (when (misstep? step)
              (send sbview add-syntax
                    (misstep-e1 step)
                    (foci (misstep-redex step))
                    "MistyRose")
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
      
      (define/private (enable/disable-buttons)
        (send nav:start enable (and steps (cursor:can-move-previous? steps)))
        (send nav:previous enable (and steps (cursor:can-move-previous? steps)))
        (send nav:next enable (and steps (cursor:can-move-next? steps)))
        (send nav:end enable (and steps (cursor:can-move-next? steps)))
        (send nav:up enable 
              (and (pair? derivs-prefix)
                   #;(or (not steps) (not (cursor:can-move-previous? steps)))))
        (send nav:down enable 
              (and (pair? derivs)
                   #;(or (not steps) (not (cursor:can-move-next? steps))))))

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
          (with-handlers ([(lambda (e) (catch-errors?))
                           (lambda (e)
                             (message-box 
                              "Error"
                              "Internal error in macro stepper (reductions)")
                             (set! synth-deriv #f)
                             (set! steps (cursor:new null)))])
            (let ([d (synthesize deriv)])
              (let ([s (cursor:new (reductions d))])
                (set! synth-deriv d)
                (set! steps s)))))
        #;(navigate-to-start)
        (update))

      ;; synthesize : Derivation -> Derivation
      (define/private (synthesize deriv)
        (let ([show-macro? (get-show-macro?)])
          (if show-macro?
              (with-handlers ([(lambda (e) (catch-errors?))
                               (lambda (e) (no-synthesize deriv))])
                (let-values ([(d s) (hide/policy deriv show-macro?)])
                  d))
              deriv)))

      (define/private (no-synthesize deriv)
        (message-box
         "Macro Debugger"
         (string-append
          "This expansion triggers an error in the macro hiding code. "
          "Trying again with macro hiding disabled."))
        (send macro-hiding-prefs enable-hiding #f)
        (synthesize deriv))

      (define/private (foci x) (if (list? x) x (list x)))

      ;; Hiding policy

      (define/private (get-show-macro?)
        (let ([policy (send macro-hiding-prefs get-policy)])
          (and policy (lambda (id) (policy-show-macro? policy id)))))
      
      ;; Initialization
      
      (super-new)
      (refresh)))

  ;; macro-hiding-prefs-widget%
  (define macro-hiding-prefs-widget%
    (class object%
      (init parent)
      (init-field stepper)
      (init-field (policy (new-hiding-policy)))
          ;; (new-standard-hiding-policy)))

      (define enabled? #f)
      (define stx #f)
      (define stx-name #f)
      (define stx-module #f)

      (define super-pane
        (new horizontal-pane%
             (parent parent)
             (stretchable-height #f)))
      (define left-pane
        (new vertical-pane%
             (parent super-pane)
             (stretchable-width #f)
             (alignment '(left top))))
      (define right-pane
        (new vertical-pane%
             (parent super-pane)))

      (define enable-ctl
        (new check-box% 
             (label "Enable macro hiding?")
             (parent left-pane)
             (value enabled?) 
             (callback
              (lambda _
                (set! enabled? (send enable-ctl get-value))
                (force-refresh)))))

      (define kernel-ctl
        (new check-box%
             (label "Hide mzscheme syntax")
             (parent left-pane)
             (value (hiding-policy-opaque-kernel policy))
             (callback (lambda _
                         (if (send kernel-ctl get-value)
                             (policy-hide-kernel policy)
                             (policy-unhide-kernel policy))
                         (refresh)))))
      (define libs-ctl
        (new check-box%
             (label "Hide library syntax")
             (parent left-pane)
             (value (hiding-policy-opaque-libs policy))
             (callback (lambda _
                         (if (send libs-ctl get-value)
                             (policy-hide-libs policy)
                             (policy-unhide-libs policy))
                         (refresh)))))

      (define look-pane
        (new horizontal-pane% (parent right-pane) (stretchable-height #f)))
      (define look-ctl
        (new list-box% (parent look-pane) (label "") (choices null)))
      (define delete-ctl
        (new button% (parent look-pane) (label "Delete")
             (callback
              (lambda _
                (delete-selected)
                (refresh)))))

      (define add-pane
        (new horizontal-pane% (parent right-pane) (stretchable-height #f)))
      (define add-text
        (new text-field%
             (label "")
             (parent add-pane)
             #;(enabled #f)
             (stretchable-width #t)))
      (define add-editor (send add-text get-editor))
      (define add-hide-module-button
        (new button% (parent add-pane) (label "Hide module") (enabled #f)
             (callback (lambda _ (add-hide-module) (refresh)))))
      (define add-hide-id-button
        (new button% (parent add-pane) (label "Hide macro") (enabled #f)
             (callback (lambda _ (add-hide-identifier) (refresh)))))
      (define add-show-id-button
        (new button% (parent add-pane) (label "Show macro") (enabled #f)
             (callback (lambda _ (add-show-identifier) (refresh)))))

      (send add-editor lock #t)
      
      ;; Methods

      ;; enable-hiding : boolean -> void
      ;; Called only by stepper, which does it's own refresh
      (define/public (enable-hiding ?)
        (set! enabled? ?))

      ;; get-policy
      (define/public (get-policy) (and enabled? policy))

      ;; refresh
      (define/private (refresh)
        (when enabled?
          (send stepper refresh/resynth)))

      ;; force-refresh
      (define/private (force-refresh)
        (send stepper refresh/resynth))

      ;; set-syntax : syntax/#f -> void
      (define/public (set-syntax lstx)
        (set! stx lstx)
        (send add-editor lock #f)
        (send add-editor erase)
        (unless (identifier? stx)
          (send add-hide-module-button enable #f))
        (when (identifier? stx)
          (let ([binding (identifier-binding stx)])
            (send add-hide-module-button enable (pair? binding))
            (if (pair? binding)
                (begin
                  (set! stx-name (cadr binding))
                  (set! stx-module (car binding)))
                (begin
                  (set! stx-name (syntax-e stx))
                  (set! stx-module #f)))
            (update-add-text)))
        (send add-editor lock #t)
        (send add-show-id-button enable (identifier? lstx))
        (send add-hide-id-button enable (identifier? lstx)))

      (define/private (update-add-text)
        (send add-editor lock #f)
        (if stx-module
            (send add-editor insert
                  (format "'~s' from module ~a"
                          stx-name
                          (mpi->string stx-module)))
            (send add-editor insert
                  (format "lexically-bound ~s"
                          stx-name)))
        (send add-editor lock #t))

      (define/private (add-hide-module)
        (when stx-module
          (policy-hide-module policy stx-module)
          (update-list-view)))

      (define/private (add-hide-identifier)
        (when (identifier? stx)
          (policy-hide-id policy stx)
          (update-list-view)))

      (define/private (add-show-identifier)
        (when (identifier? stx)
          (policy-show-id policy stx)
          (update-list-view)))

      (define/private (delete-selected)
        (for-each (lambda (n)
                    (let ([d (send look-ctl get-data n)])
                      (case (car d)
                        ((identifier) (policy-unhide-id policy (cdr d)))
                        ((show-identifier) (policy-unshow-id policy (cdr d)))
                        ((module) (policy-unhide-module policy (cdr d))))))
                  (send look-ctl get-selections))
        (update-list-view))

      (define/private (update-list-view)
        (let ([opaque-modules
               (hash-table-map (hiding-policy-opaque-modules policy)
                               (lambda (k v) k))]
              [opaque-ids
               (filter values
                       (module-identifier-mapping-map
                        (hiding-policy-opaque-ids policy)
                        (lambda (k v) (and v k))))]
              [transparent-ids
               (filter values
                       (module-identifier-mapping-map
                        (hiding-policy-transparent-ids policy)
                        (lambda (k v) (and v k))))])
          (define (om s)
            (cons (format "hide from module ~a" (mpi->string s))
                  (cons 'module s)))
          (define (*i prefix tag id)
            (cons (let ([b (identifier-binding id)])
                    (if (pair? b)
                        (let ([name (cadr b)]
                              [mod (car b)])
                          (format "~a '~s' from module ~a"
                                  prefix
                                  name
                                  (mpi->string mod)))
                        (format "~a lexically bound macro '~s'"
                                prefix
                                (syntax-e id))))
                  (cons tag id)))
          (define (oid id) (*i "hide" 'identifier id))
          (define (tid id) (*i "show" 'show-identifier id))
          (let ([choices
                 (sort (append (map om opaque-modules)
                               (map oid opaque-ids)
                               (map tid transparent-ids))
                       (lambda (a b)
                         (string<=? (car a) (car b))))])
            (send look-ctl clear)
            (for-each (lambda (c) (send look-ctl append (car c) (cdr c)))
                      choices))))

      (super-new)))


  ;; Main entry points

  (define (make-macro-stepper)
    (let ([f (new macro-stepper-frame%)])
      (send f show #t)
      (send f get-widget)))
  
  (define (go . stxs)
    (let ([stepper (make-macro-stepper)])
      (let loop ([stxs stxs])
        (when (pair? stxs)
          (send stepper add-deriv (trace (car stxs)))
          (loop (cdr stxs))))))

  (define (go/deriv deriv)
    (let* ([f (new macro-stepper-frame%)]
           [w (send f get-widget)])
      (send w add-deriv deriv)
      (send f show #t)
      w))

  )
