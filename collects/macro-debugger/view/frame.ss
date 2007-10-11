
(module frame mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "stepper.ss"
           "prefs.ss"
           "warning.ss"
           "hiding-panel.ss"
           (prefix sb: "../syntax-browser/embed.ss")
           (prefix sb: "../syntax-browser/params.ss")
           "../model/deriv.ss"
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/steps.ss"
           "cursor.ss"
           "util.ss")
  (provide macro-stepper-frame-mixin)

  (define (macro-stepper-frame-mixin base-frame%)
    (class base-frame%
      (init-field config)
      (init-field (filename #f))

      (define obsoleted? #f)

      (inherit get-area-container
               set-label
               get-menu%
               get-menu-item%
               get-menu-bar
               get-file-menu
               get-edit-menu
               get-help-menu)

      (super-new (label (make-label))
                 (width (send config get-width))
                 (height (send config get-height)))

      (define/private (make-label)
        (if filename
            (string-append (path->string
                            (file-name-from-path filename))
                           (if obsoleted? " (old)" "")
                           " - Macro stepper")
            "Macro stepper"))

      (define/override (on-size w h)
        (send config set-width w)
        (send config set-height h)
        (send widget update/preserve-view))

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

      (define warning-panel
        (new horizontal-panel%
             (parent (get-area-container))
             (stretchable-height #f)
             (style '(deleted))))

      (define widget
        (new macro-stepper-widget%
             (parent (get-area-container))
             (config config)))

      (define/public (get-widget) widget)

      (define/public (add-obsoleted-warning)
        (unless obsoleted?
          (set! obsoleted? #t)
          (new warning-canvas%
               (warning
                (string-append
                 "Warning: This macro stepper session is obsolete. "
                 "The program may have changed."))
               (parent warning-panel))
          (set-label (make-label))
          (send (get-area-container) change-children
                (lambda (children)
                  (cons warning-panel
                        (remq warning-panel children))))))

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
                                         set-identifier=? p))))])
                      (send (send widget get-controller)
                            listen-identifier=?
                            (lambda (name+func)
                              (send this-choice check
                                    (eq? (car name+func) (car p)))))))
                  (sb:identifier=-choices)))
      (let ([identifier=? (send config get-identifier=?)])
        (when identifier=?
          (let ([p (assoc identifier=? (sb:identifier=-choices))])
            (send (send widget get-controller) set-identifier=? p))))

      (new (get-menu-item%)
           (label "Clear selection")
           (parent stepper-menu)
           (callback
            (lambda _ (send (send widget get-controller) select-syntax #f))))
      (new separator-menu-item% (parent stepper-menu))

      (menu-option/notify-box stepper-menu
                              "Show macro hiding panel"
                              (get-field show-hiding-panel? config))
      #;
      (new (get-menu-item%)
           (label "Show in new frame")
           (parent stepper-menu)
           (callback (lambda _ (send widget show-in-new-frame))))
      (new (get-menu-item%)
           (label "Remove selected term")
           (parent stepper-menu)
           (callback (lambda _ (send widget remove-current-term))))
      (new (get-menu-item%)
           (label "Reset mark numbering")
           (parent stepper-menu)
           (callback (lambda _ (send widget reset-primary-partition))))
      (let ([extras-menu
             (new (get-menu%)
                  (label "Extra options")
                  (parent stepper-menu))])
        (new checkable-menu-item%
             (label "Always suffix marked identifiers")
             (parent extras-menu)
             (callback
              (lambda (i e)
                (sb:current-suffix-option
                 (if (send i is-checked?)
                     'always
                     'over-limit))
                (send widget update/preserve-view))))
        (menu-option/notify-box extras-menu
                                "Highlight redex/contractum"
                                (get-field highlight-foci? config))
        (menu-option/notify-box extras-menu
                                "Highlight frontier"
                                (get-field highlight-frontier? config))
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
        (menu-option/notify-box extras-menu
                                "Force block->letrec transformation"
                                (get-field force-letrec-transformation? config))
        (menu-option/notify-box extras-menu
                                "(Debug) Catch internal errors?"
                                (get-field debug-catch-errors? config)))

      (frame:reorder-menus this)))

  ;; Stolen from stepper

  (define warning-color "yellow")
  (define warning-font normal-control-font)

  (define warning-canvas%
    (class canvas%
      (init-field warning)
      (inherit get-dc get-client-size)
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (send dc set-font warning-font) 
          (let-values ([(cw ch) (get-client-size)]
                       [(tw th dont-care dont-care2) (send dc get-text-extent warning)])
            (send dc set-pen (send the-pen-list find-or-create-pen warning-color 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush warning-color 'solid))
            (send dc draw-rectangle 0 0 cw ch)
            (send dc draw-text 
                  warning
                  (- (/ cw 2) (/ tw 2))
                  (- (/ ch 2) (/ th 2))))))
      (super-new)
      (inherit min-width min-height stretchable-height)
      (let-values ([(tw th dc dc2) (send (get-dc) get-text-extent warning warning-font)])
        (min-width (+ 2 (inexact->exact (ceiling tw))))
        (min-height (+ 2 (inexact->exact (ceiling th)))))
      (stretchable-height #f)))

  )
