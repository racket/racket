
(module widget mzscheme
  (require "interfaces.ss"
           "controller.ss"
           "typesetter.ss"
           "hrule-snip.ss"
           "properties.ss"
           "partition.ss"
           "prefs.ss"
           "util.ss"
           (lib "list.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred"))
  (provide syntax-controller%
           syntax-widget%
           syntax-browser-frame%)

  ;; syntax-widget%
  ;; A syntax-widget creates its own syntax-controller.
  (define syntax-widget%
    (class* object% (syntax-browser<%> syntax-properties-controller<%>)
      (init parent)

      (define -main-panel (new vertical-panel% (parent parent)))
      (define -split-panel (new panel:horizontal-dragable% (parent -main-panel)))
      (define -text (new text%))
      (define -ecanvas (new editor-canvas% (parent -split-panel) (editor -text)))
      (define -props-panel (new horizontal-panel% (parent -split-panel)))
      (define props (new properties-view% (parent -props-panel)))
      (define -saved-panel-percentages #f)

      (define controller
        (new syntax-controller%
             (properties-controller this)))

      #;(send -text hide-caret #t)
      (send -text lock #t)
      (send -split-panel set-percentages 
            (let ([pp (pref:props-percentage)]) (list (- 1 pp) pp)))
      (toggle-props)

      ;; syntax-properties-controller<%> methods

      (define/public (set-syntax stx)
        (send props set-syntax stx))

      (define/public (show ?)
        (if ? (show-props) (hide-props)))

      (define/public (is-shown?)
        (send -props-panel is-shown?))

      (define/public (toggle-props)
        (if (send -props-panel is-shown?)
            (hide-props)
            (show-props)))

      (define/public (hide-props)
        (when (send -props-panel is-shown?)
          (set! -saved-panel-percentages (send -split-panel get-percentages))
          (send -split-panel delete-child -props-panel)
          (send -props-panel show #f)))

      (define/public (show-props)
        (unless (send -props-panel is-shown?)
          (send -split-panel add-child -props-panel)
          (send -split-panel set-percentages -saved-panel-percentages)
          (send -props-panel show #t)))

      ;;

      (define/public (get-controller) controller)

      ;;

      (define/public (get-main-panel) -main-panel)

      (define/public (on-close)
        (unless (= (cadr -saved-panel-percentages) (pref:props-percentage))
          (pref:props-percentage (cadr -saved-panel-percentages))))

      ;; syntax-browser<%> Methods

      (define/public (add-text text)
        (with-unlock -text
          (send -text insert text)))

      (define/public add-syntax
        (case-lambda
          [(stx) 
           (internal-add-syntax stx null #f)]
          [(stx hi-stxs hi-color)
           (internal-add-syntax stx hi-stxs hi-color)]))

      (define/public (add-separator)
        (with-unlock -text
          (send* -text
            (insert (new hrule-snip%))
            (insert "\n"))))

      (define/public (erase-all)
        (with-unlock -text (send -text erase))
        (send controller erase))

      (define/public (select-syntax stx)
        (send controller select-syntax stx))

      (define/public (get-text) -text)

      (define/private (internal-add-syntax stx hi-stxs hi-color)
        (with-unlock -text
          (let ([current-position (send -text last-position)])
            (let* ([new-ts (new typesetter-for-text%
                                (controller controller)
                                (syntax stx)
                                (text -text))]
                   [new-colorer (send new-ts get-colorer)])
              (send* -text
                (insert "\n")
                (scroll-to-position current-position))
              (unless (null? hi-stxs)
                (send new-colorer highlight-syntaxes hi-stxs hi-color))))))

      (super-new)))

  ;; syntax-widget/controls%
  (define syntax-widget/controls%
    (class* syntax-widget% ()
      (inherit get-main-panel
               get-controller
               toggle-props)

      (super-new)

      (define -control-panel 
        (new horizontal-pane% (parent (get-main-panel)) (stretchable-height #f)))

      ;; Put the control panel up front
      (send (get-main-panel) change-children
            (lambda (children)
              (cons -control-panel (remq -control-panel children))))

      (define -identifier=-choices (identifier=-choices))
      (define -choice
        (new choice% (label "identifer=?") (parent -control-panel)
             (choices (map car -identifier=-choices))
             (callback (lambda _ (on-update-identifier=?-choice)))))
      (new button% 
           (label "Clear")
           (parent -control-panel)
           (callback (lambda _ (send (get-controller) select-syntax #f))))
      (new button%
           (label "Properties")
           (parent -control-panel)
           (callback (lambda _ (toggle-props))))

      (define/private (on-update-identifier=?-choice)
        (let ([id=? (get-identifier=?)])
          (send (get-controller) on-update-identifier=? id=?)))

      (define/private (get-identifier=?)
        (cond [(assoc (send -choice get-string-selection) 
                      -identifier=-choices)
               => cdr]
              [else #f]))))


  ;; syntax-browser-frame%
  (define syntax-browser-frame%
    (class* frame% ()
      (super-new (label "Syntax Browser")
                 (width (pref:width))
                 (height (pref:height)))
      (define widget (new syntax-widget/controls% (parent this)))
      (define/public (get-widget) widget)
      (define/augment (on-close)
        (pref:width (send this get-width))
        (pref:height (send this get-height))
        (send widget on-close)
        (preferences:save)
        (inner (void) on-close))
      ))
  )
