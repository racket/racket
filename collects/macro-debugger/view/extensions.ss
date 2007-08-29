
(module extensions mzscheme
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
           (prefix s: "../syntax-browser/widget.ss")
           "../model/deriv.ss"
           "../model/deriv-util.ss"
           "../model/trace.ss"
           "../model/hide.ss"
           "../model/steps.ss"
           "cursor.ss"
           "util.ss")
  (provide stepper-keymap%
           stepper-context-menu%
           stepper-syntax-widget%)

  ;; Extensions

  (define stepper-keymap%
    (class s:widget-keymap%
      (init-field macro-stepper)
      (inherit-field controller)
      (inherit add-function)

      (super-new)

      (define/override (get-context-menu%)
        stepper-context-menu%)

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
                        (refresh))))))

  (define stepper-context-menu%
    (class s:widget-context-menu%
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
        (send show-macro enable id?)
        (send hide-macro enable id?)
        (super on-demand))

      (super-new)))
  
  (define stepper-syntax-widget%
    (class s:widget%
      (init-field macro-stepper)
      (inherit get-text)

      (define/override (setup-keymap)
        (new stepper-keymap%
             (editor (get-text))
             (widget this)
             (macro-stepper macro-stepper)))

      (define/override (show-props show?)
        (super show-props show?)
        (send macro-stepper update/preserve-view))

      (super-new
       (config (new config-adapter%
                    (config (send macro-stepper get-config)))))))
  
  (define config-adapter%
    (class object%
      (init-field config)
      (define/public pref:props-percentage
        (case-lambda [() (send config get-props-percentage)]
                     [(v) (send config set-props-percentage v)]))
      (super-new)))
  )
