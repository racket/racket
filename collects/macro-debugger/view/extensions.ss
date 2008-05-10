
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
         "warning.ss"
         "hiding-panel.ss"
         (prefix-in s: "../syntax-browser/widget.ss")
         (prefix-in s: "../syntax-browser/keymap.ss")
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/trace.ss"
         "../model/steps.ss"
         "cursor.ss"
         "../util/notify.ss")
(provide stepper-keymap%
         stepper-syntax-widget%)

;; Extensions

(define stepper-keymap%
  (class s:syntax-keymap%
    (init-field macro-stepper)
    (inherit-field config
                   controller
                   the-context-menu)
    (inherit add-function
             call-function)

    (define show-macro #f)
    (define hide-macro #f)

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
                      (refresh))))

    ;; Menu

    (inherit add-separator)

    (define/override (after-selection-items)
      (super after-selection-items)
      (add-separator)
      (set! show-macro
            (new menu-item% (label "Show selected identifier") (parent the-context-menu)
                 (callback (lambda (i e)
                             (call-function "hiding:show-macro" i e)))))
      (set! hide-macro
            (new menu-item% (label "Hide selected identifier") (parent the-context-menu)
                 (callback (lambda (i e)
                             (call-function "hiding:hide-macro" i e)))))
      (enable/disable-hide/show #f)
      (void))

    (define/private (enable/disable-hide/show ?)
      (send show-macro enable ?)
      (send hide-macro enable ?))

    (send controller listen-selected-syntax
          (lambda (stx)
            (enable/disable-hide/show (identifier? stx))))))

(define stepper-syntax-widget%
  (class s:widget%
    (init-field macro-stepper)
    (inherit get-text)
    (inherit-field controller)

    (define/override (setup-keymap)
      (new stepper-keymap%
           (editor (get-text))
           (config (send macro-stepper get-config))
           (controller controller)
           (macro-stepper macro-stepper)))

    (define/override (show-props show?)
      (super show-props show?)
      (send macro-stepper update/preserve-view))

    (super-new
     (config (send macro-stepper get-config)))))
