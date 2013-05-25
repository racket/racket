#lang racket/base
(require racket/class
         racket/gui/base
         unstable/class-iop
         "interfaces.rkt"
         (prefix-in s: "../syntax-browser/widget.rkt")
         (prefix-in s: "../syntax-browser/keymap.rkt"))
(provide stepper-keymap%
         stepper-syntax-widget%)

;; Extensions

(define stepper-keymap%
  (class s:syntax-keymap%
    (init-field/i (macro-stepper widget<%>))
    (inherit-field config
                   controller)
    (inherit add-function
             map-function
             call-function)

    (define show-macro #f)
    (define hide-macro #f)

    (super-new)

    (define/public (get-hiding-panel)
      (send/i macro-stepper widget<%> get-macro-hiding-prefs))

    (map-function ":s" "hiding:show-macro")
    (map-function ":h" "hiding:hide-macro")

    (add-function "hiding:show-macro"
                  (lambda (i e)
                    (send*/i (get-hiding-panel) hiding-prefs<%>
                      (add-show-identifier)
                      (refresh))))

    (add-function "hiding:hide-macro"
                  (lambda (i e)
                    (send*/i (get-hiding-panel) hiding-prefs<%>
                      (add-hide-identifier)
                      (refresh))))

    ;; Menu

    (define/override (add-context-menu-items menu)
      (super add-context-menu-items menu)
      (new separator-menu-item% (parent menu))
      (new menu-item% (label "Show selected identifier") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (identifier? (send controller get-selected-syntax)))))
           (callback
            (lambda (i e)
              (call-function "hiding:show-macro" i e))))
      (new menu-item% (label "Hide selected identifier") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (identifier? (send controller get-selected-syntax)))))
           (callback
            (lambda (i e) (call-function "hiding:hide-macro" i e)))))))


(define stepper-syntax-widget%
  (class s:widget%
    (init-field/i (macro-stepper widget<%>))
    (inherit get-text)
    (inherit-field controller)

    (define/override (setup-keymap)
      (new stepper-keymap%
           (editor (get-text))
           (config (send/i macro-stepper widget<%> get-config))
           (controller controller)
           (macro-stepper macro-stepper)))

    (define/override (show-props show?)
      (super show-props show?)
      (when (send (send/i macro-stepper widget<%> get-config) get-refresh-on-resize?)
        (send/i macro-stepper widget<%> update/preserve-view)))

    (super-new
     (config (send/i macro-stepper widget<%> get-config)))))
