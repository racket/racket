#lang scheme/base
(require scheme/class
         scheme/gui
         unstable/gui/notify
         "interfaces.ss"
         "partition.ss")
(provide smart-keymap%
         syntax-keymap%)

(define smart-keymap%
  (class keymap%
    (init editor)

    (inherit add-function
             map-function
             chain-to-keymap)

    (super-new)

    (define/public (get-context-menu%)
      smart-context-menu%)

    (field (the-context-menu #f))
    (set! the-context-menu (new (get-context-menu%)))

    (map-function "rightbutton" "popup-context-window")
    (add-function "popup-context-window"
                  (lambda (editor event)
                    (do-popup-context-window editor event)))

    (chain-to-keymap (send editor get-keymap) #t)
    (send editor set-keymap this)

    (define/private (do-popup-context-window editor event)
      (define-values (x y)
        (send editor dc-location-to-editor-location
              (send event get-x)
              (send event get-y)))
      (define admin (send editor get-admin))
      (send admin popup-menu the-context-menu x y))

    ))

(define smart-context-menu%
  (class popup-menu%
    (define on-demand-actions null)
    (define/public (add-on-demand p)
      (set! on-demand-actions (cons p on-demand-actions)))

    (define/override (on-demand)
      (super on-demand)
      (for-each (lambda (p) (p)) on-demand-actions))

    (super-new)))

(define syntax-keymap%
  (class smart-keymap%
    (init-field controller
                config)

    (inherit add-function
             map-function
             call-function
             chain-to-keymap)
    (inherit-field the-context-menu)
    (field [copy-menu #f]
           [clear-menu #f]
           [props-menu #f])
    (super-new)

    ;; Functionality

    (define/public (get-controller) controller)

    (add-function "copy-text"
                  (lambda (_ event)
                    (define stx (send controller get-selected-syntax))
                    (send the-clipboard set-clipboard-string
                          (if stx 
                              (format "~s" (syntax->datum stx))
                              "")
                          (send event get-time-stamp))))

    (add-function "clear-syntax-selection"
                  (lambda (i e)
                    (send controller set-selected-syntax #f)))

    (add-function "show-syntax-properties"
                  (lambda (i e)
                    (send config set-props-shown? #t)))

    (add-function "hide-syntax-properties"
                  (lambda (i e)
                    (send config set-props-shown? #f)))

    (define/private (selected-syntax)
      (send controller get-selected-syntax))

    (define/public (add-menu-items)
      (set! copy-menu
            (new menu-item% (label "Copy") (parent the-context-menu)
                 (demand-callback
                  (lambda (i)
                    (send i enable (and (selected-syntax) #t))))
                 (callback
                  (lambda (i e)
                    (call-function "copy-text" i e)))))
      (add-separator)
      (set! clear-menu
            (new menu-item%
                 (label "Clear selection")
                 (parent the-context-menu)
                 (demand-callback
                  (lambda (i)
                    (send i enable (and (selected-syntax) #t))))
                 (callback 
                  (lambda (i e)
                    (call-function "clear-syntax-selection" i e)))))
      (set! props-menu
            (menu-option/notify-box the-context-menu
                                    "View syntax properties"
                                    (get-field props-shown? config))
            #;
            (new menu-item%
                 (label "Show syntax properties")
                 (parent the-context-menu)
                 (demand-callback
                  (lambda (i)
                    (if (send config get-props-shown?)
                        (send i set-label "Hide syntax properties")
                        (send i set-label "Show syntax properties"))))
                 (callback 
                  (lambda (i e)
                    (if (send config get-props-shown?)
                        (call-function "hide-syntax-properties" i e)
                        (call-function "show-syntax-properties" i e))))))
      (void))

    (define/public (add-separator)
      (new separator-menu-item% (parent the-context-menu)))

    ;; Initialize menu

    (add-menu-items)
    ))
