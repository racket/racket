#lang scheme/base
(require scheme/class
         scheme/gui
         unstable/gui/notify
         "interfaces.ss"
         "partition.ss")
(provide syntax-keymap%)

(define keymap/popup%
  (class* keymap% (keymap/popup<%>)
    (init editor)
    (super-new)
    (inherit add-function
             map-function
             chain-to-keymap)

    (define/public (add-context-menu-items menu)
      (void))

    (map-function "rightbutton" "popup-context-menu")
    (add-function "popup-context-menu"
                  (lambda (editor event)
                    (popup-context-menu editor event)))

    (define/private (popup-context-menu editor event)
      (define-values (x y)
        (send editor dc-location-to-editor-location
              (send event get-x)
              (send event get-y)))
      (define admin (send editor get-admin))
      (define menu (new popup-menu%))
      (add-context-menu-items menu)
      (send admin popup-menu menu x y))

    ;; FIXME: move out of constructor to use sites
    (chain-to-keymap (send editor get-keymap) #t)
    (send editor set-keymap this)))

(define syntax-keymap%
  (class keymap/popup%
    (init-field controller
                config)
    (inherit add-function
             map-function
             call-function
             chain-to-keymap)
    (super-new)

    (define/private (selected-syntax)
      (send controller get-selected-syntax))

    ;; Functionality

    (add-function "copy-syntax-as-text"
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

    (define/override (add-context-menu-items menu)
      (new menu-item% (label "Copy") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (and (selected-syntax) #t))))
           (callback
            (lambda (i e)
              (call-function "copy-syntax-as-text" i e))))
      (new separator-menu-item% (parent menu))
      (new menu-item%
           (label "Clear selection")
           (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (and (selected-syntax) #t))))
           (callback 
            (lambda (i e)
              (call-function "clear-syntax-selection" i e))))
      (menu-option/notify-box menu "View syntax properties"
                              (get-field props-shown? config)))))

