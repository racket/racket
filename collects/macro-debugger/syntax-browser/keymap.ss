
#lang scheme/base
(require scheme/class
         scheme/gui
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

    (define/public (add-edit-items)
      (set! copy-menu
            (new menu-item% (label "Copy") (parent the-context-menu)
                 (callback (lambda (i e)
                             (call-function "copy-text" i e)))))
      (void))

    (define/public (after-edit-items)
      (void))

    (define/public (add-selection-items)
      (set! clear-menu
            (new menu-item%
                 (label "Clear selection")
                 (parent the-context-menu)
                 (callback 
                  (lambda (i e)
                    (call-function "clear-syntax-selection" i e)))))
      (set! props-menu
            (new menu-item%
                 (label "Show syntax properties")
                 (parent the-context-menu)
                 (callback 
                  (lambda (i e)
                    (if (send config get-props-shown?)
                        (call-function "hide-syntax-properties" i e)
                        (call-function "show-syntax-properties" i e))))))
      (void))

    (define/public (after-selection-items)
      (void))

    (define/public (add-partition-items)
      (let ([secondary (new menu% (label "identifier=?") (parent the-context-menu))])
        (for-each
         (lambda (name func)
           (let ([this-choice
                  (new checkable-menu-item%
                       (label name)
                       (parent secondary)
                       (callback 
                        (lambda (i e)
                          (send controller set-identifier=? 
                                (cons name func)))))])
             (send controller listen-identifier=?
                   (lambda (name+proc)
                     (send this-choice check (eq? name (car name+proc)))))))
         (map car (identifier=-choices))
         (map cdr (identifier=-choices))))
      (void))

    (define/public (after-partition-items)
      (void))

    (define/public (add-separator)
      (new separator-menu-item% (parent the-context-menu)))

    ;; Initialize menu

    (add-edit-items)
    (after-edit-items)

    (add-separator)
    (add-selection-items)
    (after-selection-items)

    (add-separator)
    (add-partition-items)
    (after-partition-items)

    (send the-context-menu add-on-demand
          (lambda ()
            (define stx (send controller get-selected-syntax))
            (send copy-menu enable (and stx #t))
            (send clear-menu enable (and stx #t))))
    (send config listen-props-shown?
          (lambda (shown?)
            (send props-menu set-label
                  (if shown?
                      "Hide syntax properties"
                      "Show syntax properties"))))))
