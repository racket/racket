
(module keymap mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           "interfaces.ss"
           "partition.ss")
  (provide syntax-keymap%
           context-menu%)

  (define syntax-keymap%
    (class keymap%
      (init editor)
      (init-field controller)

      (inherit add-function
               map-function
               chain-to-keymap)
      (super-new)

      (define/public (get-context-menu%)
        context-menu%)

      (define/public (make-context-menu)
        (new (get-context-menu%) (controller controller) (keymap this)))

      ;; Key mappings

      (map-function "rightbutton" "popup-context-window")

      ;; Functionality

      (add-function "popup-context-window"
                    (lambda (editor event)
                      (do-popup-context-window editor event)))

      (add-function "copy-text"
                    (lambda (_ event)
                      (define stx (send controller get-selected-syntax))
                      (send the-clipboard set-clipboard-string
                            (if stx 
                                (format "~s" (syntax-object->datum stx))
                                "")
                            (send event get-time-stamp))))

      (add-function "clear-syntax-selection"
                    (lambda (i e)
                      (send controller set-selected-syntax #f)))

      (add-function "show-syntax-properties"
                    (lambda (i e)
                      (error 'show-syntax-properties "not provided by this keymap")))

      ;; Attach to editor

      (chain-to-keymap (send editor get-keymap) #t)
      (send editor set-keymap this)

      (define/public (get-controller) controller)

      (define/private (do-popup-context-window editor event)
        (define-values (x y)
          (send editor dc-location-to-editor-location
                (send event get-x)
                (send event get-y)))
        (define admin (send editor get-admin))
        (send admin popup-menu (make-context-menu) x y))))

  (define context-menu%
    (class popup-menu%
      (init-field keymap)
      (init-field controller)
      (super-new)

      (field [copy-menu #f]
             [clear-menu #f]
             [props-menu #f])

      (define/public (add-edit-items)
        (set! copy-menu
              (new menu-item% (label "Copy") (parent this)
                   (callback (lambda (i e)
                               (send keymap call-function "copy-text" i e)))))
        (void))

      (define/public (after-edit-items)
        (void))

      (define/public (add-selection-items)
        (set! clear-menu
              (new menu-item%
                   (label "Clear selection")
                   (parent this)
                   (callback 
                    (lambda (i e)
                      (send keymap call-function "clear-syntax-selection" i e)))))
        (set! props-menu
              (new menu-item%
                   (label "Show syntax properties")
                   (parent this)
                   (callback 
                    (lambda (i e) 
                      (send keymap call-function "show-syntax-properties" i e)))))
        (void))

      (define/public (after-selection-items)
        (void))

      (define/public (add-partition-items)
        (let ([secondary (new menu% (label "identifier=?") (parent this))])
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
        (new separator-menu-item% (parent this)))

      (define/override (on-demand)
        (define stx (send controller get-selected-syntax))
        (send copy-menu enable (and stx #t))
        (send clear-menu enable (and stx #t))
        (super on-demand))

      ;; Initialization
      (add-edit-items)
      (after-edit-items)

      (add-separator)
      (add-selection-items)
      (after-selection-items)

      (add-separator)
      (add-partition-items)
      (after-partition-items)
      ))

  )
