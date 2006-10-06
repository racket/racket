
(module keymap mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           "interfaces.ss"
           "partition.ss")
  (provide keymap@
           context-menu@)
  
  (define keymap@
    (unit/sig keymap^
      (import)
      
      (define syntax-keymap%
        (class keymap%
          (init editor)
          (init-field context-menu)
          
          (inherit add-function
                   map-function
                   chain-to-keymap)
          (super-new)
          
          ;; Initialization
          (map-function "rightbutton" "popup-context-window")
          (add-function "popup-context-window"
                        (lambda (editor event)
                          (do-popup-context-window editor event)))
          
          ;; Attach to editor
          (chain-to-keymap (send editor get-keymap) #t)
          (send editor set-keymap this)
          
          (define/private (do-popup-context-window editor event)
            (define-values (x y)
              (send editor dc-location-to-editor-location
                    (send event get-x)
                    (send event get-y)))
            (define admin (send editor get-admin))
            (send admin popup-menu context-menu x y))))))
  
  (define context-menu@
    (unit/sig context-menu^
      (import snip^)

      (define context-menu%
        (class popup-menu%
          (init-field controller)
          (super-new)

          (define copy-menu #f)
          (define copy-syntax-menu #f)
          (define clear-menu #f)
          
          (define/public (add-edit-items)
            (set! copy-menu
                  (new menu-item% (label "Copy") (parent this)
                       (callback (lambda (i e)
                                   (define stx (send controller get-selected-syntax))
                                   (send the-clipboard set-clipboard-string
                                         (if stx 
                                             (format "~s" (syntax-object->datum stx))
                                             "")
                                         (send e get-time-stamp))))))
            (set! copy-syntax-menu
                  (new menu-item% (label "Copy syntax") (parent this)
                       (callback (lambda (i e)
                                   (define stx (send controller get-selected-syntax))
                                   (define t (new text%))
                                   (send t insert
                                         (new syntax-snip%
                                              (syntax stx)
                                              #;(controller controller)))
                                   (send t select-all)
                                   (send t copy)))))
            (void))

          (define/public (after-edit-items)
            (void))

          (define/public (add-selection-items)
            (set! clear-menu
                  (new menu-item%
                       (label "Clear selection")
                       (parent this)
                       (callback (lambda _ (send controller select-syntax #f)))))
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
                                (send controller on-update-identifier=? name func))))])
                   (send controller add-identifier=?-listener
                         (lambda (new-name new-id=?) 
                           (send this-choice check (eq? name new-name))))))
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
            (send copy-syntax-menu enable (and stx #t))
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

          ))))
  )
