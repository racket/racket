
(module frame mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           "interfaces.ss"
           "partition.ss")
  (provide frame@)
  
  (define frame@
    (unit
      (import prefs^
              widget^)
      (export browser^)

      ;; browse-syntax : syntax -> void
      (define (browse-syntax stx)
        (browse-syntaxes (list stx)))
      
      ;; browse-syntaxes : (list-of syntax) -> void
      (define (browse-syntaxes stxs)
        (let ((w (make-syntax-browser)))
          (for-each (lambda (stx)
                      (send w add-syntax stx)
                      (send w add-separator))
                    stxs)))
      
      ;; make-syntax-browser : -> syntax-browser<%>
      (define (make-syntax-browser)
        (let* ([view (new syntax-browser-frame%)])
          (send view show #t)
          (send view get-widget)))
      
      ;; syntax-browser-frame%
      (define syntax-browser-frame%
        (class* frame% ()
          (super-new (label "Syntax Browser")
                     (width (pref:width))
                     (height (pref:height)))
          (define widget
            (new syntax-widget/controls%
                 (parent this)
                 (pref:props-percentage pref:props-percentage)))
          (define/public (get-widget) widget)
          (define/augment (on-close)
            (pref:width (send this get-width))
            (pref:height (send this get-height))
            (send widget save-prefs)
            (inner (void) on-close))
          ))
      
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
            (cond [(assoc (send -choice get-string-selection)
                          -identifier=-choices)
                   => (lambda (p)
                        (send (get-controller)
                              on-update-identifier=? (car p) (cdr p)))]
                  [else #f]))
          (send (get-controller) add-identifier=?-listener
                (lambda (name func)
                  (send -choice set-selection
                        (or (send -choice find-string name) 0))))))
      
      ))
  )
