
(module frame mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "interfaces.ss")
  (provide frame@)
  
  (define frame@
    (unit/sig browser^
      (import prefs^
              widget^)
      
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
            (preferences:save)
            (inner (void) on-close))
          ))))
  )
