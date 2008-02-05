
#lang scheme/base
(require scheme/class
         scheme/gui
         framework/framework
         scheme/list
         "partition.ss"
         "prefs.ss"
         "widget.ss")
(provide browse-syntax
         browse-syntaxes
         make-syntax-browser
         syntax-browser-frame%
         syntax-widget/controls%)

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
    (init-field [config (new syntax-prefs%)])
    (super-new (label "Syntax Browser")
               (width (send config pref:width))
               (height (send config pref:height)))
    (define widget
      (new syntax-widget/controls%
           (parent this)
           (config config)))
    (define/public (get-widget) widget)
    (define/augment (on-close)
      (send config pref:width (send this get-width))
      (send config pref:height (send this get-height))
      (send widget shutdown)
      (inner (void) on-close))
    ))

;; syntax-widget/controls%
(define syntax-widget/controls%
  (class* widget% ()
    (inherit get-main-panel
             get-controller
             toggle-props)
    (super-new)
    (inherit-field config)

    (define -control-panel 
      (new horizontal-pane%
           (parent (get-main-panel))
           (stretchable-height #f)))

    ;; Put the control panel up front
    (send (get-main-panel) change-children
          (lambda (children)
            (cons -control-panel (remq -control-panel children))))

    (define -identifier=-choices (identifier=-choices))
    (define -choice
      (new choice% (label "identifer=?") (parent -control-panel)
           (choices (map car -identifier=-choices))
           (callback 
            (lambda (c e)
              (send (get-controller) set-identifier=?
                    (assoc (send c get-string-selection)
                           -identifier=-choices))))))
    (new button% 
         (label "Clear")
         (parent -control-panel)
         (callback (lambda _ (send (get-controller) select-syntax #f))))
    (new button%
         (label "Properties")
         (parent -control-panel)
         (callback (lambda _ (toggle-props))))

    (send (get-controller) listen-identifier=?
          (lambda (name+func)
            (send -choice set-selection
                  (or (send -choice find-string (car name+func)) 0))))
    ))
