
#lang scheme/base
(require scheme/class
         mred
         framework)
(provide warnings%
         stepper-warnings%)

;; warnings%
(define warnings%
  (class object%
    (init parent)
    (super-new)

    (define super-panel
      (new vertical-panel%
           (parent parent)
           (stretchable-height #f)))
    (define main-panel
      (new horizontal-panel%
           (parent super-panel)
           (style '(deleted border))))
    (define label (new message% (parent main-panel) (label "Warnings")))
    (define text (new text:hide-caret/selection% (auto-wrap #t)))
    (define ec
      (new editor-canvas%
           (parent main-panel)
           (editor text)
           (style '(auto-vscroll auto-hscroll))
           (line-count 3)))
    (define dismiss
      (new button%
           (parent main-panel)
           (label "Hide")
           (stretchable-height #t)
           (callback (lambda _ (show #f)))))
    (send text set-autowrap-bitmap #f)
    (send text lock #t)
    
    (define/public (get-text) text)
    
    (define/public (show ?)
      (send super-panel change-children
            (lambda _
              (if ?
                  (list main-panel)
                  null))))
    
    ;; Warning management
    (define keys null)
    
    ;; clear : -> void
    (define/public (clear)
      (set! keys null)
      (send* text
        (lock #f)
        (erase)
        (lock #t))
      (show #f))
    
    ;; add : symbol string ... -> void
    (define/public (add key . strs)
      (unless (memq key keys)
        (send text lock #f)
        (for-each (lambda (s) (send text insert s)) strs)
        (send text insert "\n\n")
        (send text scroll-to-position 0)
        (send text lock #t)
        (show #t)))
    
    ))

(define stepper-warnings%
  (class warnings%
    (super-new)
    (inherit add)
    
    (define/private (add-nonlinearity-warning)
      (add
       'nonlinearity
       "An opaque macro duplicated one of its subterms. "
       "Macro hiding requires opaque macros to use their subterms linearly. "
       "The macro stepper is showing the expansion of that macro use."))
    (define/private (add-localactions-warning)
      (add
       'localactions
       "An opaque macro called local-expand, syntax-local-lift-expression, "
       "etc. Macro hiding cannot currently handle local actions. "
       "The macro stepper is showing the expansion of that macro use."))
    (define/private (add-lifts-warning)
      (add
       'lifts
       "A transparent macro called syntax-local-lift-expression or "
       "syntax-local-lift-module-end-declaration. "
       "The macro stepper is only hiding macro after the "
       "lifts are caught."))

    (define/private (add-lift/let-warning)
      (add
       'lift/let
       "Lifts occurred during the expansion of phase 1 or higher code. "
       "The macro stepper is showing some expansions that should be hidden."))

    (define/private (add-hidden-lift-site-warning)
      (add
       'hidden-lift-site
       "An opaque macro contained the target of a lifted declaration."
       "The macro stepper is showing the expansion of that macro use."))

    (define/private (add-hidden-lift-site/continuing-warning)
      (add
       'hidden-lift-site/continuing
       "The target of a lifted declaration was a hidden #%module-begin context. "
       "The macro stepper is omitting the lifted declaration."))
    
    (define/public (add-warning tag args)
      (case tag
        ((nonlinearity)
         (add-nonlinearity-warning))
        ((localactions)
         (add-localactions-warning))
        ((lifts)
         (add-lifts-warning))
        ((lift/let)
         (add-lift/let-warning))
        ((hidden-lift-site)
         (add-hidden-lift-site-warning))
        ((hidden-lift-site/continuing)
         (add-hidden-lift-site/continuing-warning))))
    ))
