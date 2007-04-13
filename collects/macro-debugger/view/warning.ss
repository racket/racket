
(module warning mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))
  (provide warnings-frame%)

  (define include-message? #f)

  ;; warnings-frame%
  (define warnings-frame%
    (class frame%
      (super-new (label "Macro stepper warnings") (width 400) (height 300))

      (define text (new text:hide-caret/selection% (auto-wrap #t)))
      (define ec (new editor-canvas% (parent this) (editor text)))
      (send text lock #t)

      (define -nonlinearity-text #f)
      (define -localactions-text #f)
      (define -lifts-text #f)

      (define/private (add-nonlinearity-text)
        (unless -nonlinearity-text
          (set! -nonlinearity-text #t)
          (add-text "An opaque macro duplicated one of its subterms. "
                    "Macro hiding requires opaque macros to use their subterms linearly. "
                    "The macro stepper is showing the expansion of that macro use.")))
      (define/private (add-localactions-text)
        (unless -localactions-text
          (set! -localactions-text #t)
          (add-text "An opaque macro called local-expand, syntax-local-lift-expression, "
                    "etc. Macro hiding cannot currently handle local actions. "
                    "The macro stepper is showing the expansion of that macro use.")))
      (define/private (add-lifts-text)
        (unless -lifts-text
          (set! -lifts-text #t)
          (add-text "A transparent macro called syntax-local-lift-expression or "
                    "syntax-local-lift-module-end-declaration. "
                    "The macro stepper is only hiding macro after the "
                    "lifts are caught.")))

      (define/public (add-text . strs)
        (send text lock #f)
        (for-each (lambda (s) (send text insert s)) strs)
        (send text insert "\n\n")
        (send text lock #t))
      
      (define/public (add-warning tag message)
        (case tag
          ((nonlinearity)
           (add-nonlinearity-text))
          ((localactions)
           (add-localactions-text))
          ((lifts)
           (add-lifts-text)))
        (when include-message?
          (add-text message)))

      (send this show #t)))
  
  )