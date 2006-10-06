
(module warning mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  (provide warnings-frame%)
  
  ;; warnings-frame%
  (define warnings-frame%
    (class frame%
      (super-new (label "Macro stepper warnings") (width 400) (height 300))
      
      (define text (new text% (auto-wrap #t)))
      (define ec (new editor-canvas% (parent this) (editor text)))
      (send text lock #t)
      
      (define -nonlinearity-text #f)
      (define -localactions-text #f)
      
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
      
      (define/private (add-text . strs)
        (send text lock #f)
        (for-each (lambda (s) (send text insert s)) strs)
        (send text insert "\n\n")
        (send text lock #t))
      
      (define/public (add-warning tag)
        (case tag
          ((nonlinearity)
           (add-nonlinearity-text))
          ((localactions)
           (add-localactions-text))))
      
      (send this show #t)))
  
  )