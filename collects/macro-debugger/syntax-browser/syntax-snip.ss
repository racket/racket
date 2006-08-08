
(module syntax-snip mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           "interfaces.ss"
           "prefs.ss"
           "properties.ss"
           "typesetter.ss"
           "widget.ss"
           "partition.ss")
  (provide syntax-snip%
           super-syntax-snip%)

  (define current-syntax-controller (make-parameter #f))

  (define (the-syntax-controller)
    (let ([controller (current-syntax-controller)])
      (or controller
          (let ([controller (new syntax-controller%)])
            (current-syntax-controller controller)
            controller))))


  ;; syntax-snip%
  (define syntax-snip%
    (class* editor-snip% ()
      (init-field ((stx syntax)))
      (init-field controller)

      (define -outer (new text%))
      (super-new (editor -outer))

      ;; Initialization
      (send -outer begin-edit-sequence)
      (initialize -outer)
      (outer:insert "Syntax browser" style:bold)
      (outer:insert "  ")
      (outer:insert "Clear" style:hyper
                    (lambda (x y z) (send controller select-syntax #f)))
      (outer:insert "  ")
      (outer:insert "Properties" style:hyper
                    (lambda (x y z)
                      (send (send controller get-properties-controller)
                            show #t)))
      (outer:insert "\n")
      (new typesetter-for-text%
           (syntax stx)
           (controller controller)
           (text -outer))
      (send -outer lock #t)
      (send -outer end-edit-sequence)
      (send -outer hide-caret #t)

      (define/public (initialize outer)
        (void))
      
      (define/private outer:insert
        (case-lambda
          [(obj)
           (outer:insert obj style:normal)]
          [(text style)
           (outer:insert text style #f)]
          [(text style clickback)
           (let ([start (send -outer last-position)])
             (send -outer insert text)
             (let ([end (send -outer last-position)])
               (send -outer change-style style start end #f)
               (when clickback
                 (send -outer set-clickback start end clickback))))]))
      
      ;; snip% Methods
      
      (define/override (copy)
        (new syntax-snip% (controller controller) (syntax stx)))

      ))

  (define subservient-syntax-snip%
    (class syntax-snip%
      (init-field f)
      (define/override (initialize outer)
        (f outer))
      (super-new)))
  
  (define style:normal (make-object style-delta% 'change-normal))
  (define style:hyper
    (let ([s (make-object style-delta% 'change-normal)])
      (send s set-delta 'change-toggle-underline)
      (send s set-delta-foreground "blue")
      s))
  (define style:bold
    (let ([s (make-object style-delta% 'change-normal)])
      (send s set-delta 'change-bold)
      s))

  (define (show-icon) (make-object image-snip% (build-path (collection-path "icons") "turn-up.png")))
  (define (hide-icon) (make-object image-snip% (build-path (collection-path "icons") "turn-down.png")))
  
  (define super-syntax-snip%
    (class* editor-snip% ()
      (init-field ((stx syntax)))
      (init-field (controller (the-syntax-controller)))
      
      (define -outer (new text%))
      (super-new (editor -outer) (with-border? #f))

      (define/private (hide-me)
        (send* -outer
          (lock #f)
          (erase))
        (outer:insert (show-icon) style:hyper (lambda _ (show-me)))
        (outer:insert "#<syntax>")
        (send -outer lock #t))
      
      (define/private (show-me)
        (send* -outer
          (lock #f)
          (erase))
        (outer:insert (new subservient-syntax-snip%
                           (syntax stx)
                           (controller controller)
                           (f (lambda (t)
                                (let* ([start (send t last-position)]
                                       [_ (send t insert (hide-icon))]
                                       [end (send t last-position)])
                                  (send t insert " ")
                                  (send t change-style style:hyper start end #f)
                                  (send t set-clickback start end (lambda _ (hide-me))))))))
        (send* -outer
          (lock #t)))
      
      (define/private outer:insert
        (case-lambda
          [(obj)
           (outer:insert obj style:normal)]
          [(text style)
           (outer:insert text style #f)]
          [(text style clickback)
           (let ([start (send -outer last-position)])
             (send -outer insert text)
             (let ([end (send -outer last-position)])
               (send -outer change-style style start end #f)
               (when clickback
                 (send -outer set-clickback start end clickback))))]))
      
      (define/override (copy)
        (new super-syntax-snip% (controller controller) (syntax stx)))

      (hide-me)
      (send -outer hide-caret #t)
      (send -outer lock #t)
      ))
  
  )
