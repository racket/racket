(module debugger-tool mzscheme
  (require (lib "contract.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2)
        (print ((drscheme:language:get-default-mixin)) #;(drscheme:language:get-language-extensions)
               #;(preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))))
      
      (define debugger-bitmap
        (drscheme:unit:make-bitmap
         "Syntax Offset"
         (build-path (collection-path "mztake") "stock_macro-check-brackets-16.png")))
      
      (define (debugger-unit-frame-mixin super%)
        (class super%
          
          (inherit get-button-panel get-interactions-text get-definitions-text get-menu-bar)
          
          (super-instantiate ())
          
          (define debugger-button 
            (make-object button%
              (debugger-bitmap this)
              (get-button-panel)
              (lambda (button evt)
                (let* ([pos (send (get-definitions-text) get-start-position)]
                       [line (send (get-definitions-text) position-paragraph pos)]
                       [column (- pos (send (get-definitions-text) line-start-position
                                        (send (get-definitions-text) position-line pos)))])
                  
                  (message-box "Syntax Offset"
                               (format "Line: ~a~nColumn: ~a~nOffset: ~a" (add1 line) column pos))))))
          
          (define test-button
            (make-object button%
              "test-me"
              (get-button-panel)
              (lambda (button evt)
                ((lambda (iter)
                   (let* ([lang-settings
                           (preferences:get
                            (drscheme:language-configuration:get-settings-preferences-symbol))]
                          [lang (drscheme:language-configuration:language-settings-language lang-settings)]
                          [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
                     (drscheme:eval:expand-program
                      (drscheme:language:make-text/pos (get-definitions-text)
                                                       0
                                                       (send (get-definitions-text) last-position))
                      lang-settings
                      #f
                      (lambda ()
                        ;TODO error handler for exceptions
                        (error-value->string-handler
                         (lambda (val len)
                           (let ([sp (open-output-string)])
                             (send lang render-value val settings sp #f)
                             (let ([str (get-output-string sp)])
                               (if ((string-length str) . <= . len)
                                   str
                                   (string-append (substring str 0 (max 0 (- len 3))) "..."))))))
                        (drscheme:teachpack:install-teachpacks 
                         (preferences:get 'drscheme:teachpacks)))
                      void ; kill
                      iter)))
                 (lambda (stx thunk)
                   (unless (eof-object? stx)
                     (eval stx)
                     (printf ">> ~a~n~n" (syntax-object->datum stx))
                     (thunk)))))))
          
          (send (get-button-panel) change-children
            (lambda (_) (cons test-button (remq test-button 
                                                (cons debugger-button (remq debugger-button _))))))))
      
      (drscheme:get/extend:extend-unit-frame debugger-unit-frame-mixin))))
