(module debugger-tool mzscheme
  (require (lib "contract.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
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
          
          (send (get-button-panel) change-children
            (lambda (_) (cons debugger-button (remq debugger-button _))))))
      
      (drscheme:get/extend:extend-unit-frame debugger-unit-frame-mixin))))