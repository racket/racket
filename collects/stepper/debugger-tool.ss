(module debugger-tool mzscheme
  (require (lib "contract.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "bitmap-label.ss" "mrlib")
           "debugger-sig.ss"
           "private/debugger-vc.ss"
           "private/debugger-model.ss"
	   "private/my-macros.ss")
    
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) (void))
      
      (define debugger-initial-width 500)
      (define debugger-initial-height 500)
      
      (define debugger-bitmap
        (bitmap-label-maker
         "Debug"
         (build-path (collection-path "icons") "foot.png")))

      (define debugger-unit-frame<%>
        (interface ()
          on-debugger-close))
      
      (define (debugger-unit-frame-mixin super%)
        (class* super% (debugger-unit-frame<%>)
          
          (inherit get-button-panel get-interactions-text get-definitions-text get-menu-bar)
          (rename [super-on-close on-close])
          
          (define debugger-exists #f)
          (define/public (on-debugger-close)
            (set! debugger-exists #f))
          
          (define breakpoints null)
          
          (super-instantiate ())
          
          ; DEBUGGER MENU
          
          (define debugger-menu (instantiate menu% () (label "Debugger") (parent (get-menu-bar))))
          
          (instantiate menu-item% () (label "Add Breakpoint") (parent debugger-menu) 
            (callback (lambda (dc-item dc-event)
                        (set! breakpoints (append breakpoints
                                                  (list (send (get-definitions-text) get-start-position)))))))
          
          (define (position->line-n-offset pos)
            (let* ([line (send (get-definitions-text) position-line pos)]
                   [offset (- pos (send (get-definitions-text) line-start-position line))])
              (values line offset)))
          
          (instantiate menu-item% () (label "List Breakpoints") (parent debugger-menu)
            (callback (lambda (dc-item dc-event)
                        (message-box "Current Breakpoints"
                                   (format "Current breakpoint positions: ~a\n" (apply string-append
                                                                                       (map (lambda (pos)
                                                                                              (let-values ([(line offset) (position->line-n-offset pos)])
                                                                                                (format "<~v:~v> (position ~v)\n" line offset pos)))
                                                                                            breakpoints)))
                                   this
                                   '(ok)))))
          
          (instantiate menu-item% () (label "Clear All Breakpoints") (parent debugger-menu)
            (callback (lambda (dc-item dc-event)
                        (set! breakpoints null))))
          
          (define program-expander
            (contract
             (-> (-> void?) ; init
                 (-> (union eof-object? syntax? (cons/p string? any/c)) (-> void?) void?) ; iter
                 void?)
             (lambda (init iter)
               (let* ([lang-settings 
                       (frame:preferences:get
                        (drscheme:language-configuration:get-settings-preferences-symbol))]
                      [lang (drscheme:language-configuration:language-settings-language lang-settings)]
                      [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
                 (drscheme:eval:expand-program
                  (drscheme:language:make-text/pos (get-definitions-text) 
                                                   0
                                                   (send (get-definitions-text)
                                                         last-position)) 
                  lang-settings
                  #f
                  (lambda ()
                    (init)
                    (error-value->string-handler
                     (lambda (val len)
                       (let ([sp (open-output-string)])
                         (send lang render-value val settings sp)
                         (let ([str (get-output-string sp)])
                           (if ((string-length str) . <= . len)
                               str
                               (string-append (substring str 0 (max 0 (- len 3))) "..."))))))
                    (drscheme:teachpack:install-teachpacks 
                     (frame:preferences:get 'drscheme:teachpacks))) ; this belongs in model, but I'd need a unit rewrite
                  void ; kill
                  iter)))
             'program-expander
             'caller))
          
          (define debugger-button 
            (make-object button%
              (debugger-bitmap this)
              (get-button-panel)
              (lambda (button evt)
                (if debugger-exists
                    (message-box/custom "Debugger Exists"
                                        "There is already a debugger window open for this program."
                                        "OK"
                                        #f
                                        #f
                                        #f
                                        '(default=1))
                    (begin
                      (set! debugger-exists #t)
                      (start-debugger program-expander this))))))
          
          (define breakpoint-origin (get-definitions-text))
          
          (define (start-debugger program-expander drs-window)
            (define-values/invoke-unit/sig (go)
             (compound-unit/sig 
               (import [EXPANDER : (program-expander)]
                       [BREAKPOINTS : (breakpoints breakpoint-origin)]
                       [DRS-WINDOW : (drs-window)])
               (link [MODEL : debugger-model^ (debugger-model@ VIEW-CONTROLLER EXPANDER BREAKPOINTS)] 
                     [VIEW-CONTROLLER : debugger-vc^ (debugger-vc@ MODEL DRS-WINDOW)])
               (export (var (MODEL go))))
              #f
              (program-expander)
              (breakpoints breakpoint-origin)
              (drs-window))
            (go))
          
          (rename [super-enable-evaluation enable-evaluation])
          (define/override (enable-evaluation)
            (send debugger-button enable #t)
            (super-enable-evaluation))
          
          (rename [super-disable-evaluation disable-evaluation])
          (define/override (disable-evaluation)
            (send debugger-button enable #f)
            (super-disable-evaluation))
          
          (send (get-button-panel) change-children
                (lx (cons debugger-button (remq debugger-button _))))))
      
      (drscheme:get/extend:extend-unit-frame debugger-unit-frame-mixin))))
