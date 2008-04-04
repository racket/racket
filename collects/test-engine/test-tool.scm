(module test-tool scheme/base
  
  (require scheme/file scheme/class scheme/unit drscheme/tool framework mred)
  (require "test-display.scm")
  (provide tool@)
    
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (define (phase1) (void))
      (define (phase2) (void))
      
      ;Overriding interactions as the current-rep implementation
      (define (test-interactions-text%-mixin %)
        (class* % ()
          
          (inherit get-top-level-window get-definitions-text)
          
          (define/public (display-test-results test-display)
            (let* ([dr-frame (get-top-level-window)]
                   [ed-def (get-definitions-text)]
                   [tab (and ed-def (send ed-def get-tab))])
              (when (and dr-frame ed-def tab)
                  (send test-display display-settings dr-frame tab ed-def)
                  (send test-display display-results))))
          
          (super-instantiate ())
          )
        )
      
      (define (test-definitions-text%-mixin %)
        (class* % ()        
          (inherit begin-edit-sequence end-edit-sequence)

          (define colorer-frozen-by-test? #f)
          (define/public (test-froze-colorer?) colorer-frozen-by-test?)
          (define/public (toggle-test-status)
            (set! colorer-frozen-by-test?
                  (not colorer-frozen-by-test?)))
          
          (define/public (begin-test-color)
            (begin-edit-sequence #f))
          (define/public (end-test-color)
            (end-edit-sequence))
          
          (define/augment (on-delete start len)
            (begin-edit-sequence)
            (inner (void) on-delete start len))
          (define/augment (after-delete start len)
            (inner (void) after-delete start len)
            (when colorer-frozen-by-test?
              (send this thaw-colorer)
              (send this toggle-test-status))
            (end-edit-sequence))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (inner (void) after-insert start len)
            (when colorer-frozen-by-test?
              (send this thaw-colorer)
              (send this toggle-test-status))
            (end-edit-sequence))
          
          (super-instantiate ())))
      
      (define (test-frame-mixin %)
        (class* % ()

          (inherit get-current-tab)
                    
          (define/public (display-test-panel editor)
            (send test-panel update-editor editor)
            (unless (send test-panel is-shown?)
              (send test-frame add-child test-panel)
              (let ((test-box-size
                     (get-preference 'profj:test-dock-size (lambda () '(2/3 1/3)))))
                (send test-frame set-percentages test-box-size))
              ))
          (define test-panel null)
          (define test-frame null)
          
          (define test-windows null)
          (define/public (register-test-window t)
            (set! test-windows (cons t test-windows)))
          (define/public (deregister-test-window t)
            (set! test-windows (remq t test-windows)))
          
          (define/public (dock-tests)
            (for-each (lambda (t) (send t show #f)) test-windows)
            (let ((ed (send (get-current-tab) get-test-editor)))
              (when ed (display-test-panel ed))))
          (define/public (undock-tests)
            (send test-panel remove)
            (for-each (lambda (t) (send t show #t)) test-windows))
          
          (define/override (make-root-area-container cls parent)
            (let* ([outer-p (super make-root-area-container panel:vertical-dragable% parent)]
                   [louter-panel (make-object vertical-panel% outer-p)]
                   [test-p (make-object test-panel% outer-p '(deleted))]
                   [root (make-object cls louter-panel)])
              (set! test-panel test-p)
              (send test-panel update-frame this)
              (set! test-frame outer-p)
              root))
          
          (define/augment (on-tab-change from-tab to-tab)
            (let ((test-editor (send to-tab get-test-editor))
                  (panel-shown? (send test-panel is-shown?))
                  (dock? (get-preference 'profj:test-window:docked? (lambda () #f))))
              (cond
                ((and test-editor panel-shown? dock?)
                 (send test-panel update-editor test-editor))
                ((and test-editor dock?)
                 (display-test-panel test-editor))
                ((and panel-shown? (not dock?))
                 (undock-tests))
                (panel-shown? (send test-panel remove)))
              (inner (void) on-tab-change from-tab to-tab)))
                
          (super-instantiate () )))
      
      (define (test-tab%-mixin %)
        (class* % ()
          
          (inherit get-frame get-defs)
          
          (define test-editor #f)
          (define/public (get-test-editor) test-editor)
          (define/public (current-test-editor ed)
            (set! test-editor ed))
          
          (define test-window #f)
          (define/public (get-test-window) test-window)
          (define/public (current-test-window w) (set! test-window w))
          
          (define/public (update-test-preference test?)
            (let* ([language-settings
                    (preferences:get
                     (drscheme:language-configuration:get-settings-preferences-symbol))]
                   [language
                    (drscheme:language-configuration:language-settings-language
                     language-settings)]
                   [settings
                    (drscheme:language-configuration:language-settings-settings
                     language-settings)])
              (when (object-method-arity-includes? language 'update-test-setting 2)
                (let ((next-setting (drscheme:language-configuration:make-language-settings 
                                     language
                                     (send language update-test-setting settings test?))))
                  (preferences:set
                   (drscheme:language-configuration:get-settings-preferences-symbol)
                   next-setting)
                  (send (get-defs) set-next-settings next-setting)))))
          
          (define/augment (on-close)
            (when test-window
              (when (send test-window is-shown?)
                (send test-window show #f))
              (send (get-frame) deregister-test-window test-window))
            (inner (void) on-close))
          
          (super-instantiate () )))
      
      (drscheme:get/extend:extend-definitions-text test-definitions-text%-mixin)
      (drscheme:get/extend:extend-interactions-text test-interactions-text%-mixin)
      (drscheme:get/extend:extend-unit-frame test-frame-mixin)
      (drscheme:get/extend:extend-tab test-tab%-mixin)
          
      ))

  )
