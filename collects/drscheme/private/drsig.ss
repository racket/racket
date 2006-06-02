
(module drsig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide drscheme:eval^
           drscheme:debug^
           drscheme:module-language^
           drscheme:get-collection^
           drscheme:main^
           drscheme:init^
           drscheme:language-configuration^
           drscheme:language-configuration/internal^
           drscheme:tools^
           drscheme:get/extend^
           drscheme:unit^
           drscheme:frame^
           drscheme:program^
           drscheme:text^
           drscheme:rep^
           drscheme:app^
           drscheme:draw-arrow^
           drscheme:help-desk^
           drscheme:language^
           drscheme:teachpack^
           drscheme:multi-file-search^
           drscheme:module-overview^
           drscheme:font^
           drscheme:modes^
           drscheme:tool-exports^
           drscheme:tool^
	   drscheme^)

  (define-signature drscheme:modes^
    (add-mode
     get-modes
     add-initial-modes
     (struct mode (name surrogate repl-submit matches-language) 
             -setters
             (- make-mode))))
  
  (define-signature drscheme:font^
    (setup-preferences))
  
  (define-signature drscheme:debug^
    (make-debug-error-display-handler
     make-debug-error-display-handler/text
     make-debug-eval-handler
     hide-backtrace-window
     print-bug-to-stderr
     
     profile-definitions-text-mixin
     profile-tab-mixin
     profile-unit-frame-mixin
     profiling-enabled

     test-coverage-enabled
     test-coverage-interactions-text-mixin
     test-coverage-definitions-text-mixin
     test-coverage-tab-mixin
     
     add-prefs-panel
     
     show-error-and-highlight
     open-and-highlight-in-file
     show-backtrace-window
     get-cm-key
     
     display-srcloc-in-error
     show-syntax-error-context))
  
  (define-signature drscheme:module-language^
    (add-module-language
     module-language<%>
     module-language-put-file-mixin))
  
  (define-signature drscheme:get-collection^
    (get-file/collection))
  
  (define-signature drscheme:main^ ())
  
  (define-signature drscheme:init^
    (original-output-port
     original-error-port
     original-error-display-handler
     primitive-eval
     primitive-load
     error-display-handler-message-box-title
     system-custodian
     system-eventspace
     system-namespace
     first-dir))
  
  (define-signature drscheme:language-configuration^
    (add-language
     get-languages
     (struct language-settings (language settings) -setters)
     get-settings-preferences-symbol
     language-dialog
     fill-language-dialog))
  
  (define-signature drscheme:language-configuration/internal^
    (add-info-specified-languages
     get-default-language-settings
     (open drscheme:language-configuration^)
     settings-preferences-symbol

     add-built-in-languages
     
     ;; for the language dialog
     add-new-teachpack
     clear-all-teachpacks))
  
  (define-signature drscheme:tools^
    ((struct successful-tool (spec bitmap name url))
     get-successful-tools
     only-in-phase
     load/invoke-all-tools))

  (define-signature drscheme:get/extend^
    (extend-tab
     extend-interactions-text
     extend-definitions-text
     extend-interactions-canvas
     extend-definitions-canvas
     extend-unit-frame
     get-tab
     get-interactions-text
     get-definitions-text
     get-interactions-canvas
     get-definitions-canvas
     get-unit-frame))
    
  (define-signature drscheme:unit^
    (tab%
     tab<%>
     frame% 
     frame<%>
     definitions-canvas%
     get-definitions-text%
     definitions-text<%>
     interactions-canvas%
     open-drscheme-window
     find-symbol
     get-program-editor-mixin
     add-to-program-editor-mixin))
  
  (define-signature drscheme:frame^
    (<%>
     mixin
     basics-mixin
     basics<%>
     create-root-menubar))
  
  (define-signature drscheme:program^
    (frame%))

  (define-signature drscheme:eval^
    (expand-program
     expand-program/multiple
     traverse-program/multiple
     build-user-eventspace/custodian
     set-basic-parameters
     get-snip-classes))
  
  (define-signature drscheme:text^
    (text<%>
     text%))
  
  (define-signature drscheme:setup^ 
    (do-setup))
  
  (define-signature drscheme:rep^
    (drs-bindings-keymap-mixin
     current-rep
     current-language-settings
     current-value-port
     get-drs-bindings-keymap
     error-delta
     text%
     text<%>
     context<%>))
  
  (define-signature drscheme:app^
    (about-drscheme
     invite-tour
     add-language-items-to-help-menu
     add-important-urls-to-help-menu
     switch-language-to))
  
  (define-signature drscheme:draw-arrow^
    (draw-arrow))
  
  (define-signature drscheme:help-desk^
    (goto-help
     goto-tour
     goto-release-notes
     goto-plt-license
     help-desk
     get-docs
     open-url
     add-help-desk-font-prefs))
  
  (define-signature drscheme:language^
    (get-default-mixin
     extend-language-interface
     get-language-extensions
     
     create-module-based-launcher
     create-module-based-stand-alone-executable
     create-module-based-distribution

     create-distribution-for-executable

     create-executable-gui
     put-executable
     
     ;(struct loc (source position line column span) -setters)

     (struct text/pos (text start end) -setters)
     (struct simple-settings (case-sensitive 
                              printing-style 
                              fraction-style
                              show-sharing
                              insert-newlines
                              annotations)
             -setters)
     simple-settings->vector

     simple-module-based-language-config-panel
     
     add-snip-value

     register-capability
     capability-registered?
     get-capability-default
     get-capability-contract
     
     language<%>
     module-based-language<%>
     simple-module-based-language<%>
     simple-module-based-language%
     simple-module-based-language->module-based-language-mixin
     module-based-language->language-mixin))

  (define-signature drscheme:teachpack^
    (install-teachpacks
     marshall-teachpack-cache
     unmarshall-teachpack-cache
     launcher-init-code
     launcher-modules-to-embed
     new-teachpack-cache
     teachpack-cache?
     teachpack-cache-filenames
     teachpack-cache-require-specs
     set-teachpack-cache-filenames!))

  (define-signature drscheme:multi-file-search^
    (multi-file-search))
  
  (define-signature drscheme:module-overview^
    (module-overview
     make-module-overview-pasteboard
     fill-pasteboard))

  (define-signature drscheme:tool-exports^
    (phase1 
     phase2))
  
  (define-signature drscheme:tool^
    ((unit drscheme:debug : drscheme:debug^)
     (unit drscheme:unit : drscheme:unit^)
     (unit drscheme:rep : drscheme:rep^)
     (unit drscheme:frame : drscheme:frame^)
     (unit drscheme:get/extend : drscheme:get/extend^)
     (unit drscheme:language-configuration : drscheme:language-configuration^)
     (unit drscheme:language : drscheme:language^)
     (unit drscheme:help-desk : drscheme:help-desk^)
     (unit drscheme:eval : drscheme:eval^)
     (unit drscheme:teachpack : drscheme:teachpack^)
     (unit drscheme:modes : drscheme:modes^)))

  (define-signature drscheme^
    ((unit drscheme:teachpack : drscheme:teachpack^)
     (unit drscheme:language-configuration : drscheme:language-configuration/internal^))))
