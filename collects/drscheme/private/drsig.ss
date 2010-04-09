#lang scheme/base
(require scheme/unit)

(provide drscheme:eval^
         drscheme:debug^
         drscheme:module-language^
         drscheme:module-language-tools^
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
         drscheme:multi-file-search^
         drscheme:module-overview^
         drscheme:font^
         drscheme:modes^
         drscheme:tracing^
         drscheme:tool-exports^
         drscheme:tool^
         drscheme:tool-cm^)

(define-signature drscheme:modes-cm^
  ())
(define-signature drscheme:modes^ extends drscheme:modes-cm^
  (add-mode
   get-modes
   add-initial-modes
   (struct mode (name surrogate repl-submit matches-language) 
           #:omit-constructor)))

(define-signature drscheme:font-cm^
  ())
(define-signature drscheme:font^ extends drscheme:font-cm^
  (setup-preferences))

(define-signature drscheme:debug-cm^
  (profile-definitions-text-mixin
   profile-tab-mixin
   profile-unit-frame-mixin
   test-coverage-interactions-text-mixin
   test-coverage-definitions-text-mixin
   test-coverage-tab-mixin))
(define-signature drscheme:debug^ extends drscheme:debug-cm^
  (make-debug-error-display-handler
   make-debug-eval-handler
   error-display-handler/stacktrace
   bug-info->ticket-url
   test-coverage-enabled
   profiling-enabled
   
   add-prefs-panel
   
   get-error-color
   
   hide-backtrace-window
   show-backtrace-window
   show-backtrace-window/edition-pairs
   open-and-highlight-in-file
   
   small-planet-bitmap

   srcloc->edition/pair
   
   
   ;show-error-and-highlight
   ;print-bug-to-stderr
   ;display-srclocs-in-error
   ;show-syntax-error-context
   ))

(define-signature drscheme:module-language-cm^
  (module-language<%>))
(define-signature drscheme:module-language^ extends drscheme:module-language-cm^
  (add-module-language
   module-language-put-file-mixin))

(define-signature drscheme:module-language-tools-cm^
  (frame-mixin
   frame<%>
   tab-mixin
   tab<%>
   definitions-text-mixin
   definitions-text<%>))
(define-signature drscheme:module-language-tools^ extends drscheme:module-language-tools-cm^
  ())

(define-signature drscheme:get-collection-cm^ ())
(define-signature drscheme:get-collection^ extends drscheme:get-collection-cm^
  (get-file/collection))

(define-signature drscheme:main-cm^ ())
(define-signature drscheme:main^ extends drscheme:main-cm^ ())

(define-signature drscheme:init-cm^
  ())
(define-signature drscheme:init^ extends drscheme:init-cm^
  (original-output-port
   original-error-port
   original-error-display-handler
   primitive-eval
   primitive-load
   error-display-handler-message-box-title
   system-logger
   system-custodian
   system-eventspace
   system-namespace
   first-dir))

(define-signature drscheme:language-configuration-cm^
  ())
(define-signature drscheme:language-configuration^ extends drscheme:language-configuration-cm^
  (add-language
   get-languages
   (struct language-settings (language settings))
   get-settings-preferences-symbol
   language-dialog
   fill-language-dialog))

(define-signature drscheme:language-configuration/internal^ extends drscheme:language-configuration^
  (add-info-specified-languages
   get-default-language-settings
   settings-preferences-symbol
   get-all-scheme-manual-keywords
   get-all-manual-keywords
   add-built-in-languages
   not-a-language-language<%>))

(define-signature drscheme:tools-cm^
  ())
(define-signature drscheme:tools^ extends drscheme:tools-cm^
  ((struct successful-tool (spec bitmap name url))
   get-successful-tools
   only-in-phase
   load/invoke-all-tools
   add-prefs-panel))

(define-signature drscheme:get/extend-cm^
  ())
(define-signature drscheme:get/extend^ extends drscheme:get/extend-cm^
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

(define-signature drscheme:unit-cm^
  (tab%
   tab<%>
   frame% 
   frame<%>
   definitions-canvas%
   get-definitions-text%
   definitions-text<%>
   interactions-canvas%))
(define-signature drscheme:unit^ extends drscheme:unit-cm^
  (open-drscheme-window
   find-symbol
   get-program-editor-mixin
   add-to-program-editor-mixin
   forget-saved-bug-report
   record-saved-bug-report
   (struct teachpack-callbacks (get-names remove add))))

(define-signature drscheme:frame-cm^
  (<%>
   mixin
   basics-mixin
   basics<%>))
(define-signature drscheme:frame^ extends drscheme:frame-cm^
  (create-root-menubar
   add-keybindings-item
   planet-spec?))

(define-signature drscheme:program-cm^
  (frame%))
(define-signature drscheme:program^ extends drscheme:program-cm^
  ())

(define-signature drscheme:eval-cm^
  ())
(define-signature drscheme:eval^ extends drscheme:eval-cm^
  (expand-program
   expand-program/multiple
   traverse-program/multiple
   build-user-eventspace/custodian
   set-basic-parameters
   get-snip-classes))

(define-signature drscheme:text-cm^
  (text<%>
   text%))
(define-signature drscheme:text^ extends drscheme:text-cm^
  ())

(define-signature drscheme:setup-cm^
  ())
(define-signature drscheme:setup^ extends drscheme:setup-cm^ 
  (do-setup))

(define-signature drscheme:rep-cm^
  (drs-bindings-keymap-mixin
   text%
   text<%>
   context<%>))
(define-signature drscheme:rep^ extends drscheme:rep-cm^
  (current-rep
   current-language-settings
   current-value-port
   get-drs-bindings-keymap
   error-delta
   get-welcome-delta 
   get-dark-green-delta
   drs-autocomplete-mixin))

(define-signature drscheme:app-cm^
  ())
(define-signature drscheme:app^ extends drscheme:app-cm^
  (about-drscheme
   add-language-items-to-help-menu
   add-important-urls-to-help-menu
   switch-language-to))

(define-signature drscheme:draw-arrow-cm^
  ())
(define-signature drscheme:draw-arrow^ extends drscheme:draw-arrow-cm^
  (draw-arrow))

(define-signature drscheme:help-desk-cm^
  ())
(define-signature drscheme:help-desk^ extends drscheme:help-desk-cm^
  (help-desk
   goto-plt-license
   get-docs))

(define-signature drscheme:language-cm^
  (language<%>
   module-based-language<%>
   simple-module-based-language<%>
   simple-module-based-language%
   simple-module-based-language->module-based-language-mixin
   module-based-language->language-mixin))
(define-signature drscheme:language^ extends drscheme:language-cm^
  (get-default-mixin
   extend-language-interface
   get-language-extensions
   
   create-module-based-launcher
   create-module-based-stand-alone-executable
   create-module-based-distribution
   
   create-distribution-for-executable
   
   create-executable-gui
   put-executable
   
   ;(struct loc (source position line column span))
   
   (struct text/pos (text start end))
   (struct simple-settings (case-sensitive 
                            printing-style 
                            fraction-style
                            show-sharing
                            insert-newlines
                            annotations))
   simple-settings->vector
   
   simple-module-based-language-config-panel
   simple-module-based-language-convert-value
   setup-printing-parameters
   
   add-snip-value
   setup-setup-values
   
   register-capability
   capability-registered?
   get-capability-default
   get-capability-contract))

(define-signature drscheme:multi-file-search-cm^
  ())
(define-signature drscheme:multi-file-search^ extends drscheme:multi-file-search-cm^
  (multi-file-search))

(define-signature drscheme:module-overview-cm^
  ())
(define-signature drscheme:module-overview^ extends drscheme:module-overview-cm^
  (module-overview
   make-module-overview-pasteboard
   fill-pasteboard))

(define-signature drscheme:tracing-cm^
  (tab-mixin
   frame-mixin))
(define-signature drscheme:tracing^ extends drscheme:tracing-cm^
  (annotate))

(define-signature drscheme:tool-exports-cm^
  ())
(define-signature drscheme:tool-exports^ extends drscheme:tool-exports-cm^
  (phase1 
   phase2))

(define-signature drscheme:tool-cm^
  ((open (prefix drscheme:debug: drscheme:debug-cm^))
   (open (prefix drscheme:unit: drscheme:unit-cm^))
   (open (prefix drscheme:rep: drscheme:rep-cm^))
   (open (prefix drscheme:frame: drscheme:frame-cm^))
   (open (prefix drscheme:get/extend: drscheme:get/extend-cm^))
   (open (prefix drscheme:language-configuration: drscheme:language-configuration-cm^))
   (open (prefix drscheme:language: drscheme:language-cm^))
   (open (prefix drscheme:help-desk: drscheme:help-desk-cm^))
   (open (prefix drscheme:eval: drscheme:eval-cm^))
   (open (prefix drscheme:modes: drscheme:modes-cm^))
   (open (prefix drscheme:tracing: drscheme:tracing-cm^))
   (open (prefix drscheme:module-language: drscheme:module-language-cm^))))

(define-signature drscheme:tool^ 
  ((open (prefix drscheme:debug: drscheme:debug^))
   (open (prefix drscheme:unit: drscheme:unit^))
   (open (prefix drscheme:rep: drscheme:rep^))
   (open (prefix drscheme:frame: drscheme:frame^))
   (open (prefix drscheme:get/extend: drscheme:get/extend^))
   (open (prefix drscheme:language-configuration: drscheme:language-configuration^))
   (open (prefix drscheme:language: drscheme:language^))
   (open (prefix drscheme:help-desk: drscheme:help-desk^))
   (open (prefix drscheme:eval: drscheme:eval^))
   (open (prefix drscheme:modes: drscheme:modes^))
   (open (prefix drscheme:tracing: drscheme:tracing^))
   (open (prefix drscheme:module-language: drscheme:module-language^))))
