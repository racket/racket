#lang racket/base
(require racket/unit)

(provide drracket:eval^
         drracket:debug^
         drracket:module-language^
         drracket:module-language/int^
         drracket:module-language-tools^
         drracket:module-language-tools/int^
         drracket:get-collection^
         drracket:main^
         drracket:init^
         drracket:language-configuration^
         drracket:language-configuration/internal^
         drracket:tools^
         drracket:tools-drs^
         drracket:get/extend^
         drracket:unit^
         drracket:unit/int^
         drracket:frame^
         drracket:frame/int^
         drracket:program^
         drracket:text^
         drracket:rep^
         drracket:rep/int^
         drracket:app^
         drracket:draw-arrow^
         drracket:help-desk^
         drracket:language^
         drracket:language/int^
         drracket:multi-file-search^
         drracket:module-overview^
         drracket:font^
         drracket:modes^
         drracket:modes/int^
         drracket:tracing^
         drracket:tool-exports^
         drracket:tool^
         drracket:tool-cm^
         drscheme:tool^
         drscheme:tool-cm^
         drracket:interface^)

(define-signature drracket:modes-cm^
  ())
(define-signature drracket:modes^ extends drracket:modes-cm^
  (add-mode
   get-modes
   (struct mode (name
                 surrogate
                 repl-submit
                 matches-language
                 intended-to-edit-programs?)
     #:omit-constructor)))
(define-signature drracket:modes/int^ extends drracket:modes^
  (add-initial-modes))

(define-signature drracket:font-cm^
  ())
(define-signature drracket:font^ extends drracket:font-cm^
  (setup-preferences))

(define-signature drracket:debug-cm^
  (profile-definitions-text-mixin
   profile-tab-mixin
   profile-unit-frame-mixin
   test-coverage-interactions-text-mixin
   test-coverage-definitions-text-mixin
   test-coverage-tab-mixin))
(define-signature drracket:debug^ extends drracket:debug-cm^
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
   show-backtrace-window/edition-pairs/two
   open-and-highlight-in-file
   
   small-planet-bitmap

   srcloc->edition/pair
   
   test-coverage-on-style-name
   test-coverage-off-style-name
   
   ;show-error-and-highlight
   ;print-bug-to-stderr
   ;display-srclocs-in-error
   ;show-syntax-error-context
   ))

(define-signature drracket:module-language-cm^
  (module-language<%>))
(define-signature drracket:module-language^ extends drracket:module-language-cm^
  (add-module-language
   module-language-put-file-mixin))
(define-signature drracket:module-language/int^ extends drracket:module-language^
  (module-language-online-expand-text-mixin
   module-language-online-expand-frame-mixin
   module-language-online-expand-tab-mixin
   module-language-online-expand-rep-mixin
   module-language-big-defs/ints-interactions-text-mixin
   module-language-big-defs/ints-definitions-text-mixin
   initialize-prefs-panel
   big-defs/ints-label<%>
   
   change-lang-surrogate-mixin
   default-surrogate%
   modes<%>
   modes-mixin))

(define-signature drracket:module-language-tools-cm^
  (frame-mixin
   tab-mixin
   definitions-text-mixin))
(define-signature drracket:module-language-tools^ extends drracket:module-language-tools-cm^
  (add-opt-out-toolbar-button
   add-online-expansion-handler
   add-online-expansion-monitor
   register-online-expansion-pref
   done
   done?
   start
   start?))
(define-signature drracket:module-language-tools/int^ extends drracket:module-language-tools^
  (get-online-expansion-pref-funcs
   (struct online-expansion-handler (mod-path id local-handler monitor?))
   get-online-expansion-handlers
   no-more-online-expansion-handlers))

(define-signature drracket:get-collection-cm^ ())
(define-signature drracket:get-collection^ extends drracket:get-collection-cm^
  (get-file/collection))

(define-signature drracket:main-cm^ ())
(define-signature drracket:main^ extends drracket:main-cm^ ())

(define-signature drracket:init-cm^
  ())
(define-signature drracket:init^ extends drracket:init-cm^
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
   system-security-guard
   first-dir
   get-last-N-errors))

(define-signature drracket:language-configuration-cm^
  ())
(define-signature drracket:language-configuration^ extends drracket:language-configuration-cm^
  (add-language
   get-languages
   (struct language-settings (language settings))
   make-language-settings
   get-settings-preferences-symbol
   language-dialog
   fill-language-dialog))

(define-signature drracket:language-configuration/internal^ extends drracket:language-configuration^
  (add-info-specified-languages
   get-default-language-settings
   settings-preferences-symbol
   get-all-scheme-manual-keywords
   get-all-manual-keywords
   add-built-in-languages
   not-a-language-language<%>
   language-allows-executable-creation?))

(define-signature drracket:tools-cm^
  ())
(define-signature drracket:tools^ extends drracket:tools-cm^
  ((struct successful-tool (spec bitmap name url))
   make-successful-tool
   get-successful-tools
   only-in-phase
   load/invoke-all-tools
   add-prefs-panel))

(define-signature drracket:tools-drs-cm^
  ())
(define-signature drracket:tools-drs^ extends drracket:tools-drs-cm^
  (invoke-drs-tool))

(define-signature drracket:get/extend-cm^
  ())
(define-signature drracket:get/extend^ extends drracket:get/extend-cm^
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
   get-unit-frame
   
   allow-re-extension!
   disallow-re-extension!))

(define-signature drracket:unit-cm^
  (tab%
   frame% 
   definitions-canvas%
   get-definitions-text%
   interactions-canvas%))
(define-signature drracket:unit^ extends drracket:unit-cm^
  (open-drscheme-window
   find-symbol
   get-program-editor-mixin
   add-to-program-editor-mixin
   (struct teachpack-callbacks (get-names add remove remove-all))
   make-teachpack-callbacks
   add-search-help-desk-menu-item))
(define-signature drracket:unit/int^ extends drracket:unit^
  (forget-saved-bug-report
   record-saved-bug-report))

(define-signature drracket:frame-cm^
  (mixin
   basics-mixin))
(define-signature drracket:frame^ extends drracket:frame-cm^
  ())
(define-signature drracket:frame/int^ extends drracket:frame^
  (create-root-menubar
   add-keybindings-item
   planet-spec?))

(define-signature drracket:program-cm^
  (frame%))
(define-signature drracket:program^ extends drracket:program-cm^
  ())

(define-signature drracket:eval-cm^
  ())
(define-signature drracket:eval^ extends drracket:eval-cm^
  (expand-program
   expand-program/multiple
   traverse-program/multiple
   build-user-eventspace/custodian
   set-basic-parameters
   get-snip-classes))

(define-signature drracket:text-cm^
  (text<%>
   text%))
(define-signature drracket:text^ extends drracket:text-cm^
  ())

(define-signature drracket:setup-cm^
  ())
(define-signature drracket:setup^ extends drracket:setup-cm^ 
  (do-setup))

(define-signature drracket:rep-cm^
  (drs-bindings-keymap-mixin
   text%
   text<%>))
(define-signature drracket:rep^ extends drracket:rep-cm^
  (current-rep
   current-language-settings
   current-value-port
   after-expression
   get-drs-bindings-keymap
   get-error-delta
   get-welcome-delta 
   get-dark-green-delta))
(define-signature drracket:rep/int^ extends drracket:rep^
  (drs-autocomplete-mixin))

(define-signature drracket:app-cm^
  ())
(define-signature drracket:app^ extends drracket:app-cm^
  (about-drscheme
   add-language-items-to-help-menu
   add-important-urls-to-help-menu
   switch-language-to))

(define-signature drracket:draw-arrow-cm^
  ())
(define-signature drracket:draw-arrow^ extends drracket:draw-arrow-cm^
  (draw-arrow))

(define-signature drracket:help-desk-cm^
  ())
(define-signature drracket:help-desk^ extends drracket:help-desk-cm^
  (help-desk
   goto-plt-license))

(define-signature drracket:language-cm^
  (language<%>
   module-based-language<%>
   simple-module-based-language<%>
   simple-module-based-language%
   simple-module-based-language->module-based-language-mixin
   module-based-language->language-mixin))
(define-signature drracket:language^ extends drracket:language-cm^
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
   make-text/pos
   (struct text/pos (text start end))
   make-simple-settings
   (struct simple-settings (case-sensitive 
                            printing-style 
                            fraction-style
                            show-sharing
                            insert-newlines
                            annotations))
   simple-settings->vector
   
   simple-module-based-language-convert-value
   setup-printing-parameters
   make-setup-printing-parameters
   
   add-snip-value
   
   register-capability
   capability-registered?
   get-capability-default
   get-capability-contract))
(define-signature drracket:language/int^ extends drracket:language^
  (simple-module-based-language-config-panel
   setup-setup-values))

(define-signature drracket:multi-file-search-cm^
  ())
(define-signature drracket:multi-file-search^ extends drracket:multi-file-search-cm^
  (multi-file-search
   search-type-params
   search-types))

(define-signature drracket:module-overview-cm^
  ())
(define-signature drracket:module-overview^ extends drracket:module-overview-cm^
  (module-overview
   module-overview/file
   make-module-overview-pasteboard
   fill-pasteboard))

(define-signature drracket:tracing-cm^
  (tab-mixin
   frame-mixin))
(define-signature drracket:tracing^ extends drracket:tracing-cm^
  (annotate))

(define-signature drracket:interface^
  (frame:basics<%>
   frame:<%>
   unit:frame<%>
   unit:definitions-text<%>
   unit:tab<%>
   rep:context<%>
   
   module-language-tools:definitions-text<%>
   module-language-tools:tab<%>
   module-language-tools:frame<%>))

(define-signature drracket:tool-exports-cm^
  ())
(define-signature drracket:tool-exports^ extends drracket:tool-exports-cm^
  (phase1 
   phase2))

(define-signature no-prefix:tool-cm^
  ((open (prefix debug: drracket:debug-cm^))
   (open (prefix unit: drracket:unit-cm^))
   (open (prefix rep: drracket:rep-cm^))
   (open (prefix frame: drracket:frame-cm^))
   (open (prefix get/extend: drracket:get/extend-cm^))
   (open (prefix language-configuration: drracket:language-configuration-cm^))
   (open (prefix language: drracket:language-cm^))
   (open (prefix help-desk: drracket:help-desk-cm^))
   (open (prefix eval: drracket:eval-cm^))
   (open (prefix font: drracket:font-cm^))
   (open (prefix modes: drracket:modes-cm^))
   (open (prefix tracing: drracket:tracing-cm^))
   (open (prefix module-language: drracket:module-language-cm^))
   (open (prefix module-language-tools: drracket:module-language-tools-cm^))
   (open drracket:interface^)))

(define-signature drracket:tool-cm^
  ((open (prefix drracket: no-prefix:tool-cm^))))
(define-signature drscheme:tool-cm^
  ((open (prefix drscheme: no-prefix:tool-cm^))))

(define-signature no-prefix:tool^ 
  ((open (prefix debug: drracket:debug^))
   (open (prefix unit: drracket:unit^))
   (open (prefix rep: drracket:rep^))
   (open (prefix frame: drracket:frame^))
   (open (prefix get/extend: drracket:get/extend^))
   (open (prefix language-configuration: drracket:language-configuration^))
   (open (prefix language: drracket:language^))
   (open (prefix help-desk: drracket:help-desk^))
   (open (prefix eval: drracket:eval^))
   (open (prefix modes: drracket:modes^))
   (open (prefix tracing: drracket:tracing^))
   (open (prefix module-language: drracket:module-language^))
   (open (prefix module-language-tools: drracket:module-language-tools^))
   (open drracket:interface^)))

(define-signature drracket:tool^
  ((open (prefix drracket: no-prefix:tool^))))
(define-signature drscheme:tool^
  ((open (prefix drscheme: no-prefix:tool^))))
