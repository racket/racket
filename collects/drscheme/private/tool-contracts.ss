
(module tool-contracts "tool-contract-language.ss"
  
  ;                           
  ;                           
  ;                           
  ;                        ;  
  ;                        ;  
  ;                        ;  
  ;    ;;;  ;     ; ;;;    ;  
  ;   ;   ;  ;   ; ;   ;   ;  
  ;  ;    ;  ;   ;     ;   ;  
  ;  ;;;;;;   ; ;   ;;;;   ;  
  ;  ;        ; ;  ;   ;   ;  
  ;   ;        ;   ;   ;   ;  
  ;    ;;;;    ;    ;;;;;  ;  
  ;                           
  ;                           
  ;                           
  
  
  (drscheme:eval:set-basic-parameters
   ((listof (is-a?/c snip-class%)) . -> . void?)
   (snipclasses)
   "sets the parameters that are shared between the repl's"
   "initialization and \\iscmprocedure{drscheme:eval:build-user-eventspace/custodian}"
   ""
   "Specifically, it sets these parameters:"
   "\\begin{itemize}"
   "\\item \\rawscm{current-namespace} has been set to a newly"
   "  created empty namespace. This namespace has the following modules "
   "  copied (with \\MzLink{mz:namespace-utilities}{\\rawscm{namespace-attach-module}})"
   "  from DrScheme's original namespace:"
   "  \\begin{itemize}"
   "  \\item \\Symbol{mzscheme}"
   "  \\item \\scmc{'(lib \"mred.ss\" \"mred\")}"
   "  \\end{itemize}"
   ""
   "\\item"
   "  \\MzLink{mz:p:read-curly-brace-as-paren}{\\rawscm{read-curly-brace-as-paren}}"
   "  is \\scmc{\\#t},"
   "\\item"
   "  \\MzLink{mz:p:read-square-bracket-as-paren}{\\rawscm{read-square-bracket-as-paren}}"
   "  is \\scmc{\\#t},"
   "\\item "
   "  \\MzLink{mz:p:error-print-width}{\\rawscm{error-print-width}} is set to 250."
   "\\item"
   "@flink current-ps-setup"
   "is set to a newly created"
   "@link ps-setup"
   "object."
   "\\item The \\MzLink{mz:p:exit-handler}{\\rawscm{exit-handler}} is set to"
   "a parameter that kills the user's custodian."
   "\\item The snip-class-list, returned by"
   "@flink get-the-snip-class-list"
   "is initialized with all of the snipclasses in DrScheme's eventspace's snip-class-list."
   ""
   "\\end{itemize}")
  
  (drscheme:eval:get-snip-classes
   (-> (listof (is-a?/c snip-class%)))
   ()
   "Returns a list of all of the snipclasses in the current eventspace")
  
  (drscheme:eval:expand-program
   ((or/c port? drscheme:language:text/pos?)
    drscheme:language-configuration:language-settings?
    boolean?
    (-> void?)
    (-> void?)
    ((or/c eof-object? syntax? (cons/c string? any/c))
     (-> any)
     . -> .
     any)
    . -> .
    void?)
   (input language-settings eval-compile-time-part? init kill-termination iter)
   
   "Use this function to expand the contents of the definitions"
   "window for use with external program processing tools."
   ""
   "This function uses"
   "@flink drscheme:eval:build-user-eventspace/custodian"
   "to build the user's environment."
   "The arguments \\var{language-settings}, \\var{init}, and"
   "\\var{kill-termination} are passed to"
   "@flink drscheme:eval:build-user-eventspace/custodian %"
   "."
   ""
   "The \\var{input} argument specifies the source of the program."
   ""
   "The \\var{eval-compile-time-part?} argument indicates if"
   "\\Mzhyperref{\\rawscm{expand}}{mz:expansion}"
   "is called or if"
   "\\scheme|expand-top-level-with-compile-time-evals|"
   "is called when the program is expanded."
   "Roughly speaking, if your tool will evaluate each expression"
   "itself by calling"
   "\\Mzhyperref{\\rawscm{eval}}{mz:evalload}"
   "then pass \\scheme{#f}. Otherwise, if your tool"
   "just processes the expanded program, be sure to pass"
   "\\scheme{#t}."
   ""
   "This function calls"
   "@ilink drscheme:language:language front-end/complete-program"
   "to expand the program."
   ""
   "The first argument to \\var{iter} is the expanded program"
   "(represented as syntax) or eof."
   "The \\var{iter} argument is called for each expression in the"
   "expanded program and once more with eof, unless an error is"
   "raised during expansion."
   "It is called from the user's thread."
   "If an exception is raised during expansion of the"
   "user's program, \\var{iter} is not called."
   "Consider setting the exception-handler during \\var{init} to"
   "handle this situation."
   ""
   "The second argument to \\var{iter} is a thunk that"
   "continues expanding the rest of the contents of the"
   "definitions window. If the first argument to \\var{iter} was"
   "eof, this argument is just the primitive"
   "\\rawscm{void}."
   ""
   "See also"
   "@flink drscheme:eval:expand-program/multiple %"
   ".")
  
  (drscheme:eval:traverse-program/multiple
   (drscheme:language-configuration:language-settings?
    (-> void?)
    (-> void?)
    . -> .
    ((or/c port? drscheme:language:text/pos?)
     ((or/c eof-object? syntax? (cons/c string? any/c))
      (-> any)
      . -> .
      any)
     boolean?
     . -> .
     void?))
   (language-settings init kill-termination)
   
   "This function is similar to"
   "@flink drscheme:eval:expand-program/multiple"
   "The only difference is that it does not"
   "expand the program in the editor; instead"
   "the processing function can decide how to"
   "expand the program.")
  
  (drscheme:eval:expand-program/multiple
   (drscheme:language-configuration:language-settings?
    boolean?
    (-> void?)
    (-> void?)
    . -> .
    ((or/c port? drscheme:language:text/pos?)
     ((or/c eof-object? syntax? (cons/c string? any/c))
      (-> any)
      . -> .
      any)
     boolean?
     . -> .
     void?))
   (language-settings eval-compile-time-part? init kill-termination)
   
   "This function is just like"
   "@flink drscheme:eval:expand-program"
   "except that it is curried and the second application"
   "can be used multiple times."
   "Use this function if you want to initialize the user's"
   "thread (and namespace, etc) once but have program text"
   "that comes from multiple sources."
   ""
   "The extra boolean argument to the result function"
   "determines if"
   "@ilink drscheme:language:language front-end/complete-program"
   "or"
   "@ilink drscheme:language:language front-end/interaction"
   "is called.")
  
  (drscheme:eval:build-user-eventspace/custodian
   ((drscheme:language-configuration:language-settings?
     (-> void?)
     (-> void?))
    . ->* .
    (eventspace? custodian?))
   (language-settings init kill-termination)
   
   "This function creates a custodian and an eventspace (on the"
   "new custodian) to expand the user's program. It does not"
   "kill this custodian, but it can safely be shutdown (with"
   "\\MzLink{mz:custodians}{custodian-shutdown-all}) after the"
   "expansion is finished."
   ""
   "It initializes the"
   "user's eventspace's main thread with several parameters:"
   "\\begin{itemize}"
   "\\item \\rawscm{current-custodian} is set to a new custodian."
   "\\item"
   "In addition, it calls"
   "@flink drscheme:eval:set-basic-parameters %"
   "."
   "\\end{itemize}"
   ""
   "The \\var{language-settings} argument is the current"
   "language and its settings. See"
   "@flink drscheme:language-configuration:make-language-settings"
   "for details on that structure."
   ""
   "If the program is associated with a DrScheme"
   "frame, get the frame's language settings from the"
   "@ilink drscheme:unit:definitions-text get-next-settings"
   "method of "
   "@ilink drscheme:unit:definitions-text %"
   ".  Also, the most recently chosen language in"
   "the language dialog is saved via the framework's"
   "preferences. Apply"
   "@flink preferences:get"
   "to"
   "@flink drscheme:language-configuration:get-settings-preferences-symbol"
   "for that \\var{language-settings}."
   ""
   "The \\var{init} argument is called after the user's parameters"
   "are all set, but before the program is run. It is called on"
   "the user's thread. The"
   "\\MzLink{mz:p:current-directory}{current-directory} and"
   "\\MzLink{mz:p:current-load-relative-directory}{current-load-relative-directory}"
   "parameters are not set, so if there are appropriate directories,"
   "the \\var{init} argument is a good place to set them."
   ""
   "The \\var{kill-termination} argument is called when the main thread of"
   "the eventspace terminates, no matter if the custodian was"
   "shutdown, or the thread was killed. This procedure is also"
   "called when the thread terminates normally. This procedure is"
   "called from a new, dedicated thread ({\\it i. e.}, not the thread"
   "created to do the expansion, nor the thread that"
   "\\rawscm{drscheme:eval:build-user-eventspace/custodian} was called from.)")
  
  
  
  ;                                         
  ;                                         
  ;                                         
  ;       ;          ;                      
  ;       ;          ;                      
  ;       ;          ;                      
  ;    ;; ;    ;;;   ; ;;    ;   ;    ;; ;  
  ;   ;  ;;   ;   ;  ;;  ;   ;   ;   ;  ;;  
  ;  ;    ;  ;    ;  ;    ;  ;   ;  ;    ;  
  ;  ;    ;  ;;;;;;  ;    ;  ;   ;  ;    ;  
  ;  ;    ;  ;       ;    ;  ;   ;  ;    ;  
  ;   ;  ;;   ;      ;;  ;   ;  ;;   ;  ;;  
  ;    ;; ;    ;;;;  ; ;;     ;; ;    ;; ;  
  ;                                      ;  
  ;                                 ;    ;  
  ;                                  ;;;;   
  
  (drscheme:debug:show-error-and-highlight
   (string? 
    (or/c any/c exn?) 
    (-> (listof srcloc?) (or/c false/c (listof (list/c (is-a?/c text%) number? number?))) any)
    . -> . 
    any)
   (msg exn highlight-errors)
   "The first two arguments are the same as the arguments to the error-display-handler. "
   "This function prints the error message to the current-error-port, like the default error-display-handler "
   "and also calls \\var{highlight-errors} to do error highlighting. It is be passed the stack trace "
   "for the error message."
   ""
   "This function should be called on the same thread/eventspace where the error happened.")
  
  (drscheme:debug:make-debug-error-display-handler
   ((string? (or/c any/c exn?) . -> . any)
    . -> .
    (string? (or/c any/c exn?) . -> . any))
   
   (oedh)
   
   "This function implements an error-display-handler in terms"
   "of another error-display-handler."
   ""
   "This function is designed to work in conjunction with"
   "@flink drscheme:debug:make-debug-eval-handler %"
   "."
   ""
   "See also MzScheme's"
   "MzLink{mz:p:error-display-handler}{error-display-handler}"
   "parameter."
   ""
   "If the current-error-port is the definitions window in"
   "drscheme, this error handler inserts some debugging"
   "annotations, calls \\var{oedh}, and then highlights the"
   "source location of the runtime error.")
  
  (drscheme:debug:make-debug-eval-handler
   ((any/c . -> . any/c)
    . -> .
    (any/c . -> . any/c))
   
   (odeh)
   
   "This function implements an eval-handler in terms of another"
   "eval-handler."
   ""
   "This function is designed to work in conjunction with"
   "@flink drscheme:debug:make-debug-error-display-handler %"
   "."
   ""
   "See also MzScheme's MzLink{mz:p:eval-handler}{eval-handler}"
   "parameter. "
   ""
   "The resulting eval-handler expands and annotates the input"
   "expression and then passes it to the input eval-handler,"
   "unless the input expression is already compiled, in which"
   "case it just hands it directly to the input eval-handler.")
  
  (drscheme:debug:hide-backtrace-window
   (-> void?)
   ()
   "Hides the backtrace window.")
  
  
  (drscheme:debug:profiling-enabled
   (case-> (boolean? . -> . void?)
           (-> boolean?))
   ((enabled?) ())
   "A parameter that controls if profiling information is recorded."
   ""
   "Defaults to \\scmc{\\#f}."
   ""
   "Only applies if"
   "@flink drscheme:debug:make-debug-eval-handler"
   "has been added to the eval handler.")
  
  (drscheme:debug:add-prefs-panel
   (-> void?)
   ()
   "Adds the profiling preferences panel.")
  
  (drscheme:debug:open-and-highlight-in-file
   (srcloc? . -> . void?)
   (debug-info)
   "This function opens a DrScheme to display"
   "\\var{debug-info}. The first element in"
   "the cons indicates where the file is"
   "and the two number indicate a range of"
   "text to show."
   ""
   "See also"
   "@flink drscheme:debug:get-cm-key %"
   ".")
  
  (drscheme:debug:show-backtrace-window
   (string?
    (listof any/c)
    . -> .
    void?)
   (error-message dis)
   "Shows the backtrace window you get when clicking on the bug in"
   "DrScheme's REPL."
   ""
   "The \\var{error-message} argument is the text of the error,"
   "\\var{dis} is the debug information, extracted from the"
   "continuation mark in the exception record, using"
   "@flink drscheme:debug:get-cm-key %"
   ".")
  
  (drscheme:debug:get-cm-key
   (-> any)
   ()
   "Returns a key used with \\scheme|contination-mark-set->list|."
   "The contination mark set attached to an exception record"
   "for the user's program may use this mark. If it does,"
   "each mark on the continuation is the same type as"
   "the input to"
   "@flink drscheme:debug:open-and-highlight-in-file %"
   ".")
  
  ;                           
  ;                           
  ;                           
  ;                   ;       
  ;                           
  ;                       ;   
  ;   ;   ;   ; ;;    ;  ;;;; 
  ;   ;   ;   ;;  ;   ;   ;   
  ;   ;   ;   ;   ;   ;   ;   
  ;   ;   ;   ;   ;   ;   ;   
  ;   ;   ;   ;   ;   ;   ;   
  ;   ;  ;;   ;   ;   ;   ;   
  ;    ;; ;   ;   ;   ;    ;; 
  ;                           
  ;                           
  ;                           
  
  
  (drscheme:unit:get-program-editor-mixin
   (-> ((subclass?/c text%) . -> . (subclass?/c text%)))
   ()
   "Returns a mixin that must be mixed in to any"
   "\\iscmclass{text} object that might contain"
   "program text (and thus can be in the source"
   "field of some syntax object)."
   ""
   "See also"
   "@flink drscheme:unit:add-to-program-editor-mixin %"
   ".")
  
  (drscheme:unit:add-to-program-editor-mixin
   (((subclass?/c text%) . -> . (subclass?/c text%)) . -> . void?)
   (mixin)
   "\\phase{1}"
   ""
   "Adds \\var{mixin} to the result of"
   "@flink drscheme:unit:get-program-editor-mixin %"
   ".")
  
  (drscheme:unit:open-drscheme-window
   (case->
    (-> (is-a?/c drscheme:unit:frame%))
    ((or/c string? false/c) . -> . (is-a?/c drscheme:unit:frame%)))
   (() (filename))
   
   "Opens a drscheme frame that displays \\var{filename},"
   "or nothing if \\var{filename} is \\scmc{\\#f} or not supplied.")
  
  
  
  ;                                            
  ;                                            
  ;                                            
  ;                           ;                
  ;                           ;                
  ;                           ;                
  ;   ; ;;  ;;     ;;;     ;; ;    ;;;    ;;;  
  ;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;  ;     
  ;   ;   ;   ;  ;     ; ;    ;  ;    ;  ;;    
  ;   ;   ;   ;  ;     ; ;    ;  ;;;;;;   ;;   
  ;   ;   ;   ;  ;     ; ;    ;  ;          ;  
  ;   ;   ;   ;   ;   ;   ;  ;;   ;         ;  
  ;   ;   ;   ;    ;;;     ;; ;    ;;;;  ;;;   
  ;                                            
  ;                                            
  ;                                            
  
  
  (drscheme:modes:add-mode
   (string?
    (or/c false/c (is-a?/c mode:surrogate-text<%>))
    ((is-a?/c drscheme:rep:text%) number? . -> . boolean?)
    ((or/c false/c (listof string?)) . -> . boolean?)
    . -> .
    drscheme:modes:mode?)
   (name surrogate repl-submit matches-language)
   "Adds a mode to DrScheme. Returns a mode value"
   "that identifies the mode."
   ""
   "The first argument, \\var{name}, is the name"
   "of the mode, used in DrScheme's GUI to allow"
   "the user to select this mode."
   ""
   "The \\var{surrogate} argument is set to the"
   "definitions text and the interactions text"
   "(via the"
   "@ilink mode:host-text set-surrogate"
   "method) whenever this mode is enabled."
   ""
   "The \\var{repl-submit} procedure is called"
   "whenever the user types a return in the interactions"
   "window. It is passed the interactions editor"
   "and the position where the last prompt occurs."
   "If it "
   "returns \\scheme|#t|, the text after the last"
   "prompt is treated as a program fragment and"
   "evaluated, according to the language settings."
   "If it returns \\scheme|#f|, the text is"
   "assumed to be an incomplete program fragment, and"
   "the keystroke is not treated specially."
   ""
   "The \\var{matches-language} predicate is called whenever"
   "the language changes. If it returns \\scheme|#t|"
   "this mode is installed. It is passed the list of strings"
   "that correspond to the names of the language in the"
   "language dialog."
   ""
   "Modes are tested in the opposite order that they are"
   "added. That is, the last mode to be added gets tested"
   "first when the filename changes or when the language"
   "changes."
   ""
   "See also"
   "@flink drscheme:modes:get-modes %"
   ".")
  
  (drscheme:modes:mode?
   (any/c . -> . boolean?)
   (val)
   "Determines if \\var{val} is a mode.")
  
  (drscheme:modes:get-modes
   (-> (listof drscheme:modes:mode?))
   ()
   "Returns all of the modes currently added to DrScheme."
   ""
   "See also"
   "@flink drscheme:modes:add-mode %"
   ".")
  
  (drscheme:modes:mode-name
   (drscheme:modes:mode? . -> . string?)
   (mode)
   "Extracts the name of the mode."
   ""
   "See also"
   "@flink drscheme:modes:add-mode %"
   ".")
  
  (drscheme:modes:mode-surrogate
   (drscheme:modes:mode? . -> . (or/c false/c (is-a?/c mode:surrogate-text<%>)))
   (mode)
   "Extracts the surrogate of the mode."
   ""
   "See also"
   "@flink drscheme:modes:add-mode %"
   ".")
  
  (drscheme:modes:mode-repl-submit
   (drscheme:modes:mode? . -> . any)
   (mode)
   "Extracts the repl submission predicate of the mode."
   ""
   "See also"
   "@flink drscheme:modes:add-mode %"
   ".")
  
  (drscheme:modes:mode-matches-language
   (drscheme:modes:mode? . -> . ((or/c false/c (listof string?)) . -> . boolean?))
   (mode)
   "Extracts the language matching predicate of the mode."
   ""
   "See also"
   "@flink drscheme:modes:add-mode %"
   ".")
  
  
  ;                      
  ;                      
  ;                      
  ;                      
  ;                      
  ;                      
  ;   ; ;   ;;;   ; ;;   
  ;   ;;   ;   ;  ;;  ;  
  ;   ;   ;    ;  ;    ; 
  ;   ;   ;;;;;;  ;    ; 
  ;   ;   ;       ;    ; 
  ;   ;    ;      ;;  ;  
  ;   ;     ;;;;  ; ;;   
  ;               ;      
  ;               ;      
  ;               ;      
  
  
  (drscheme:rep:get-welcome-delta 
   (-> (is-a?/c style-delta%))
   ()
   "Returns a style delta that matches the style and color of the "
   "phrase ``Welcome to'' in the beginning of the interactions window.")
  
  (drscheme:rep:get-dark-green-delta
   (-> (is-a?/c style-delta%))
   ()
   "Returns a style delta that matches the style and color of the "
   "name of a language in the interactions window.")
  
  (drscheme:rep:get-drs-bindings-keymap
   (-> (is-a?/c keymap%))
   ()
   "Returns a keymap that binds various DrScheme-specific"
   "keybindings. This keymap is used in the definitions"
   "and interactions window."
   ""
   "Defaultly binds C-x;o to a function that switches"
   "the focus between the definitions and interactions"
   "windows. Also binds f5 to Execute and f1 to Help Desk.")
  
  (drscheme:rep:current-rep
   (-> (or/c false/c (is-a?/c drscheme:rep:text%)))
   ()
   
   "This is a parameter whose value should not be set by tools."
   "It is initialized to the repl that controls this evaluation"
   "in the user's thread."
   ""
   "It only returns \\scheme|#f| if the program not running"
   "in the context of a repl (eg, the test suite window).")
  
  (drscheme:rep:current-value-port
   (-> (or/c false/c port?))
   ()
   "This is a parameter whose value is a port that"
   "prints in the REPL in blue. It is used to print"
   "the values of toplevel expressions in the REPL."
   ""
   "It is only initialized on the user's thread")
  
  
  ;                                                                        
  ;                                                                        
  ;                                                                        
  ;                          ;                                          ;  
  ;                          ;                                          ;  
  ;                  ;      ;                   ;                       ;  
  ;    ;; ;    ;;;  ;;;;    ;     ;;;  ;     ; ;;;;   ;;;   ; ;;     ;; ;  
  ;   ;  ;;   ;   ;  ;      ;    ;   ;  ;   ;   ;    ;   ;  ;;  ;   ;  ;;  
  ;  ;    ;  ;    ;  ;      ;   ;    ;   ; ;    ;   ;    ;  ;   ;  ;    ;  
  ;  ;    ;  ;;;;;;  ;     ;    ;;;;;;    ;     ;   ;;;;;;  ;   ;  ;    ;  
  ;  ;    ;  ;       ;     ;    ;        ; ;    ;   ;       ;   ;  ;    ;  
  ;   ;  ;;   ;      ;     ;     ;      ;   ;   ;    ;      ;   ;   ;  ;;  
  ;    ;; ;    ;;;;   ;;  ;       ;;;; ;     ;   ;;   ;;;;  ;   ;    ;; ;  
  ;       ;               ;                                                
  ;  ;    ;               ;                                                
  ;   ;;;;                                                                 
  
  
  (drscheme:get/extend:extend-tab
   (case->
    ((make-mixin-contract drscheme:unit:tab%) . -> . void?)
    ((make-mixin-contract drscheme:unit:tab%) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This class implements the tabs in drscheme. One is created for each tab"
   "in a frame (each frame always has at least one tab, even if the tab bar is not shown)"
   ""
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#t}.")
  
  (drscheme:get/extend:extend-interactions-text
   (case->
    ((make-mixin-contract drscheme:rep:text<%>) . -> . void?)
    ((make-mixin-contract drscheme:rep:text<%>) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This text is used in the bottom window of drscheme frames."
   ""
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#t}.")
  
  (drscheme:get/extend:get-interactions-text
   (-> (implementation?/c drscheme:rep:text<%>))
   ()
   
   "Once this function is called, "
   "@flink drscheme:get/extend:extend-interactions-text "
   "raises an error, disallowing any more extensions.")
  
  (drscheme:get/extend:extend-definitions-text
   (case->
    ((make-mixin-contract drscheme:unit:definitions-text<%>) . -> . void?)
    ((make-mixin-contract drscheme:unit:definitions-text<%>) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This text is used in the top window of drscheme frames."
   ""
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#f}.")
  
  (drscheme:get/extend:get-definitions-text
   (-> (implementation?/c drscheme:unit:definitions-text<%>))
   ()
   
   "Once this function is called, "
   "@flink drscheme:get/extend:extend-definitions-text "
   "raises an error, disallowing any more extensions.")
  
  (drscheme:get/extend:extend-interactions-canvas
   (case->
    ((make-mixin-contract drscheme:unit:interactions-canvas%) . -> . void?)
    ((make-mixin-contract drscheme:unit:interactions-canvas%) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This canvas is used in the bottom window of drscheme frames."
   ""
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#f}.")
  
  (drscheme:get/extend:get-interactions-canvas
   (-> (subclass?/c drscheme:unit:interactions-canvas%))
   ()
   
   "Once this function is called, "
   "@flink drscheme:get/extend:extend-interactions-canvas"
   "raises an error, disallowing any more extensions.")
  
  (drscheme:get/extend:extend-definitions-canvas
   (case->
    ((make-mixin-contract drscheme:unit:definitions-canvas%) . -> . void?)
    ((make-mixin-contract drscheme:unit:definitions-canvas%) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This canvas is used in the top window of drscheme frames."
   
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#f}.")
  
  (drscheme:get/extend:get-definitions-canvas
   (-> (subclass?/c drscheme:unit:definitions-canvas%))
   ()
   
   "Once this function is called, "
   "@flink drscheme:get/extend:extend-definitions-canvas"
   "raises an error, disallowing any more extensions.")
  
  (drscheme:get/extend:extend-unit-frame
   (case->
    ((make-mixin-contract drscheme:unit:frame%) . -> . void?)
    ((make-mixin-contract drscheme:unit:frame%) boolean? . -> . void?))
   ((mixin) (mixin before?))
   
   "This is the frame that implements the main drscheme window."
   ""
   "The argument, \\var{before}, controls if the mixin is applied before or"
   "after already installed mixins."
   "If unsupplied, this is the same as supplying \\scmc{\\#f}.")
  
  (drscheme:get/extend:get-unit-frame
   (-> (subclass?/c drscheme:unit:frame%))
   ()
   
   "Once this function is called, "
   "@flink drscheme:get/extend:extend-unit-frame"
   "raises an error, disallowing any more extensions.")
  
  
  ;                                                           
  ;                                                           
  ;                                                           
  ;   ;                                                       
  ;   ;                                                       
  ;   ;                                                       
  ;   ;   ;;;    ; ;;     ;; ;   ;   ;   ;;;     ;; ;    ;;;  
  ;   ;  ;   ;   ;;  ;   ;  ;;   ;   ;  ;   ;   ;  ;;   ;   ; 
  ;   ;      ;   ;   ;  ;    ;   ;   ;      ;  ;    ;  ;    ; 
  ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;;;;  ;    ;  ;;;;;; 
  ;   ;  ;   ;   ;   ;  ;    ;   ;   ;  ;   ;  ;    ;  ;      
  ;   ;  ;   ;   ;   ;   ;  ;;   ;  ;;  ;   ;   ;  ;;   ;     
  ;   ;   ;;;;;  ;   ;    ;; ;    ;; ;   ;;;;;   ;; ;    ;;;; 
  ;                          ;                      ;         
  ;                     ;    ;                 ;    ;         
  ;                      ;;;;                   ;;;;          
  ;                                                                                       
  ;                                                                                       
  ;                                                                                       
  ;                           ;;; ;                                    ;                  
  ;                          ;                                                            
  ;                          ;                                    ;                       
  ;    ;;;    ;;;    ; ;;   ;;;;  ;    ;; ;   ;   ;   ; ;  ;;;   ;;;;  ;    ;;;    ; ;;   
  ;   ;   ;  ;   ;   ;;  ;   ;    ;   ;  ;;   ;   ;   ;;  ;   ;   ;    ;   ;   ;   ;;  ;  
  ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;       ;   ;    ;  ;     ;  ;   ;  
  ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;    ;;;;   ;    ;  ;     ;  ;   ;  
  ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;   ;   ;   ;    ;  ;     ;  ;   ;  
  ;   ;   ;  ;   ;   ;   ;   ;    ;   ;  ;;   ;  ;;   ;   ;   ;   ;    ;   ;   ;   ;   ;  
  ;    ;;;    ;;;    ;   ;   ;    ;    ;; ;    ;; ;   ;    ;;;;;   ;;  ;    ;;;    ;   ;  
  ;                                       ;                                               
  ;                                  ;    ;                                               
  ;                                   ;;;;                                                
  
  (drscheme:language-configuration:get-languages
   (-> (listof (is-a?/c drscheme:language:language<%>)))
   ()
   "This can only be called after all of the tools initialization phases have completed."
   ""
   "Returns the list of all of the languages installed in DrScheme.")
  
  (drscheme:language-configuration:add-language
   ((and/c (is-a?/c drscheme:language:language<%>) language-object)
    . -> . void?)
   (language)
   
   "\\phase{2}"
   ""
   "Adds \\var{language} to the languages offerend by DrScheme.")
  
  (drscheme:language-configuration:get-settings-preferences-symbol
   (-> symbol?)
   ()
   "Returns the symbol that is used to store the user's language"
   "settings. Use as an argument to either"
   "@flink preferences:get"
   "or"
   "@flink preferences:set %"
   ".")
  
  (drscheme:language-configuration:make-language-settings
   ((or/c (is-a?/c drscheme:language:language<%>) language-object)
    any/c
    . -> .
    drscheme:language-configuration:language-settings?)
   (language settings)
   
   "This is the constructor for a record consisting of two"
   "elements, a language and its settings. "
   ""
   "The settings is a language-specific record that holds a"
   "value describing a parameterization of the language."
   ""
   "It has two selectors,"
   "@flink drscheme:language-configuration:language-settings-language"
   "and "
   "@flink drscheme:language-configuration:language-settings-settings %"
   ", and a predicate,"
   "@flink drscheme:language-configuration:language-settings?")
  
  (drscheme:language-configuration:language-settings-settings
   (drscheme:language-configuration:language-settings?
    . -> .
    any/c)
   (ls)
   "Extracts the settings field of a language-settings.")
  
  (drscheme:language-configuration:language-settings-language
   (drscheme:language-configuration:language-settings?
    . -> .
    (or/c (is-a?/c drscheme:language:language<%>) language-object))
   (ls)
   
   "Extracts the language field of a language-settings.")
  
  (drscheme:language-configuration:language-settings?
   (any/c . -> . boolean?)
   (val)
   
   "Determines if the argument is a language-settings or not.")
  
  (drscheme:language-configuration:language-dialog
   (opt->
    (boolean? drscheme:language-configuration:language-settings?)
    ((or/c false/c (is-a?/c top-level-window<%>))
     boolean?)
    (or/c false/c drscheme:language-configuration:language-settings?))
   ((show-welcome? language-settings-to-show)
    ((parent #t)
     (manuals? #f)))
   "Opens the language configuration dialog."
   "See also"
   "@flink drscheme:language-configuration:fill-language-dialog %"
   "."
   ""
   "The \\var{show-welcome?} argument determines if"
   "if a ``Welcome to DrScheme'' message and some"
   "natural language buttons are shown."
   ""
   "The \\var{language-settings-to-show} argument"
   "must be some default language settings that the dialog"
   "is initialized to."
   "If unsure of a default, the currently set language"
   "in the user's preferences can be obtained via:"
   "\\begin{schemedisplay}"
   "(preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))"
   "\\end{schemedisplay}"
   ""
   "The \\var{parent} argument is used as the parent"
   "to the dialog."
   ""
   "The \\var{manuals?} argument is passed to"
   "@flink drscheme:language-configuration:fill-language-dialog %"
   "."
   ""
   "The result if \\scheme|#f| when the user cancells the dialog, and"
   "the selected language if they hit ok.")
  
  (drscheme:language-configuration:fill-language-dialog
   (opt->
    ((is-a?/c vertical-panel%)
     (is-a?/c area-container<%>)
     drscheme:language-configuration:language-settings?)
    ((or/c false/c (is-a?/c top-level-window<%>))
     boolean?
     (-> symbol? void?))
    drscheme:language-configuration:language-settings?)
   ((panel button-panel language-setting)
    ((re-center #f)
     (manuals? #f)
     (ok-handler void)))
   "This procedure accepts two parent panels and"
   "fills them with the contents of the language dialog."
   "It is used to include language configuration controls"
   "in some larger context in another dialog."
   ""
   "The \\var{panel} argument is the main panel where the"
   "language controls will be placed."
   "The function adds buttons to the \\var{button-panel}"
   "to revert a language to its default settings and to"
   "show the details of a language."
   ""
   "The \\var{language-setting} is the default"
   "language to show in the dialog."
   ""
   "The \\var{re-center} argument is used when the \\gui{Show Details}"
   "button is clicked. If that argument is a \\iscmintf{top-level-window},"
   "the \\gui{Show Details} callback will recenter the window each time"
   "it is clicked. Otherwise, the argument is not used."
   ""
   "If \\var{manuals?} is \\scheme{#f} the usual language dialog (as seen"
   "in the start up drscheme window and from the Choose Language dialog"
   "created when drscheme is started up) is shown. If it isn't, the dialog"
   "does not have the details and on the right-hand side shows the manual"
   "ordering for the chosen language. This is used in Help Desk."
   ""
   "\\var{ok-handler} is a function that is in charge of interfacing the OK"
   "button. It should accept a symbol message: \\scheme{'enable} and"
   "\\scheme{'disable} to toggle the button, and \\scheme{'execute} to run"
   "the desired operation. (The language selection dialog also uses an"
   "internal \\scheme{'enable-sync} message.)")
  
  (drscheme:language:register-capability
   (->r ([s symbol?]
         [the-contract contract?]
         [default the-contract])
        void?)
   (s the-contract default)
   "Registers a new capability with a default value for each language"
   "and a contract on the values the capability might have."
   ""
   "By default, these capabilities are registered as DrScheme starts up:"
   "\\begin{itemize}"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:check-syntax-button (flat-contract boolean?) #t)|"
   "--- controls the visiblity of the check syntax button"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:language-menu-title (flat-contract string?) (string-constant scheme-menu-name))|"
   " --- controls the name of the menu just to the right of the language menu (defaultly named ``Scheme'')"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:define-popup (or/c (cons/c string? string?) false/c) (cons \"(define\" \"(define ...)\"))|"
   " --- specifies the prefix that the define popup should look for and what label it should have,"
   "or \\scheme|#f| if it should not appear at all."
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-fraction (flat-contract boolean?) #t)|"
   " --- determines if the insert fraction menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-lambda (flat-contract boolean?) #t)|"
   " --- determines if the insert lambda menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-large-letters (flat-contract boolean?) #t)|"
   " --- determines if the insert large letters menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-image (flat-contract boolean?) #t)|"
   " --- determines if the insert image menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-comment-box (flat-contract boolean?) #t)|"
   " --- determines if the insert comment box menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-gui-tool (flat-contract boolean?) #t)|"
   " --- determines if the insert gui menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:slideshow-menu-item (flat-contract boolean?) #t)|"
   " --- determines if the insert pict box menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:insert-text-box (flat-contract boolean?) #t)|"
   " --- determines if the insert text box menu item in the special menu is visible"
   "\\item \\scheme|(drscheme:language:register-capability 'drscheme:special:xml-menus (flat-contract boolean?) #t)|"
   " --- determines if the insert scheme box, insert scheme splice box, and the insert xml box menu item ins the special menu are visible"
   
   "\\end{itemize}")
  (drscheme:language:capability-registered? 
   (-> symbol? boolean?)
   (s)
   "Indicates if"
   "@flink drscheme:language:register-capability"
   "has been called with \\var{s}.")
  (drscheme:language:get-capability-default
   (->d (and/c symbol? drscheme:language:capability-registered?)
        (λ (s) (drscheme:language:get-capability-contract s)))
   (s)
   "Returns the default for a particular capability.")
  
  
  ;                                                           
  ;                                                           
  ;                                                           
  ;   ;                                                       
  ;   ;                                                       
  ;   ;                                                       
  ;   ;   ;;;    ; ;;     ;; ;   ;   ;   ;;;     ;; ;    ;;;  
  ;   ;  ;   ;   ;;  ;   ;  ;;   ;   ;  ;   ;   ;  ;;   ;   ; 
  ;   ;      ;   ;   ;  ;    ;   ;   ;      ;  ;    ;  ;    ; 
  ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;;;;  ;    ;  ;;;;;; 
  ;   ;  ;   ;   ;   ;  ;    ;   ;   ;  ;   ;  ;    ;  ;      
  ;   ;  ;   ;   ;   ;   ;  ;;   ;  ;;  ;   ;   ;  ;;   ;     
  ;   ;   ;;;;;  ;   ;    ;; ;    ;; ;   ;;;;;   ;; ;    ;;;; 
  ;                          ;                      ;         
  ;                     ;    ;                 ;    ;         
  ;                      ;;;;                   ;;;;          
  
  
  (drscheme:language:add-snip-value
   (opt-> ((-> any/c boolean?)
           (-> any/c (is-a?/c snip%)))
          ((-> any/c))
          void?)
   ((test-value convert-value)
    ((setup-thunk void)))
   "Registers a handler to convert values into snips as they are printed in the REPL."
   ""
   "The \\var{test-snip} argument is called to determine if this handler can convert the value "
   "and the \\var{convert-value} argument is called to build a snip. "
   "The (optional) \\var{setup-thunk} is called just after the user's namespace and other "
   "setings are built, but before any of the user's code is evaluated."
   ""
   "All three functions are called on the user's thread and with the user's settings.")
  
  (drscheme:language:extend-language-interface
   (interface?
    ((implementation?/c drscheme:language:language<%>) . ->d . (λ (%) (subclass?/c %)))
    . -> .
    void?)
   (interface default-implementation)
   
   "\\phase{1}"
   ""
   "Each language added passed to"
   "@flink drscheme:language-configuration:add-language"
   "must implement \\var{interface}. "
   ""
   "The \\var{default-implementation} is a mixin"
   "that provides a default implementation of "
   "\\var{interface}. Languages that are unaware of"
   "the specifics of \\var{extension} use"
   "\\var{default-implementation} via"
   "@flink drscheme:language:get-default-mixin %"
   ".")
  
  (drscheme:language:get-default-mixin
   (-> ((implementation?/c drscheme:language:language<%>) . ->d . (λ (%) (subclass?/c %))))
   ()
   
   "\\phase{2}"
   ""
   "The result of this function is the composite of all of the "
   "\\var{default-implementation} arguments passed"
   "to"
   "@flink drscheme:language:extend-language-interface %"
   ".")
  
  (drscheme:language:get-language-extensions
   (-> (listof interface?))
   ()
   "\\phase{2}"
   ""
   "Returns a list of the interfaces passed to"
   "@flink drscheme:language:extend-language-interface %"
   ".")
  
  (drscheme:language:put-executable
   ((is-a?/c top-level-window<%>) 
    path? 
    (or/c boolean? (symbols 'launcher 'standalone 'distribution)) 
    boolean? 
    string? 
    . -> . (or/c false/c path?))
   (parent program-filename mode mred? title)
   "Calls the MrEd primitive"
   "@flink put-file"
   "with arguments appropriate for creating an executable"
   "from the file \\var{program-filename}. "
   ""
   "The arguments \\var{mred?} and \\var{mode} indicates"
   "what type of executable this should be (and the dialog"
   "may be slightly different on some platforms, depending"
   "on these arguments). For historical reasons, \\scmc{\\#f}"
   "is allowed for \\var{mode} as an alias for \\Symbol{launcher}, and"
   "\\scmc{\\#t} is allowed for \\var{mode} as an alias for \\Symbol{stand-alone}."
   ""
   "The \\var{title} argument is used as the title to the primitive"
   "@flink put-file"
   "or"
   "@flink get-directory"
   "primitive.")
  
  (drscheme:language:create-executable-gui
   ((or/c false/c (is-a?/c top-level-window<%>))
    (or/c false/c string?)
    (or/c (λ (x) (eq? x #t)) (symbols 'launcher 'standalone 'distribution))
    (or/c (λ (x) (eq? x #t)) (symbols 'mzscheme 'mred))
    . -> .
    (or/c false/c
          (list/c (symbols 'no-show 'launcher 'stand-alone 'distribution)
                  (symbols 'no-show 'mred 'mzscheme)
                  string?)))
   (parent program-name show-type show-base)
   "Opens a dialog to prompt the user about their choice of executable."
   "If \\var{show-type} is \\scmc{\\#t}, the user is prompted about"
   "a choice of executable: stand-alone,"
   "launcher, or distribution; otherwise, the symbol determines the type."
   "If \\var{show-base}"
   "is \\scmc{\\#t}, the user is prompted about a choice of base"
   "binary: mzscheme or mred; otherwise the symbol determines the base."
   ""
   "The \\var{program-name} argument is used to construct the default"
   "executable name in a platform-specific manner."
   ""
   "The \\var{parent} argument is used for the parent of the dialog."
   ""
   "The result of this function is \\scmc{\\#f} if the user cancel's"
   "the dialog and a list of three items indicating what options"
   "they chose. If either \\var{show-type} or \\var{show-base}"
   "was not \\scmc{\\#t}, the corresponding result will be \\scmc{'no-show},"
   "otherwise it will indicate the user's choice.")
  
  (drscheme:language:create-module-based-stand-alone-executable 
   ((or/c path? string?)
    (or/c path? string?) any/c any/c any/c boolean? boolean?
    . -> .
    void?)
   (program-filename
    executable-filename
    module-language-spec
    transformer-module-language-spec
    init-code
    gui?
    use-copy?)
   
   "This procedure creates a stand-alone executable in the file"
   "\\var{executable-filename} that runs the program"
   "\\var{program-filename}. "
   ""
   "The arguments"
   "\\var{module-language-spec} and"
   "\\var{transformer-module-language-spec} specify the "
   "settings of the initial namespace, both the transformer"
   "portion and the regular portion. "
   ""
   "The \\var{init-code} argument is an s-expression representing"
   "the code for a module. This module is expected to provide"
   "the identifer \\rawscm{init-code}, bound to a procedure of no"
   "arguments. That module is required and the \\var{init-code}"
   "procedure is executed to initialize language-specific"
   "settings before the code in \\var{program-filename} runs."
   ""
   "The \\var{gui?} argument indicates if a MrEd or MzScheme"
   "stand-alone executable is created."
   ""
   "The \\var{use-copy?} argument indicates if the initial"
   "namespace should be populated with"
   "\\rawscm{namespace-require/copy} or"
   "\\rawscm{namespace-require}. ")
  
  (drscheme:language:create-module-based-distribution
   ((or/c path? string?)
    (or/c path? string?) any/c any/c any/c boolean? boolean?
    . -> .
    void?)
   (program-filename
    distribution-filename
    module-language-spec
    transformer-module-language-spec
    init-code
    gui?
    use-copy?)
   
   "Like"
   "@flink drscheme:language:create-module-based-stand-alone-executable   %"
   ", but packages the stand-alone executable into a distribution.")
  
  (drscheme:language:create-distribution-for-executable
   ((or/c path? string?) 
    boolean?
    (-> path? void?)
    . -> .
    void?)
   (distribution-filename
    gui?
    make-executable)
   
   "Creates a distribution where the given \\var{make-executable} procedure"
   " creates the stand-alone executable to be distributed. "
   "The \\var{make-executable} procedure is given the name of the "
   "executable to create. The \\var{gui?} argument is needed in case the"
   "executable's name (which \\rawscm{drscheme:language:create-distribution-for-executable} "
   "must generate) depends on the type of executable. During the distribution-making "
   "process, a progress dialog is shown to the user, and the user can click an "
   "\\OnScreen{Abort} button that sends a break to the current thread.")
  
  (drscheme:language:create-module-based-launcher
   ((or/c path? string?) (or/c path? string?) any/c any/c any/c boolean? boolean?
                         . -> .
                         void?)
   (program-filename
    executable-filename
    module-language-spec
    transformer-module-language-spec
    init-code
    gui?
    use-copy?)
   
   "This procedure is identical to "
   "@flink drscheme:language:create-module-based-stand-alone-executable %"
   ", except that it creates a launcher instead of a"
   "stand-alone executable.")
  
  (drscheme:language:text/pos-text
   (drscheme:language:text/pos? . -> . (is-a?/c text%))
   (text/pos)
   
   "Selects the \\iscmclass{text} from a text/pos.")
  
  (drscheme:language:text/pos-start
   (drscheme:language:text/pos? . -> . number?)
   (text/pos)
   
   "Selects the starting position from a text/pos.")
  
  (drscheme:language:text/pos-end
   (drscheme:language:text/pos? . -> . number?)
   (text/pos)
   
   "Selects the ending position from a text/pos.")
  
  (drscheme:language:text/pos?
   (any/c . -> . boolean?)
   (val)
   
   "Returns \\scmc{\\#t} if \\var{val} is a text/pos, and \\scmc{\\#f}"
   "otherwise.")
  
  (drscheme:language:make-text/pos
   ((is-a?/c text%) number? number?
                    . -> .
                    drscheme:language:text/pos?)
   (text start end)
   
   "Constructs a text/pos.")
  
  (drscheme:language:simple-settings-case-sensitive 
   (drscheme:language:simple-settings? . -> . boolean?)
   (simple-settings)
   
   "Extracts the case-sensitive setting from a simple-settings.")
  
  (drscheme:language:simple-settings-printing-style
   (drscheme:language:simple-settings?
    . -> .
    (symbols 'constructor 'quasiquote 'write 'current-print))
   (simple-settings)
   
   "Extracts the printing-style setting from a simple-settings.")
  
  (drscheme:language:simple-settings-fraction-style
   (drscheme:language:simple-settings?
    . -> .
    (symbols 'mixed-fraction
             'mixed-fraction-e
             'repeating-decimal
             'repeating-decimal-e))
   (simple-settings)
   
   "Extracts the fraction-style setting from a simple-settings.")
  
  (drscheme:language:simple-settings-show-sharing
   (drscheme:language:simple-settings?
    . -> .
    boolean?)
   (simple-settings)
   
   "Extracts the show-sharing setting from a simple-settings.")
  
  (drscheme:language:simple-settings-insert-newlines
   (drscheme:language:simple-settings?
    . -> .
    boolean?)
   (simple-settings)
   
   "Extracts the insert-newline setting from a simple-settings.")
  
  (drscheme:language:simple-settings-annotations
   (drscheme:language:simple-settings?
    . -> .
    (symbols 'none 'debug 'debug/profile 'test-coverage))
   (simple-settings)
   
   "Extracts the debugging setting from a simple-settings.")
  
  (drscheme:language:simple-settings?
   (any/c . -> . boolean?)
   (val)
   
   "Determines if \\var{val} is a simple-settings.")
  
  (drscheme:language:make-simple-settings
   (boolean?
    (symbols 'constructor 'quasiquote 'write 'current-print)
    (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
    boolean?
    boolean?
    (symbols 'none 'debug 'debug/profile 'test-coverage)
    . -> .
    drscheme:language:simple-settings?)
   (case-sensitive
    printing-style
    fraction-style
    show-sharing
    insert-newlines
    annotations)
   
   "Constructs a simple settings.")
  
  (drscheme:language:simple-settings->vector
   (drscheme:language:simple-settings? . -> . vector?)
   (simple-settings)
   
   "Constructs a vector whose elements are the fields of \\var{simple-settings}.")
  
  
  ;                                                                   
  ;                                                                   
  ;                                                                   
  ;   ;              ;                       ;                 ;      
  ;   ;              ;                       ;                 ;      
  ;   ;              ;                       ;                 ;      
  ;   ; ;;     ;;;   ;   ; ;;             ;; ;    ;;;    ;;;   ;   ;  
  ;   ;;  ;   ;   ;  ;   ;;  ;           ;  ;;   ;   ;  ;      ;  ;   
  ;   ;   ;  ;    ;  ;   ;    ;         ;    ;  ;    ;  ;;     ; ;    
  ;   ;   ;  ;;;;;;  ;   ;    ;  ;;;;;; ;    ;  ;;;;;;   ;;    ;;;    
  ;   ;   ;  ;       ;   ;    ;         ;    ;  ;          ;   ;  ;   
  ;   ;   ;   ;      ;   ;;  ;           ;  ;;   ;         ;   ;   ;  
  ;   ;   ;    ;;;;  ;   ; ;;             ;; ;    ;;;;  ;;;    ;    ; 
  ;                      ;                                            
  ;                      ;                                            
  ;                      ;                                            
  
  (drscheme:help-desk:open-url
   (string? . -> . void?)
   (url)
   
   "Opens \\var{url} in a new help desk window.")
  
  (drscheme:help-desk:help-desk
   (case->
    (-> void?)
    (string? boolean? (symbols 'keyword 'keyword+index 'all) (symbols 'exact 'contains 'regexp)
             . -> .
             void?)
    (string? boolean? (symbols 'keyword 'keyword+index 'all) . -> . void?)
    (string? boolean? . -> . void?))
   (()
    (key lucky? type mode)
    (key lucky? type)
    (key lucky?))
   
   "This function opens a help desk window, or brings an already open help"
   "desk window to the front. If an argument is specified, that key is"
   "searched for."
   ""
   "If no arguments are supplied, this function"
   "opens a help-desk window to the starting page, or just brings a"
   "help-desk window to the front (without changing what page it is"
   "viewing)."
   ""
   "If any arguments are supplied, this function"
   "opens a help-desk window and searches for \\var{key}, according to "
   "\\var{lucky?}, \\var{type}, and \\var{mode}."
   "If the second, third, fourth, and/or fifth arguments are omitted, "
   "they default to \\scmc{\\#t} \\Symbol{keyword+index} and \\Symbol{exact},"
   "and \\Symbol{all} respectively.")
  
  
  
  
  ;                                                     
  ;                                                     
  ;                                                     
  ;   ;                                      ;       ;  
  ;   ;                                      ;          
  ;   ;                                      ;          
  ;   ;   ;;;    ; ;;     ;; ;        ;;;    ; ;;    ;  
  ;   ;  ;   ;   ;;  ;   ;  ;;       ;   ;   ;;  ;   ;  
  ;   ;      ;   ;   ;  ;    ;      ;     ;  ;    ;  ;  
  ;   ;   ;;;;   ;   ;  ;    ;      ;     ;  ;    ;  ;  
  ;   ;  ;   ;   ;   ;  ;    ;      ;     ;  ;    ;  ;  
  ;   ;  ;   ;   ;   ;   ;  ;;       ;   ;   ;;  ;   ;  
  ;   ;   ;;;;;  ;   ;    ;; ;        ;;;    ; ;;    ;  
  ;                          ;                       ;  
  ;                     ;    ;                       ;  
  ;                      ;;;;                      ;;   
  
  
  (define language-object
    
    (object-contract
     (config-panel ((is-a?/c area-container<%>)
                    . -> .
                    (case-> (any/c . -> . void?) (-> any/c))))
     (create-executable (any/c
                         (or/c (is-a?/c dialog%) (is-a?/c frame%))
                         path?
                         . -> .
                         void?))
     (default-settings (-> any/c))
     (default-settings? (any/c . -> . boolean?))
     (order-manuals ((listof bytes?) . -> . (values (listof bytes?) boolean?)))
     (front-end/complete-program (input-port?
                                  any/c
                                  . -> .
                                  (-> any/c)))
     (front-end/interaction (input-port?
                             any/c
                             . -> .
                             (-> any/c)))
     (get-language-name (-> string?))
     (get-language-numbers (-> (cons/c number? (listof number?))))
     (get-language-position (-> (cons/c string? (listof string?))))
     (get-language-url (-> (or/c false/c string?)))
     (get-one-line-summary (-> string?))
     (get-comment-character (-> (values string? char?)))
     (get-style-delta (-> (or/c false/c
                                (is-a?/c style-delta%)
                                (listof (list/c (is-a?/c style-delta%) number? number?)))))
     (marshall-settings (any/c . -> . printable/c))
     (on-execute (any/c ((-> any) . -> . any) . -> . any))
     (render-value (any/c 
                    any/c
                    output-port?
                    . -> .
                    void?))
     (render-value/format (any/c 
                           any/c
                           output-port?
                           (or/c number? (symbols 'infinity))
                           . -> .
                           any))
     (unmarshall-settings (printable/c . -> . any))
     (capability-value 
      (->d (and/c symbol? drscheme:language:capability-registered?)
           (λ (cap-name) (drscheme:language:get-capability-contract cap-name))))
     
     )
    #;
    (is-a?/c drscheme:language:language<%>)))
