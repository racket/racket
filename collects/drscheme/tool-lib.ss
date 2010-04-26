#lang at-exp racket/base

#|

This first time this is loaded, it loads all of drscheme and invokes
the main unit, starting up drscheme. After that, it just provides
all of the names in the tools library, for use defining keybindings

|#
(require scheme/class
         scheme/gui/base
         (except-in scheme/unit struct)
         scheme/contract
         scheme/class
         
         drscheme/private/link
         drscheme/private/drsig
         
         framework
         framework/splash
         
         mrlib/switchable-button
         scribble/srcdoc
         drscheme/private/language-object-contract)

(require (for-syntax scheme/base))

(require/doc drscheme/private/ts scheme/base scribble/manual)

(require/doc (for-label errortrace/errortrace-key
                        scheme/pretty 
                        mzlib/pconvert))

(define-values/invoke-unit/infer drscheme@)
(provide-signature-elements drscheme:tool-cm^) ;; provide all of the classes & interfaces

(provide drscheme:unit:program-editor-mixin)
(define-syntax (drscheme:unit:program-editor-mixin stx)
  (syntax-case stx ()
    [(_ a ...)
     #'((drscheme:unit:get-program-editor-mixin) a ...)]
    [_ #'(drscheme:unit:get-program-editor-mixin)]))

(language-object-abstraction drscheme:language:object/c #t)

(provide/doc
 
 (proc-doc/names
  drscheme:module-language-tools:add-opt-out-toolbar-button
  (-> (-> (is-a?/c top-level-window<%>)
          (is-a?/c area-container<%>) 
          (is-a?/c switchable-button%))
      symbol?
      void?)
  (make-button id)
  @{Call this function to add another button to DrScheme's toolbar. When buttons are added this way,
    DrScheme monitors the @tt{#lang} line at the top of the file; when it changes DrScheme queries
    the language to see if this button should be included.
    These buttons are ``opt out'', meaning that if the language doesn't explicitly ask to not
    have this button (or all such buttons), the button will appear.
    
    @section-index["drscheme:opt-out-toolbar-buttons"]
    See @scheme[read-language] for more details on how language's specify how to opt out.
    DrScheme will invoke the @tt{get-info} proc from @scheme[read-language] with
    @tt{'drscheme:opt-out-toolbar-buttons}. If the result is a list of symbols, the
    listed symbols are opted out. If the result is @scheme[#f], all buttons are opted
    out. The default is the empty list, meaning that all opt-out buttons appear..
    })
 
 (proc-doc/names
  drscheme:module-language:add-module-language
  (-> any)
  ()
  @{Adds the module language to DrScheme. This is called during DrScheme's startup.})
 
 (proc-doc/names
  drscheme:module-language:module-language-put-file-mixin
  (-> (implementation?/c text:basic<%>) (implementation?/c text:basic<%>))
  (super%)
  @{Extends @scheme[super%] by overriding the @method[editor<%> put-file] method
    to use a default name from the buffer, if the buffer contains something like
    @tt{(module name ...)}.})
  
 
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
 
 
 (proc-doc/names
  drscheme:eval:set-basic-parameters
  (-> (listof (is-a?/c snip-class%)) void?)
  (snipclasses)
  @{sets the parameters that are shared between the repl's
    initialization and @scheme[drscheme:eval:build-user-eventspace/custodian]
    
    Specifically, it sets these parameters:
    @itemize[
             @item{ @scheme[current-namespace] has been set to a newly
                    created empty namespace. This namespace has the following modules 
                    copied (with @scheme[namespace-attach-module])
                    from DrScheme's original namespace:
                    @itemize[@item{@scheme['mzscheme]}@item{@scheme['mred]}]
                    }@item{
                           @scheme[read-curly-brace-as-paren]
                            is @scheme[#t],
                            }@item{
                                   @scheme[read-square-bracket-as-paren]
                                    is @scheme[#t],
                                    }@item{ 
                                           @scheme[error-print-width] is set to 250.
                                           }@item{
                                                  @scheme[current-ps-setup]
                                                   is set to a newly created
                                                   @scheme[ps-setup%]
                                                   object.
                                                   }@item{ The @scheme[exit-handler] is set to
                                                               a parameter that kills the user's custodian.
                                                               }@item{ The snip-class-list, returned by
                                                                      @scheme[get-the-snip-class-list]
                                                                      is initialized with all of the snipclasses in DrScheme's eventspace's snip-class-list.
                                                                      
                                                                      }]})
 
 (proc-doc/names
  drscheme:eval:get-snip-classes
  (-> (listof (is-a?/c snip-class%)))
  ()
  @{Returns a list of all of the snipclasses in the current eventspace.})
 
 (proc-doc/names
  drscheme:eval:expand-program
  (-> (or/c port? drscheme:language:text/pos?)
      drscheme:language-configuration:language-settings?
      boolean?
      (-> void?)
      (-> void?)
      (-> (or/c eof-object? syntax? (cons/c string? any/c))
          (-> any)
          any)
      void?)
  (input language-settings eval-compile-time-part? init kill-termination iter)
  
  @{Use this function to expand the contents of the definitions
    window for use with external program processing tools.
    
    This function uses
    @scheme[drscheme:eval:build-user-eventspace/custodian]
    to build the user's environment.
    The arguments @scheme[language-settings], @scheme[init], and
    @scheme[kill-termination] are passed to
    @scheme[drscheme:eval:build-user-eventspace/custodian].
    
    The @scheme[input] argument specifies the source of the program.
    
    The @scheme[eval-compile-time-part?] argument indicates if
    @scheme[expand]
    is called or if
    @scheme[expand-top-level-with-compile-time-evals]
    is called when the program is expanded.
    Roughly speaking, if your tool will evaluate each expression
    itself by calling
    @scheme[eval]
    then pass @scheme[#f]. Otherwise, if your tool
    just processes the expanded program, be sure to pass
    @scheme[#t].
    
    This function calls
    @method[drscheme:language:language<%> front-end/complete-program]
    to expand the program. Unlike when the @onscreen{Run} is clicked,
    however, it does not call 
    @method[drscheme:language:language<%> front-end/finished-complete-program].
    
    
    The first argument to @scheme[iter] is the expanded program
    (represented as syntax) or eof.
    The @scheme[iter] argument is called for each expression in the
    expanded program and once more with eof, unless an error is
    raised during expansion.
    It is called from the user's thread.
    If an exception is raised during expansion of the
    user's program, @scheme[iter] is not called.
    Consider setting the exception-handler during @scheme[init] to
    handle this situation.
    
    The second argument to @scheme[iter] is a thunk that
    continues expanding the rest of the contents of the
    definitions window. If the first argument to @scheme[iter] was
    eof, this argument is just the primitive
    @scheme[void].
    
    See also
    @scheme[drscheme:eval:expand-program/multiple].})
 
 (proc-doc/names
  drscheme:eval:traverse-program/multiple
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
  
  @{This function is similar to
    @scheme[drscheme:eval:expand-program/multiple]
    The only difference is that it does not
    expand the program in the editor; instead
    the processing function can decide how to
    expand the program.})
 
 (proc-doc/names
  drscheme:eval:expand-program/multiple
  (-> drscheme:language-configuration:language-settings?
      boolean?
      (-> void?)
      (-> void?)
      (-> (or/c port? drscheme:language:text/pos?)
          (-> (or/c eof-object? syntax? (cons/c string? any/c))
              (-> any)
              any)
          boolean?
          void?))
  (language-settings eval-compile-time-part? init kill-termination)
  
  @{This function is just like
    @scheme[drscheme:eval:expand-program]
    except that it is curried and the second application
    can be used multiple times.
    Use this function if you want to initialize the user's
    thread (and namespace, etc) once but have program text
    that comes from multiple sources.
    
    The extra boolean argument to the result function
    determines if
    @scheme[drscheme:language:language front-end/complete-program<%>]
    or
    @scheme[drscheme:language:language front-end/interaction<%>]
    is called.})
 
 (proc-doc/names
  drscheme:eval:build-user-eventspace/custodian
  (->* (drscheme:language-configuration:language-settings?
        (-> void?)
        (-> void?))
       ()
       (values eventspace? custodian?))
  ((language-settings init kill-termination) ())
  
  @{This function creates a custodian and an eventspace (on the
    new custodian) to expand the user's program. It does not
    kill this custodian, but it can safely be shutdown (with
    @scheme[custodian-shutdown-all]) after the
    expansion is finished.
    
    It initializes the
    user's eventspace's main thread with several parameters:
    @itemize[
             @item{ @scheme[current-custodian] is set to a new custodian.
                    }@item{
                           In addition, it calls
                           @scheme[drscheme:eval:set-basic-parameters].
                           }]
    
    The @scheme[language-settings] argument is the current
    language and its settings. See
    @scheme[drscheme:language-configuration:make-language-settings]
    for details on that structure.
    
    If the program is associated with a DrScheme
    frame, get the frame's language settings from the
    @method[drscheme:unit:definitions-text<%> get-next-settings]
    method of 
    @scheme[drscheme:unit:definitions-text<%>].  Also, the most recently chosen language in
    the language dialog is saved via the framework's
    preferences. Apply
    @scheme[preferences:get]
    to
    @scheme[drscheme:language-configuration:get-settings-preferences-symbol]
    for that @scheme[language-settings].
    
    The @scheme[init] argument is called after the user's parameters
    are all set, but before the program is run. It is called on
    the user's thread. The
    @scheme[current-directory] and
    @scheme[current-load-relative-directory]
    parameters are not set, so if there are appropriate directories,
    the @scheme[init] argument is a good place to set them.
    
    The @scheme[kill-termination] argument is called when the main thread of
    the eventspace terminates, no matter if the custodian was
    shutdown, or the thread was killed. This procedure is also
    called when the thread terminates normally. This procedure is
    called from a new, dedicated thread (@italic{i. e.}, not the thread
    created to do the expansion, nor the thread that
    @scheme[drscheme:eval:build-user-eventspace/custodian] was called from.)})
 
 
 
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
 
 (proc-doc/names
  drscheme:debug:error-display-handler/stacktrace
  (->* (string? any/c)
       ((or/c false/c (listof srcloc?))
        #:definitions-text (or/c #f (is-a?/c drscheme:unit:definitions-text<%>))
        #:interactions-text (or/c #f (is-a?/c drscheme:rep:text<%>))
        )
       any/c)
  ((msg exn) ((stack #f)
              (defs #f)
              (ints #f)))
  @{Displays the error message represented by the string, adding
    embellishments like those that appears in the DrScheme REPL,
    specifically a clickable icon for the stack trace (if the srcloc location is not empty),
    and a clickable icon for the source of the error (read & syntax errors show their source
    locations and otherwise the first place in the stack trace is shown).
    
    If @scheme[stack] is false, then the stack traces embedded in the @scheme[exn] argument (if any) are used.
    Specifically, this function looks for a stacktrace via
    @scheme[errortrace-key] in the continuation marks of @scheme[exn] and @scheme[continuation-mark-set->context].
    
    If @scheme[stack] is not false, that stack is added to the stacks already in the exception.
    
    This should be called in the same eventspace and on the same thread as the error.})
 
 (proc-doc/names
  drscheme:debug:make-debug-error-display-handler
  (-> (-> string? (or/c any/c exn?) any)
      (-> string? (or/c any/c exn?) any))
  
  (oedh)
  
  @{This function implements an error-display-handler in terms
    of another error-display-handler.
    
    See also MzScheme's
    @scheme[error-display-handler]
    parameter.
    
    If the current-error-port is the definitions window in
    drscheme, this error handler inserts some debugging
    annotations, calls @scheme[oedh], and then highlights the
    source location of the runtime error.
    
    It looks for both stack trace information in the continuation
    marks both via the
    @schememodname[errortrace/errortrace-key] 
    module and via 
    @scheme[continuation-mark-set->context].
    
    })
 
 (proc-doc/names
  drscheme:debug:hide-backtrace-window
  (-> void?)
  ()
  @{Hides the backtrace window.})
 
 (proc-doc/names
  drscheme:debug:add-prefs-panel
  (-> void?)
  ()
  @{Adds the profiling preferences panel.})
 
 (proc-doc/names
  drscheme:debug:open-and-highlight-in-file
  (->* ((or/c srcloc? (listof srcloc?)))
       ((or/c #f (cons/c (λ (x) (and (weak-box? x)
                                     (let ([v (weak-box-value x)])
                                       (or (not v)
                                           (is-a?/c v editor<%>)))))
                         number?)))
       void?)
  ((debug-info)
   ((edition-pair #f)))
  @{This function opens a DrScheme to display
    @scheme[debug-info]. Only the src the position
    and the span fields of the srcloc are considered.
    
    The @scheme[edition-pair] is used to determine if a
    warning message is shown when before opening the file.
    If the @scheme[edition-pair] is not @scheme[#f], it is compared
    with the result of @method[text:basic<%> get-edition-number]
    of the editor that is loaded to determine if the file has been
    edited since the source location was recorded. If so, it 
    puts up a warning dialog message to that effect.})
 
 (proc-doc/names
  drscheme:debug:show-backtrace-window/edition-pairs
  (-> string?
      (listof srcloc?)
      (listof (or/c #f (cons/c (λ (x) (and (weak-box? x)
                                           (let ([v (weak-box-value x)])
                                             (or (not v)
                                                 (is-a?/c v editor<%>)))))
                               number?)))
      (or/c #f (is-a?/c drscheme:unit:definitions-text<%>))
      (or/c #f (is-a?/c drscheme:rep:text<%>))
      void?)
  (error-message dis editions-pairs defs ints)
  @{Shows the backtrace window you get when clicking on the bug in
    DrScheme's REPL.
    
    The @scheme[error-message] argument is the text of the error,
    @scheme[dis] is the debug information, extracted from the
    continuation mark in the exception record, using
    @scheme[errortrace-key].
    
    The @scheme[editions] argument indicates the editions of any editors
    that are open editing the files corresponding to the source locations
    
    The @scheme[defs] argument should be non-@scheme[#f] if there are 
    possibly stacktrace frames that contain unsaved versions of the 
    definitions window from drscheme. Similarly, the @scheme[ints] argument
    should be non-@scheme[#f] if there are possibly stacktrace frames that contain
    unsaved versions of the interactions window.
    
    Use
    @scheme[drscheme:rep:current-rep] to get the rep during evaluation of a program.
    
    })
 
 (proc-doc/names
  drscheme:debug:show-backtrace-window
  (->* (string?
        (or/c exn? 
              (listof srcloc?)
              (non-empty-listof (cons/c string? (listof srcloc?)))))
       ((or/c #f (is-a?/c drscheme:rep:text<%>))
        (or/c #f (is-a?/c drscheme:unit:definitions-text<%>)))
       void?)
  ((error-message dis)
   ((rep #f)
    (defs #f)))
  @{Shows the backtrace window you get when clicking on the bug in
    DrScheme's REPL.
    
    This function simply calls @scheme[drscheme:debug:show-backtrace-window/edition-pairs],
    using @scheme[drscheme:debug:srcloc->edition/pair].
    })
 
 (proc-doc/names
  drscheme:debug:srcloc->edition/pair
  (-> srcloc?
      (or/c #f (is-a?/c drscheme:rep:text<%>))
      (or/c #f (is-a?/c drscheme:unit:definitions-text<%>))
      (or/c #f (cons/c (let ([weak-box-containing-an-editor?
                              (λ (x) (and (weak-box? x)
                                          (let ([v (weak-box-value x)])
                                            (or (not v)
                                                (is-a?/c v editor<%>)))))])
                         weak-box-containing-an-editor?)
                       number?)))
  (srcloc ints defs)
  @{Constructs a edition pair from a source location,
    returning the current edition of the editor editing
    the source location (if any).
    
    The @scheme[ints] and @scheme[defs] arguments are used to map source locations, 
    in the case that the source location corresponds to the definitions
    window (when it has not been saved) or the interactions window.
    })
 
 
 
 ;                                                              
 ;                                                              
 ;                                                              
 ;   ;;             ;;                  ;;;               ;;    
 ;   ;;             ;;                   ;;               ;;    
 ;   ;;             ;;                   ;;               ;;    
 ;   ;;;;;;  ;;;;   ;;  ;;;;;         ;;;;;  ;;;;    ;;;; ;;  ;;
 ;   ;;  ;; ;;; ;;  ;;  ;;  ;;       ;; ;;; ;;; ;;  ;;  ; ;; ;  
 ;   ;;  ;; ;;;;;;  ;;  ;;  ;;  ;;;;;;;  ;; ;;;;;;  ;;;;  ;;;;  
 ;   ;;  ;; ;;      ;;  ;;  ;;  ;;;;;;;  ;; ;;      ;;;;  ;;;;; 
 ;   ;;  ;; ;;;  ;  ;;  ;;  ;;       ;;  ;; ;;;  ; ;  ;;  ;; ;; 
 ;   ;; ;;;  ;;;;   ;;  ;;;;;        ;;;;;;  ;;;;  ;;;;   ;; ;;;
 ;                      ;;                                      
 ;                      ;;                                      
 ;                      ;;                                      
 
 (proc-doc/names
  drscheme:help-desk:help-desk
  (->* ()
       ((or/c #f string?)
        (or/c #f string? (list/c string? string?)))
       any)
  (()
   ((search-key #f)
    (search-context #f)))
  @{if @scheme[search-key] is a string, calls @scheme[perform-search] with
       @scheme[search-key] and @scheme[search-context].
       
       Otherwise, calls @scheme[send-main-page] with no arguments.})
 
 
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
 
 
 (proc-doc/names
  drscheme:unit:get-program-editor-mixin
  (-> ((subclass?/c text%) . -> . (subclass?/c text%)))
  ()
  @{Returns a mixin that must be mixed in to any
    @scheme[text%] object that might contain
    program text (and thus can be in the source
    field of some syntax object).
    
    See also
    @scheme[drscheme:unit:add-to-program-editor-mixin].})
 
 (proc-doc/names
  drscheme:unit:add-to-program-editor-mixin
  (((subclass?/c text%) . -> . (subclass?/c text%)) . -> . void?)
  (mixin)
  @{@phase[1]
     
     Adds @scheme[mixin] to the result of
     @scheme[drscheme:unit:get-program-editor-mixin].})
 
 (proc-doc/names
  drscheme:unit:open-drscheme-window
  (case->
   (-> (is-a?/c drscheme:unit:frame%))
   ((or/c string? false/c) . -> . (is-a?/c drscheme:unit:frame%)))
  (() (filename))
  
  @{Opens a drscheme frame that displays @scheme[filename],
                                         or nothing if @scheme[filename] is @scheme[#f] or not supplied.})
 
 
 
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
 
 
 (proc-doc/names
  drscheme:modes:add-mode
  (string?
   (or/c false/c (is-a?/c mode:surrogate-text<%>))
   ((is-a?/c drscheme:rep:text%) number? . -> . boolean?)
   ((or/c false/c (listof string?)) . -> . boolean?)
   . -> .
   drscheme:modes:mode?)
  (name surrogate repl-submit matches-language)
  @{Adds a mode to DrScheme. Returns a mode value
    that identifies the mode.
    
    The first argument, @scheme[name], is the name
    of the mode, used in DrScheme's GUI to allow
    the user to select this mode.
    
    The @scheme[surrogate] argument is set to the
    definitions text and the interactions text
    (via the
    @scheme[mode:host-text set-surrogate<%>]
    method) whenever this mode is enabled.
    
    The @scheme[repl-submit] procedure is called
    whenever the user types a return in the interactions
    window. It is passed the interactions editor
    and the position where the last prompt occurs.
    If it 
    returns @scheme[#t], the text after the last
    prompt is treated as a program fragment and
    evaluated, according to the language settings.
    If it returns @scheme[#f], the text is
    assumed to be an incomplete program fragment, and
    the keystroke is not treated specially.
    
    The @scheme[matches-language] predicate is called whenever
    the language changes. If it returns @scheme[#t]
    this mode is installed. It is passed the list of strings
    that correspond to the names of the language in the
    language dialog.
    
    Modes are tested in the opposite order that they are
    added. That is, the last mode to be added gets tested
    first when the filename changes or when the language
    changes.
    
    See also
    @scheme[drscheme:modes:get-modes].})
 
 (proc-doc/names
  drscheme:modes:mode?
  (any/c . -> . boolean?)
  (val)
  @{Determines if @scheme[val] is a mode.})
 
 (proc-doc/names
  drscheme:modes:get-modes
  (-> (listof drscheme:modes:mode?))
  ()
  @{Returns all of the modes currently added to DrScheme.
    
    See also
    @scheme[drscheme:modes:add-mode].})
 
 (proc-doc/names
  drscheme:modes:mode-name
  (drscheme:modes:mode? . -> . string?)
  (mode)
  @{Extracts the name of the mode.
    
    See also
    @scheme[drscheme:modes:add-mode].})
 
 (proc-doc/names
  drscheme:modes:mode-surrogate
  (drscheme:modes:mode? . -> . (or/c false/c (is-a?/c mode:surrogate-text<%>)))
  (mode)
  @{Extracts the surrogate of the mode.
    
    See also
    @scheme[drscheme:modes:add-mode].})
 
 (proc-doc/names
  drscheme:modes:mode-repl-submit
  (drscheme:modes:mode? . -> . any)
  (mode)
  @{Extracts the repl submission predicate of the mode.
    
    See also
    @scheme[drscheme:modes:add-mode].})
 
 (proc-doc/names
  drscheme:modes:mode-matches-language
  (drscheme:modes:mode? . -> . ((or/c false/c (listof string?)) . -> . boolean?))
  (mode)
  @{Extracts the language matching predicate of the mode.
    
    See also
    @scheme[drscheme:modes:add-mode].})
 
 
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
 
 
 (proc-doc/names
  drscheme:rep:get-welcome-delta 
  (-> (is-a?/c style-delta%))
  ()
  @{Returns a style delta that matches the style and color of the 
    phrase ``Welcome to'' in the beginning of the interactions window.})
 
 (proc-doc/names
  drscheme:rep:get-dark-green-delta
  (-> (is-a?/c style-delta%))
  ()
  @{Returns a style delta that matches the style and color of the 
    name of a language in the interactions window.})
 
 (proc-doc/names
  drscheme:rep:get-drs-bindings-keymap
  (-> (is-a?/c keymap%))
  ()
  @{Returns a keymap that binds various DrScheme-specific
    keybindings. This keymap is used in the definitions
    and interactions window.
    
    Defaultly binds C-x;o to a function that switches
    the focus between the definitions and interactions
    windows. Also binds f5 to Execute and f1 to Help Desk.})
 
 (proc-doc/names
  drscheme:rep:current-rep
  (-> (or/c false/c (is-a?/c drscheme:rep:text%)))
  ()
  
  @{This is a parameter whose value should not be set by tools.
    It is initialized to the repl that controls this evaluation
    in the user's thread.
    
    It only returns @scheme[#f] if the program not running
    in the context of a repl (eg, the test suite window).})
 
 (proc-doc/names
  drscheme:rep:current-value-port
  (-> (or/c false/c port?))
  ()
  @{This is a parameter whose value is a port that
    prints in the REPL in blue. It is used to print
    the values of toplevel expressions in the REPL.
    
    It is only initialized on the user's thread.})
 
 
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
 
 
 (proc-doc/names
  drscheme:get/extend:extend-tab
  (case->
   ((make-mixin-contract drscheme:unit:tab<%>) . -> . void?)
   ((make-mixin-contract drscheme:unit:tab<%>) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This class implements the tabs in drscheme. One is created for each tab
    in a frame (each frame always has at least one tab, even if the tab bar is not shown)
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#t].})
 
 (proc-doc/names
  drscheme:get/extend:extend-interactions-text
  (case->
   ((make-mixin-contract drscheme:rep:text<%>) . -> . void?)
   ((make-mixin-contract drscheme:rep:text<%>) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This text is used in the bottom window of drscheme frames.
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#t].})
 
 (proc-doc/names
  drscheme:get/extend:get-interactions-text
  (-> (implementation?/c drscheme:rep:text<%>))
  ()
  
  @{Once this function is called, 
    @scheme[drscheme:get/extend:extend-interactions-text] 
    raises an error, disallowing any more extensions.})
 
 (proc-doc/names
  drscheme:get/extend:extend-definitions-text
  (case->
   ((make-mixin-contract drscheme:unit:definitions-text<%>) . -> . void?)
   ((make-mixin-contract drscheme:unit:definitions-text<%>) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This text is used in the top window of drscheme frames.
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#f].})
 
 (proc-doc/names
  drscheme:get/extend:get-definitions-text
  (-> (implementation?/c drscheme:unit:definitions-text<%>))
  ()
  
  @{Once this function is called,
    @scheme[drscheme:get/extend:extend-definitions-text] 
    raises an error, disallowing any more extensions.})
 
 (proc-doc/names
  drscheme:get/extend:extend-interactions-canvas
  (case->
   ((make-mixin-contract drscheme:unit:interactions-canvas%) . -> . void?)
   ((make-mixin-contract drscheme:unit:interactions-canvas%) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This canvas is used in the bottom window of drscheme frames.
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#f].})
 
 (proc-doc/names
  drscheme:get/extend:get-interactions-canvas
  (-> (subclass?/c drscheme:unit:interactions-canvas%))
  ()
  
  @{Once this function is called, 
    @scheme[drscheme:get/extend:extend-interactions-canvas]
    raises an error, disallowing any more extensions.})
 
 (proc-doc/names
  drscheme:get/extend:extend-definitions-canvas
  (case->
   ((make-mixin-contract drscheme:unit:definitions-canvas%) . -> . void?)
   ((make-mixin-contract drscheme:unit:definitions-canvas%) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This canvas is used in the top window of drscheme frames.
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#f].})
 
 (proc-doc/names
  drscheme:get/extend:get-definitions-canvas
  (-> (subclass?/c drscheme:unit:definitions-canvas%))
  ()
  
  @{Once this function is called, 
    @scheme[drscheme:get/extend:extend-definitions-canvas]
    raises an error, disallowing any more extensions.})
 
 (proc-doc/names
  drscheme:get/extend:extend-unit-frame
  (case->
   ((make-mixin-contract drscheme:unit:frame%) . -> . void?)
   ((make-mixin-contract drscheme:unit:frame%) boolean? . -> . void?))
  ((mixin) (mixin before?))
  
  @{This is the frame that implements the main drscheme window.
    
    The argument, @scheme[before], controls if the mixin is applied before or
    after already installed mixins.
    If unsupplied, this is the same as supplying @scheme[#f].})
 
 (proc-doc/names
  drscheme:get/extend:get-unit-frame
  (-> (subclass?/c drscheme:unit:frame%))
  ()
  
  @{Once this function is called, 
    @scheme[drscheme:get/extend:extend-unit-frame]
    raises an error, disallowing any more extensions.})
 
 
 
 ;                                                
 ;                                                
 ;                                                
 ;                                                
 ;    ;                       ;;;                 
 ;  ;;;                                           
 ;  ;;;; ;;; ;;;;;;;    ;;;   ;;; ;;; ;;   ;; ;;; 
 ;  ;;;; ;;;;;;;;;;;;  ;;;;;  ;;; ;;;;;;; ;;;;;;; 
 ;  ;;;  ;;;  ;;  ;;; ;;;  ;; ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;  ;;;    ;;;;; ;;;     ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;  ;;;  ;;; ;;; ;;;  ;; ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;; ;;;  ;;; ;;;  ;;;;;  ;;; ;;; ;;; ;;;;;;; 
 ;   ;;; ;;;   ;;;;;;   ;;;   ;;; ;;; ;;;  ;; ;;; 
 ;                                            ;;; 
 ;                                        ;;;;;;  
 ;                                                
 ;                                                
 
 (proc-doc/names
  drscheme:tracing:annotate
  (-> syntax? syntax?)
  (stx)
  @{Call this function to add tracing annotations to the a fully-expanded
    expression. When the program runs, DrScheme will pop open the tracing
    window to display the trace.})
 
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
 
 (proc-doc/names
  drscheme:language-configuration:get-languages
  (-> (listof (is-a?/c drscheme:language:language<%>)))
  ()
  @{This can only be called after all of the tools initialization phases have completed.
    
    Returns the list of all of the languages installed in DrScheme.})
 
 (proc-doc/names
  drscheme:language-configuration:add-language
  ((and/c (is-a?/c drscheme:language:language<%>) drscheme:language:object/c)
   . -> . void?)
  (language)
  
  @{@phase[2]
     
     Adds @scheme[language] to the languages offerend by DrScheme.})
 
 (proc-doc/names
  drscheme:language-configuration:get-settings-preferences-symbol
  (-> symbol?)
  ()
  @{Returns the symbol that is used to store the user's language
    settings. Use as an argument to either
    @scheme[preferences:get]
    or
    @scheme[preferences:set].})
 
 (proc-doc/names
  drscheme:language-configuration:make-language-settings
  ((or/c (is-a?/c drscheme:language:language<%>) drscheme:language:object/c)
   any/c
   . -> .
   drscheme:language-configuration:language-settings?)
  (language settings)
  
  @{This is the constructor for a record consisting of two
    elements, a language and its settings. 
    
    The settings is a language-specific record that holds a
    value describing a parameterization of the language.
    
    It has two selectors,
    @scheme[drscheme:language-configuration:language-settings-language]
    and 
    @scheme[drscheme:language-configuration:language-settings-settings], and a predicate,
    @scheme[drscheme:language-configuration:language-settings?]})
 
 (proc-doc/names
  drscheme:language-configuration:language-settings-settings
  (-> drscheme:language-configuration:language-settings?
      any/c)
  (ls)
  @{Extracts the settings field of a language-settings.})
 
 (proc-doc/names
  drscheme:language-configuration:language-settings-language
  (drscheme:language-configuration:language-settings?
   . -> .
   (or/c (is-a?/c drscheme:language:language<%>) drscheme:language:object/c))
  (ls)
  
  @{Extracts the language field of a language-settings.})
 
 (proc-doc/names
  drscheme:language-configuration:language-settings?
  (any/c . -> . boolean?)
  (val)
  
  @{Determines if the argument is a language-settings or not.})
 
 (proc-doc/names
  drscheme:language-configuration:language-dialog
  (->* (boolean? drscheme:language-configuration:language-settings?)
       ((or/c false/c (is-a?/c top-level-window<%>)))
       (or/c false/c drscheme:language-configuration:language-settings?))
  ((show-welcome? language-settings-to-show)
   ((parent #t)))
  @{Opens the language configuration dialog.
    See also
    @scheme[drscheme:language-configuration:fill-language-dialog].
    
    The @scheme[show-welcome?] argument determines if
    if a ``Welcome to DrScheme'' message and some
    natural language buttons are shown.
    
    The @scheme[language-settings-to-show] argument
    must be some default language settings that the dialog
    is initialized to.
    If unsure of a default, the currently set language
    in the user's preferences can be obtained via:
    @schemeblock[
                 (preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))
                                                                                                    ]
    
    The @scheme[parent] argument is used as the parent
    to the dialog.
    
    The result if @scheme[#f] when the user cancells the dialog, and
    the selected language if they hit ok.})
 
 (proc-doc/names
  drscheme:language-configuration:fill-language-dialog
  (->*
   ((is-a?/c vertical-panel%)
    (is-a?/c area-container<%>)
    drscheme:language-configuration:language-settings?)
   ((or/c false/c (is-a?/c top-level-window<%>))
    (-> symbol? void?))
   drscheme:language-configuration:language-settings?)
  ((panel button-panel language-setting)
   ((re-center #f)
    (ok-handler void)))
  @{This procedure accepts two parent panels and
    fills them with the contents of the language dialog.
    It is used to include language configuration controls
    in some larger context in another dialog.
    
    The @scheme[panel] argument is the main panel where the
    language controls will be placed.
    The function adds buttons to the @scheme[button-panel]
    to revert a language to its default settings and to
    show the details of a language.
    
    The @scheme[language-setting] is the default
    language to show in the dialog.
    
    The @scheme[re-center] argument is used when the @onscreen{Show Details}
    button is clicked. If that argument is a @scheme[top-level-window<%>],
    the @onscreen{Show Details} callback will recenter the window each time
    it is clicked. Otherwise, the argument is not used.
    
    @scheme[ok-handler] is a function that is in charge of interfacing the OK
    button. It should accept a symbol message: @scheme['enable] and
    @scheme['disable] to toggle the button, and @scheme['execute] to run
    the desired operation. (The language selection dialog also uses an
    internal @scheme['enable-sync] message.)})
 
 (proc-doc
  drscheme:language:register-capability
  (->d ([s symbol?]
        [the-contract contract?]
        [default the-contract])
       ()
       [res void?])
  @{Registers a new capability with a default value for each language
    and a contract on the values the capability might have.
    
    By default, these capabilities are registered as DrScheme starts up:
    @(let-syntax ([cap (syntax-rules ()
                         [(cap key contract default desc ...)
                          (item @scheme['key : contract = default]
                                "--- " desc ...)])])
       (itemize
        @cap[drscheme:check-syntax-button boolean? #t]{
                                                       controls the visiblity of the check syntax button}
        @cap[drscheme:language-menu-title
             string?
             (string-constant scheme-menu-name)]{
                                                 controls the name of the menu just to the right of the language
                                                 menu (defaultly named ``Scheme'')}
                                                @cap[drscheme:define-popup
                                                     (or/c #f
                                                           (list/c string? string? string?)
                                                           (cons/c string? string?))
                                                     (list "(define" "(define ...)" "δ")]{
                                                                                          specifies the prefix that the define popup should look for and what
                                                                                          label it should have, or @scheme[#f] if it should not appear at all.
                                                                                          
                                                                                          If the list of three strings alternative is used, the first string is
                                                                                          the prefix that is looked for when finding definitions. The second
                                                                                          and third strings are used as the label of the control, in horizontal
                                                                                          and vertical mode, respectively.
                                                                                          
                                                                                          The pair of strings alternative is deprecated. If it is used, 
                                                                                          the pair @scheme[(cons a-str b-str)] is the same as @scheme[(list a-str b-str "δ")].}
                                                                                         @cap[drscheme:help-context-term (or/c false/c string?) #f]{
                                                                                                                                                    specifies a context query for documentation searches that are
                                                                                                                                                    initiated in this language, can be @scheme[#f] (no change to the
                                                                                                                                                    user's setting) or a string to be used as a context query (note: the
                                                                                                                                                    context is later maintained as a cookie, @scheme[""] is different
                                                                                                                                                    from @scheme[#f] in that it clears the stored context)}
                                                                                         @cap[drscheme:special:insert-fraction boolean? #t]{
                                                                                                                                            determines if the insert fraction menu item in the special menu is
                                                                                                                                            visible}
                                                                                         @cap[drscheme:special:insert-lambda boolean? #t]{
                                                                                                                                          determines if the insert lambda menu item in the special menu is
                                                                                                                                          visible}
                                                                                         @cap[drscheme:special:insert-large-letters boolean? #t]{
                                                                                                                                                 determines if the insert large letters menu item in the special menu
                                                                                                                                                 is visible}
                                                                                         @cap[drscheme:special:insert-image boolean? #t]{
                                                                                                                                         determines if the insert image menu item in the special menu is
                                                                                                                                         visible}
                                                                                         @cap[drscheme:special:insert-comment-box boolean? #t]{
                                                                                                                                               determines if the insert comment box menu item in the special menu
                                                                                                                                               is visible}
                                                                                         @cap[drscheme:special:insert-gui-tool boolean? #t]{
                                                                                                                                            determines if the insert gui menu item in the special menu is
                                                                                                                                            visible}
                                                                                         @cap[drscheme:special:slideshow-menu-item boolean? #t]{
                                                                                                                                                determines if the insert pict box menu item in the special menu is
                                                                                                                                                visible}
                                                                                         @cap[drscheme:special:insert-text-box boolean? #t]{
                                                                                                                                            determines if the insert text box menu item in the special menu is
                                                                                                                                            visible}
                                                                                         @cap[drscheme:special:xml-menus boolean? #t]{
                                                                                                                                      determines if the insert scheme box, insert scheme splice box, and
                                                                                                                                      the insert xml box menu item in the special menu are visible}
                                                                                         @cap[drscheme:autocomplete-words (listof string?) '()]{
                                                                                                                                                determines the list of words that are used when completing words in
                                                                                                                                                this language}
                                                                                         @cap[drscheme:tabify-menu-callback
                                                                                              (or/c false/c (-> (is-a?/c text%) number? number? void?))
                                                                                              (λ (t a b) (send t tabify-selection a b))]{
                                                                                                                                         is used as the callback when the ``Reindent'' or ``Reindent All''
                                                                                                                                         menu is selected. The first argument is the editor, and the second
                                                                                                                                         and third are a range in the editor.}
                                                                                                                                        ))})
 
 (proc-doc/names
  drscheme:language:capability-registered? 
  (-> symbol? boolean?)
  (s)
  @{Indicates if
    @scheme[drscheme:language:register-capability]
    has been called with @scheme[s].})
 (proc-doc
  drscheme:language:get-capability-default
  (->d ([s (and/c symbol? drscheme:language:capability-registered?)])
       ()
       [res (drscheme:language:get-capability-contract s)])
  @{Returns the default for a particular capability.})
 (proc-doc/names
  drscheme:language:get-capability-contract
  (-> (and/c symbol? drscheme:language:capability-registered?)
      contract?)
  (s)
  @{Returns the contract for a given capability, which was specified
    when @scheme[drscheme:language:register-capability] was called.})
 
 
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
 
 
 (proc-doc/names
  drscheme:language:add-snip-value
  (->* ((-> any/c boolean?)
        (-> any/c (is-a?/c snip%)))
       ((-> any/c))
       void?)
  ((test-value convert-value)
   ((setup-thunk void)))
  @{Registers a handler to convert values into snips as they are printed in the REPL.
    
    The @scheme[test-snip] argument is called to determine if this handler can convert the value 
    and the @scheme[convert-value] argument is called to build a snip. 
    The (optional) @scheme[setup-thunk] is called just after the user's namespace and other 
    setings are built, but before any of the user's code is evaluated.
    
    All three functions are called on the user's thread and with the user's settings.})
 
 (proc-doc/names
  drscheme:language:extend-language-interface
  (-> interface?
      (make-mixin-contract drscheme:language:language<%>)
      void?)
  (interface default-implementation)
  
  @{@phase[1]
     
     Each language added passed to
     @scheme[drscheme:language-configuration:add-language]
     must implement @scheme[interface]. 
     
     The @scheme[default-implementation] is a mixin
     that provides a default implementation of 
     @scheme[interface]. Languages that are unaware of
     the specifics of @scheme[extension] use
     @scheme[default-implementation] via
     @scheme[drscheme:language:get-default-mixin].})
 
 (proc-doc
  drscheme:language:get-default-mixin
  (-> (make-mixin-contract drscheme:language:language<%>))
  
  @{@phase[2]
     
     The result of this function is the composite of all of the 
     @scheme[default-implementation] arguments passed
     to
     @scheme[drscheme:language:extend-language-interface].})
 
 (proc-doc/names
  drscheme:language:get-language-extensions
  (-> (listof interface?))
  ()
  @{@phase[2]
     
     Returns a list of the interfaces passed to
     @scheme[drscheme:language:extend-language-interface].})
 
 (proc-doc/names
  drscheme:language:put-executable
  ((is-a?/c top-level-window<%>) 
   path? 
   (or/c boolean? (symbols 'launcher 'standalone 'distribution)) 
   boolean? 
   string? 
   . -> . (or/c false/c path?))
  (parent program-filename mode mred? title)
  @{Calls the MrEd primitive
    @scheme[put-file]
    with arguments appropriate for creating an executable
    from the file @scheme[program-filename]. 
    
    The arguments @scheme[mred?] and @scheme[mode] indicates
    what type of executable this should be (and the dialog
    may be slightly different on some platforms, depending
    on these arguments). For historical reasons, @scheme[#f]
    is allowed for @scheme[mode] as an alias for @scheme['launcher], and
    @scheme[#t] is allowed for @scheme[mode] as an alias for @scheme['stand-alone].
    
    The @scheme[title] argument is used as the title to the primitive
    @scheme[put-file]
    or
    @scheme[get-directory]
    primitive.})
 
 (proc-doc/names
  drscheme:language:create-executable-gui
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
  @{Opens a dialog to prompt the user about their choice of executable.
    If @scheme[show-type] is @scheme[#t], the user is prompted about
    a choice of executable: stand-alone,
    launcher, or distribution; otherwise, the symbol determines the type.
    If @scheme[show-base]
    is @scheme[#t], the user is prompted about a choice of base
    binary: mzscheme or mred; otherwise the symbol determines the base.
    
    The @scheme[program-name] argument is used to construct the default
    executable name in a platform-specific manner.
    
    The @scheme[parent] argument is used for the parent of the dialog.
    
    The result of this function is @scheme[#f] if the user cancel's
    the dialog and a list of three items indicating what options
    they chose. If either @scheme[show-type] or @scheme[show-base]
    was not @scheme[#t], the corresponding result will be @scheme['no-show],
    otherwise it will indicate the user's choice.})
 
 (proc-doc/names
  drscheme:language:create-module-based-stand-alone-executable 
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
  
  @{This procedure creates a stand-alone executable in the file
    @scheme[executable-filename] that runs the program
    @scheme[program-filename]. 
    
    The arguments
    @scheme[module-language-spec] and
    @scheme[transformer-module-language-spec] specify the 
    settings of the initial namespace, both the transformer
    portion and the regular portion. Both may be @scheme[#f]
    to indicate there are no initial bindings.
    
    The @scheme[init-code] argument is an s-expression representing
    the code for a module. This module is expected to provide
    the identifer @scheme[init-code], bound to a procedure of no
    arguments. That module is required and the @scheme[init-code]
    procedure is executed to initialize language-specific
    settings before the code in @scheme[program-filename] runs.
    
    The @scheme[gui?] argument indicates if a MrEd or MzScheme
    stand-alone executable is created.
    
    The @scheme[use-copy?] argument indicates if the initial
    namespace should be populated with
    @scheme[namespace-require/copy] or
    @scheme[namespace-require]. })
 
 (proc-doc/names
  drscheme:language:create-module-based-distribution
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
  
  @{Like
    @scheme[drscheme:language:create-module-based-stand-alone-executable], but packages the stand-alone executable into a distribution.})
 
 (proc-doc/names
  drscheme:language:create-distribution-for-executable
  ((or/c path? string?) 
   boolean?
   (-> path? void?)
   . -> .
   void?)
  (distribution-filename
   gui?
   make-executable)
  
  @{Creates a distribution where the given @scheme[make-executable] procedure
                                           creates the stand-alone executable to be distributed. 
                                           The @scheme[make-executable] procedure is given the name of the 
                                           executable to create. The @scheme[gui?] argument is needed in case the
                                           executable's name (which @scheme[drscheme:language:create-distribution-for-executable] 
                                           must generate) depends on the type of executable. During the distribution-making 
                                           process, a progress dialog is shown to the user, and the user can click an 
                                           @onscreen{Abort} button that sends a break to the current thread.})
 
 (proc-doc/names
  drscheme:language:create-module-based-launcher
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
  
  @{This procedure is identical to 
    @scheme[drscheme:language:create-module-based-stand-alone-executable], except that it creates a launcher instead of a
    stand-alone executable.})
 
 (proc-doc/names
  drscheme:language:simple-module-based-language-convert-value
  (-> any/c drscheme:language:simple-settings? any/c)
  (value settings)
  @{Sets the @scheme[constructor-style-printing] and @scheme[show-sharing] 
    parameters based on @scheme[settings] and sets @scheme[current-print-convert-hook]
    to ignore snips and then uses @scheme[print-convert] on @scheme[value].
    
    Unless, of course, the @scheme[settings] argument has @scheme['write] in
    the @scheme[simple-settings-printing-style] field, in which case it simply 
    returns @scheme[value].})
 
 (proc-doc/names
  drscheme:language:setup-printing-parameters
  (-> (-> any) drscheme:language:simple-settings? (or/c number? 'infinity) any)
  (thunk settings width)
  @{Sets all of the @scheme[pretty-print] and @scheme[print-convert] parameters
    either to the defaults to values based on @scheme[settings]
    and then invokes @scheme[thunk], returning what it returns.})
 
 (proc-doc/names
  drscheme:language:text/pos-text
  (drscheme:language:text/pos? . -> . (is-a?/c text%))
  (text/pos)
  
  @{Selects the @scheme[text%] from a text/pos.})
 
 (proc-doc/names
  drscheme:language:text/pos-start
  (drscheme:language:text/pos? . -> . number?)
  (text/pos)
  
  @{Selects the starting position from a text/pos.})
 
 (proc-doc/names
  drscheme:language:text/pos-end
  (drscheme:language:text/pos? . -> . number?)
  (text/pos)
  
  @{Selects the ending position from a text/pos.})
 
 (proc-doc/names
  drscheme:language:text/pos?
  (any/c . -> . boolean?)
  (val)
  
  @{Returns @scheme[#t] if @scheme[val] is a text/pos, and @scheme[#f]
            otherwise.})
 
 (proc-doc/names
  drscheme:language:make-text/pos
  ((is-a?/c text%) number? number?
                   . -> .
                   drscheme:language:text/pos?)
  (text start end)
  
  @{Constructs a text/pos.})
 
 (proc-doc/names
  drscheme:language:simple-settings-case-sensitive 
  (drscheme:language:simple-settings? . -> . boolean?)
  (simple-settings)
  
  @{Extracts the case-sensitive setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings-printing-style
  (drscheme:language:simple-settings?
   . -> .
   (symbols 'constructor 'quasiquote 'write))
  (simple-settings)
  
  @{Extracts the printing-style setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings-fraction-style
  (drscheme:language:simple-settings?
   . -> .
   (symbols 'mixed-fraction
            'mixed-fraction-e
            'repeating-decimal
            'repeating-decimal-e))
  (simple-settings)
  
  @{Extracts the fraction-style setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings-show-sharing
  (drscheme:language:simple-settings?
   . -> .
   boolean?)
  (simple-settings)
  
  @{Extracts the show-sharing setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings-insert-newlines
  (drscheme:language:simple-settings?
   . -> .
   boolean?)
  (simple-settings)
  
  @{Extracts the insert-newline setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings-annotations
  (drscheme:language:simple-settings?
   . -> .
   (symbols 'none 'debug 'debug/profile 'test-coverage))
  (simple-settings)
  
  @{Extracts the debugging setting from a simple-settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings?
  (any/c . -> . boolean?)
  (val)
  
  @{Determines if @scheme[val] is a simple-settings.})
 
 (proc-doc/names
  drscheme:language:make-simple-settings
  (-> boolean?
      (symbols 'constructor 'quasiquote 'write)
      (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
      boolean?
      boolean?
      (symbols 'none 'debug 'debug/profile 'test-coverage)
      drscheme:language:simple-settings?)
  (case-sensitive
   printing-style
   fraction-style
   show-sharing
   insert-newlines
   annotations)
  
  @{Constructs a simple settings.})
 
 (proc-doc/names
  drscheme:language:simple-settings->vector
  (drscheme:language:simple-settings? . -> . vector?)
  (simple-settings)
  
  @{Constructs a vector whose elements are the fields of @scheme[simple-settings].}))
