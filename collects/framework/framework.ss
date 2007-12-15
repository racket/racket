
(module framework mzscheme
  (require (lib "unit.ss")
           (lib "mred-unit.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           
           "preferences.ss"
           "test.ss"
           "gui-utils.ss"
           "decorated-editor-snip.ss"
           
           "framework-unit.ss"
           "private/sig.ss"
           
           (lib "contract.ss"))
  
  (provide-signature-elements
   (prefix application: framework:application-class^)
   (prefix version: framework:version-class^)
   (prefix color-model: framework:color-model-class^)
   (prefix mode: framework:mode-class^)
   (prefix exit: framework:exit-class^)
   (prefix menu: framework:menu-class^)
   (prefix preferences: framework:preferences-class^)
   (prefix number-snip: framework:number-snip-class^)
   (prefix autosave: framework:autosave-class^)
   (prefix path-utils: framework:path-utils-class^)
   (prefix icon: framework:icon-class^)
   (prefix keymap: framework:keymap-class^)
   (prefix editor: framework:editor-class^)
   (prefix pasteboard: framework:pasteboard-class^)
   (prefix text: framework:text-class^)
   (prefix color: framework:color-class^)
   (prefix color-prefs: framework:color-prefs-class^)
   (prefix comment-box: framework:comment-box-class^)
   (prefix finder: framework:finder-class^)
   (prefix group: framework:group-class^)
   (prefix canvas: framework:canvas-class^)
   (prefix panel: framework:panel-class^)
   (prefix frame: framework:frame-class^)
   (prefix handler: framework:handler-class^)
   (prefix scheme: framework:scheme-class^)
   (prefix main: framework:main-class^))

  (provide (all-from "test.ss")
           (all-from "gui-utils.ss")
           (all-from "preferences.ss")
           (all-from "decorated-editor-snip.ss"))

  (define-syntax (provide/contract/docs stx)
    (syntax-case stx ()
      [(_ (name contract docs ...) ...)
       (syntax (provide/contract (name contract) ...))]))
    
  (define-compound-unit/infer framework+mred@
    (import)
    (export framework^)
    (link standard-mred@ framework@))
     
  
  (define-values/invoke-unit/infer framework+mred@)

  (provide/contract/docs
   
   (text:autocomplete-append-after
    (parameter/c string?)
    ()
    "A string that is inserted after a completions is inserted by a"
    "@ilink text:autocomplete"
    "instance."
    ""
    "Defaults to \"\"")
   (text:autocomplete-limit
    (parameter/c (and/c integer? exact? positive?))
    ()
    "Controls the number of completions visible at a time in the menu"
    "produced by"
    "@ilink text:autocomplete"
    "instances."
    ""
    "Defaults to 15.")
   (text:get-completions/manuals
    (-> (or/c false/c (listof symbol?)) (listof string?))
    (manuals)
    "Returns the list of keywords for the manuals from \\var{manuals}"
    "by extracting all of the documented exports of the manuals."
    "The symbols are meant to be module paths."
    "If \\var{manuals} is false, then all of the documented names are used.")
   
   (number-snip:make-repeating-decimal-snip
    (number? boolean? . -> . (is-a?/c snip%))
    (num show-prefix?)
    
    "Makes a number snip that shows the decimal expansion for \\var{number}"
    "The boolean indicates if a {\\tt \\#e} prefix appears"
    "on the number."
    ""
    "See also"
    "@flink number-snip:make-fraction-snip %"
    ".")
   (number-snip:make-fraction-snip
    (number? boolean? . -> . (is-a?/c snip%))
    (num show-prefix-in-decimal-view?)
    
    "Makes a number snip that shows a fractional view of \\var{number}."
    "The boolean indicates if a {\\tt \\#e} prefix appears"
    "on the number, when shown in the decimal state."
    ""
    "See also"
    "@flink number-snip:make-repeating-decimal-snip %"
    ".")   
   (version:add-spec
    (any/c any/c . -> . void?)
    (spec revision)
    "These two values are appended to the version string. \\rawscm{write} is"
    "used to transform them to strings. For example:"
    ""
    "\\rawscm{(version:add-spec 's 1)}"
    ""
    "in version 205 will make the version string be \\rawscm{\"205s1\"}. The"
    "symbols \\rawscm{'f} and \\rawscm{'d} are used internally for framework and"
    "drscheme revisions.")
   (version:version
    (-> string?)
    ()
    "This function returns a string describing the version of this"
    "application. See also "
    "@flink version:add-spec %"
    ".")

   (application:current-app-name
    (case-> (-> string?)
            (string? . -> . void?))
    (() (name))
    "This is a parameter specifying the name of the current application. It"
    "is used in the help menu (see \\iscmclass{frame:standard-menus}) and in"
    "frame titles (see \\iscmclass{frame:editor})."
    ""
    "The first case in the case-lambda returns"
    "the current name, and"
    "the second case in the case-lambda sets"
    "the name of the application to \\var{name}.")

   (preferences:put-preferences/gui
    (-> (listof symbol?)
        (listof any/c)
        any)
    (name-list val-list)
    "Like \\scheme{put-preferences}, but has more sophisticated"
    "error handling. In particular, it"
    "\\begin{itemize}"
    "\\item waits for three consecutive failures before informing the user"
    "\\item gives the user the opportunity to ``steal'' the lockfile after the"
    "third failure, and"
    "\\item when failures occur, it remembers what its arguments were and"
    "if any preference save eventually succeeds, all of the past failures are"
    "also written at that point.")

   (preferences:add-panel
    ((or/c string? (cons/c string? (listof string?)))
     ((is-a?/c area-container-window<%>) 
      . ->d .
      (λ (parent)
        (let ([children (map (λ (x) x) (send parent get-children))])
          (λ (child)
            (and (is-a? child area-container-window<%>)
                 (andmap eq?
                         (append children (list child))
                         (send parent get-children)))))))
     . -> .
     void?)
    (labels f)
   "\\rawscm{preferences:add-preference-panel} adds the result of"
   "\\var{f} with name \\var{labels} to the preferences dialog"
   "box."
   ""
   "The labels determine where this preference panel is placed"
   "in the dialog. If the list is just one string, the"
   "preferences panel is placed at the top level of the dialog."
   "If there are more strings, a hierarchy of nested panels is"
   "created and the new panel is added at the end."
   "If multiple calls to \\rawscm{preferences:add-preference-panel}"
   "pass the same prefix of strings, those panels are placed in the"
   "same children."
   ""
   "When the preference dialog is opened for the first"
   "time, the function \\var{f} is called with a panel, and"
   "\\var{f} is expected to add a new child panel to it and add"
   "whatever preferences configuration controls it wants to that"
   "panel. Then, \\var{f}'s should return the panel it added.")
   
   (preferences:add-editor-checkbox-panel
    (-> void?)
    ()
    "Adds a preferences panel for configuring options"
    "related to editing.")
   (preferences:add-warnings-checkbox-panel
    (-> void?)
    ()
    "Adds a preferences panel for configuring"
    "options relating to warnings")
   (preferences:add-scheme-checkbox-panel
    (-> void?)
    ()
    "Adds a preferences panel for configuring"
    "options related to Scheme.")
   
   (preferences:add-to-warnings-checkbox-panel
    (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
    (proc)
    "Saves \\var{proc} until the preferences panel is"
    "created, when it is called with the Misc. panel to"
    "add new children to the panel.")
   
   (preferences:add-to-scheme-checkbox-panel
    (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
    (proc)
    "Saves \\var{proc} until the preferences panel is "
    "created, when it is called with the Scheme "
    "preferences panel to "
    "add new children to the panel.")
   
   (preferences:add-to-editor-checkbox-panel
    (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
    (proc)
    "Saves \\var{proc} until the preferences panel is "
    "created, when it is called with the Echeme "
    "preferences panel to "
    "add new children to the panel.")

   (preferences:add-font-panel
    (-> void?)
    ()
    "Adds a font selection preferences panel to the preferences dialog.")
   (preferences:show-dialog
    (-> void?)
    ()
    "Shows the preferences dialog.")
   (preferences:hide-dialog
    (-> void?)
    ()
    "Hides the preferences dialog.")
   (preferences:add-on-close-dialog-callback
    ((-> void?) . -> . void?)
    (cb)
    "Registers \\var{cb}. Next time the"
    "user clicks the OK button the preferences"
    "dialog, all of the \\var{cb} functions"
    "are called, assuming that each of the callbacks"
    "passed to"
    "@flink preferences:add-can-close-dialog-callback"
    "succeed.")
   (preferences:add-can-close-dialog-callback
    ((-> boolean?) . -> . void?)
    (cb)
    "Registers \\var{cb}. Next time the"
    "user clicks the OK button the preferences"
    "dialog, all of the \\var{cb} functions"
    "are called. If any of them return \\scm{\\#f},"
    "the dialog is not closed."
    ""
    "See also"
    "@flink preferences:add-on-close-dialog-callback %"
    ".")

   (autosave:register
    ((and/c (is-a?/c autosave:autosavable<%>)
            (is-a?/c editor<%>))
     . -> . 
     void?)
    (obj)
    "Adds \\var{obj} to the list of objects to be autosaved. When it is time"
    "to autosave, the \\rawscm{do-autosave} method of the object is"
    "called. This method is responsible for performing the autosave."
    ""
    "There is no need to"
    "de-register an object because the autosaver keeps a ``weak'' pointer"
    "to the object; i.e., the autosaver does not keep an object from"
    "garbage collection.")
   
   (autosave:restore-autosave-files/gui
    (-> void?)
    ()
    "Opens a GUI to ask the user about recovering any autosave files"
    "left around from crashes and things."
    ""
    "This function doesn't return until the user has finished"
    "restoring the autosave files. (It uses yield to handle events"
    "however).")

   (exit:exiting? 
    (-> boolean?)
    ()
    "Returns \\rawscm{\\#t} to indicate that an exit"
    "operation is taking place. Does not indicate that the"
    "app will actually exit, since the user may cancel"
    "the exit."
    ""
    "See also"
    "@flink exit:insert-on-callback"
    "and"
    "@flink exit:insert-can?-callback %"
    ".")
   (exit:set-exiting
    (boolean? . -> . void?)
    (exiting?)
    "Sets a flag that affects the result of"
    "@flink exit:exiting? %"
    ".")
   (exit:insert-on-callback
    ((-> void?) . -> . (-> void?))
    (callback)
    "Adds a callback to be called when exiting. This callback must not"
    "fail. If a callback should stop an exit from happening, use"
    "@flink exit:insert-can?-callback %"
    ".")
   (exit:insert-can?-callback
    ((-> boolean?) . -> . (-> void?))
    (callback)
    "Use this function to add a callback that determines if an attempted"
    "exit can proceed. This callback should not clean up any state, since"
    "another callback may veto the exit. Use"
    "@flink exit:insert-on-callback"
    "for callbacks that clean up state.")
   (exit:can-exit?
    (-> boolean?)
    ()
    "Calls the ``can-callbacks'' and returns their results."
    "See"
    "@flink exit:insert-can?-callback"
    "for more information.")
   (exit:on-exit
    (-> void?)
    ()
    "Calls the ``on-callbacks''. See"
    "@flink exit:insert-on-callback"
    "for more information.")
   (exit:exit
    (-> any)
    ()
    "\\rawscm{exit:exit} performs four actions:"
    "\\begin{itemize}"
    "\\item sets the result of the"
    "@flink exit:exiting?"
    "function to \\rawscm{\\#t}."
    "\\item invokes the exit-callbacks, with "
    "@flink exit:can-exit? %"
    "If none of the ``can?'' callbacks return \\rawscm{\\#f}, "
    "\\item"
    "invokes"
    "@flink exit:on-exit %"
    "and then "
    "\\item"
    "queues a callback that calls"
    "\\rawscm{exit} (a mzscheme procedure)"
    "and (if \\rawscm{exit} returns) sets the"
    "result of"
    "@flink exit:exiting?"
    "back to \\rawscm{\\#t}."
    "\\end{itemize}")
   (exit:user-oks-exit
    (-> boolean?)
    ()
    "Opens a dialog that queries the user"
    "about exiting. Returns the user's decision.")
   
   
   (path-utils:generate-autosave-name
    (string? . -> . string?)
    (filename)
    "Generates a name for an autosave file from \\var{filename}.")
   (path-utils:generate-backup-name
    (path? . -> . path?)
    (filename)
    "Generates a name for an backup file from \\var{filename}.")
   (finder:dialog-parent-parameter
    (parameter/c (or/c false/c (is-a?/c dialog%) (is-a?/c frame%)))
    ()
    "This is a parameter (see "
    "\\Mzhyperref{parameters}{mz:parameters} for information about parameters)"
    "which determines the parent of the dialogs created by"
    "@flink finder:get-file %"
    ", "
    "@flink finder:put-file %"
    ","
    "@flink finder:common-get-file %"
    ","
    "@flink finder:common-put-file %"
    ","
    "@flink finder:common-get-file-list %"
    ","
    "@flink finder:std-get-file %"
    ", and"
    "@flink finder:std-put-file %"
    ".")
   (finder:default-extension
    (case-> (-> string?)
            (string? . -> . void?))
    (() (extension))
    "This parameter controls the default extension for the framework's "
    "@flink finder:put-file"
    "dialog. Its value gets passed as the"
    "\\var{default-extension} argument to"
    "@flink put-file %"
    "."
    ""
    "Its default value is \\rawscm{\"\"}.")
   (finder:default-filters
    (case->
     ((listof (list/c string? string?)) . -> . void?)
     (-> (listof (list/c string? string?))))
    ((filters) ())
    "This parameter controls the default extension for the framework's "
    "@flink finder:put-file"
    "dialog. Its value gets passed as the"
    "\\var{default-filters} argument to"
    "@flink put-file %"
    "."
    ""
    "Its default value is \\rawscm{'((\"Any\" \"*.*\"))}.")

   (finder:common-put-file
    (opt->
     ()
     (string?
      (or/c false/c path?)
      boolean?
      string?
      (or/c false/c byte-regexp?)
      string?
      (or/c (is-a?/c top-level-window<%>) false/c))
     (or/c false/c path?))
    (()
     ((name "Untitled")
      (directory #f)
      (replace? #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent (finder:dialog-parent-parameter))))
    "This procedure queries the user for a single filename, using a"
    "platform-independent dialog box. Consider using"
    "@flink finder:put-file "
    "instead of this function."
    ""
    "See section \\ref{selecting-a-filename} for more information.")
   (finder:common-get-file
    (opt->
     ()
     ((or/c path? false/c)
      string?
      (or/c byte-regexp? false/c)
      string?
      (or/c false/c (is-a?/c top-level-window<%>)))
     (or/c path? false/c))
    (()
     ((directory #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent #f)))
    "This procedure queries the user for a single filename, using a"
    "platform-independent dialog box. Consider using"
    "@flink finder:get-file "
    "instead of this function."
    ""
    "See section \\ref{selecting-a-filename} for more information.")
   (finder:std-put-file
    (opt->
     ()
     (string?
      (or/c false/c path?)
      boolean?
      string?
      (or/c false/c byte-regexp?)
      string?
      (or/c (is-a?/c top-level-window<%>) false/c))
     (or/c false/c path?))
    (()
     ((name "Untitled")
      (directory #f)
      (replace? #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent (finder:dialog-parent-parameter))))
    "This procedure queries the user for a single filename, using a"
    "platform-dependent dialog box. Consider using"
    "@flink finder:put-file "
    "instead of this function."
    ""
    "See section \\ref{selecting-a-filename} for more information.")
   (finder:std-get-file
    (opt->
     ()
     ((or/c path? false/c)
      string?
      (or/c byte-regexp? false/c)
      string?
      (or/c false/c (is-a?/c top-level-window<%>)))
     (or/c path? false/c))
    (()
     ((directory #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent #f)))
    "This procedure queries the user for a single filename, using a"
    "platform-dependent dialog box. Consider using"
    "@flink finder:get-file "
    "instead of this function."
    ""
    "See section \\ref{selecting-a-filename} for more information.")
   (finder:put-file
    (opt->
     ()
     (string?
      (or/c false/c path?)
      boolean?
      string?
      (or/c false/c byte-regexp?)
      string?
      (or/c (is-a?/c top-level-window<%>) false/c))
     (or/c false/c path?))
    (()
     ((name "Untitled")
      (directory #f)
      (replace? #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent (finder:dialog-parent-parameter))))
    "Queries the user for a filename."
    ""
    "If the result of \\rawscm{(%"
    "@flink preferences:get"
    "'framework:file-dialogs)}"
    "is \\rawscm{'std} this calls "
    "@flink finder:std-put-file %"
    ", and if it is \\rawscm{'common}, "
    "@flink finder:common-put-file"
    "is called.")
   (finder:get-file
    (opt->
     ()
     ((or/c path? false/c)
      string?
      (or/c byte-regexp? string? false/c)
      string?
      (or/c false/c (is-a?/c top-level-window<%>)))
     (or/c path? false/c))
    (()
     ((directory #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent #f)))    
    "Queries the user for a filename."
    ""
    "If the result of \\rawscm{(%"
    "@flink preferences:get"
    "'framework:file-dialogs)}"
    "is \\rawscm{'std} this calls "
    "@flink finder:std-get-file %"
    ", and if it is \\rawscm{'common}, "
    "@flink finder:common-get-file"
    "is called.")
   (finder:common-get-file-list
    (opt->
     ()
     ((or/c false/c path?)
      string?
      (or/c false/c byte-regexp?)
      string?
      (or/c false/c (is-a?/c top-level-window<%>)))
     (or/c (listof path?) false/c))
    (()
     ((directory #f)
      (prompt "Select File")
      (filter #f)
      (filter-msg "That filename does not have the right form.")
      (parent #f)))
    "This procedure queries the user for a list of filenames, using a"
    "platform-independent dialog box."
    ""
    "See section \\ref{selecting-a-filename} for more information.")

   (frame:setup-size-pref
    (symbol? number? number? . -> . void)
    (size-pref-sym width height)
    "Initializes a preference for the"
    "@mixin-link frame:size-pref"
    "mixin."
    ""
    "The first argument should be the preferences symbol, and the second an third"
    "should be the default width and height, respectively.")
   (frame:add-snip-menu-items
    (opt-> ((is-a?/c menu%) (subclass?/c menu-item%))
           ((-> (is-a?/c menu-item%) void?))
           void?)
    ((menu menu-item%) 
     ((func void)))
    "Inserts three menu items into \\var{menu},"
    "one that inserts a text box, one that inserts a"
    "pasteboard box, and one that inserts an image"
    "into the currently focused editor (if there is one)."
    "Uses \\var{menu-item\\%} as the class for"
    "the menu items."
    ""
    "Calls \\var{func} right after inserting each menu item.")
   
   (frame:reorder-menus
    ((is-a?/c frame%) . -> . void?)
    (frame)
    "Re-orders the menus in a frame. It moves the ``File'' and ``Edit'' menus to"
    "the front of the menubar and moves the ``Windows'' and ``Help'' menus"
    "to the end of the menubar."
    ""
    "This is useful in conjunction with the "
    "frame classes. After instantiating the class and adding ones own menus,"
    "the menus will be mis-ordered. This function fixes them up.")
   
   (frame:remove-empty-menus
    ((is-a?/c frame%) . -> . void?)
    (frame)
    "Removes empty menus in a frame."
    ""
    "This is useful if you have code that might leave empty menus.")
   
   (group:get-the-frame-group
    (-> (is-a?/c group:%))
    ()
    "This returns the frame group.")
   
   (group:on-close-action
    (-> void?)
    ()
    "See also "
    "@flink group:can-close-check %"
    "."
    ""
    "Call this function from the"
    "@ilink top-level-window can-close?"
    "callback of a frame"
    "in order for the group to properly close the application.")
   (group:can-close-check
    (-> boolean?)
    ()
    "See also "
    "@flink group:on-close-action %"
    "."
    ""
    "Call this function from the"
    "@ilink top-level-window can-close?"
    "callback of a frame"
    "in order for the group to properly close the application.")
   
   (handler:handler?
    (any/c . -> . boolean?)
    (obj)
    "This predicate determines if its input is a handler")
   (handler:handler-name
    (handler:handler? . -> . string?)
    (handler)
    "Extracts the name from a handler.")
   (handler:handler-extension
    (handler:handler? . -> . (or/c (path? . -> . boolean?) (listof string?)))
    (handler)
    "Extracts the extension from a handler.")
   (handler:handler-handler
    (handler:handler? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
    (handler)
    "Extracs the handler's handling function")
   (handler:insert-format-handler
    (string?
     (or/c string? (listof string?) (path? . -> . boolean?))
     (path? . -> . (or/c false/c (is-a?/c frame:editor<%>)))
     . -> .
     void?)
    (name pred handler)
    "This function inserts a format handler."
    ""
    "The string, \\var{name} names the format handler for use with"
    "@flink handler:find-named-format-handler %"
    ". If \\var{pred} is a string, it is matched with the extension of a filename by"
    "@flink handler:find-format-handler %"
    ". If \\var{pred} is a list of strings, they are each matched with the extension of a filename by"
    "@flink handler:find-format-handler %"
    ". If it is a function, the filename is applied to the function and the"
    "functions result determines if this is the handler to use."
    ""
    "The most recently added format handler takes precedence over all other format handlers.")
   (handler:find-named-format-handler
    (string? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
    (name)
    "This function selects a format handler. See also"
    "@flink handler:insert-format-handler %"
    "."
    ""
    "It finds a handler based on \\var{name}.")
   (handler:find-format-handler
    (path? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
    (filename)
    "This function selects a format handler. See also"
    "@flink handler:insert-format-handler %"
    "."
    ""
    "It finds a handler based on \\var{filename}.")

   (handler:edit-file
    (opt->
     ((or/c path? false/c))
     ((-> (is-a?/c frame:editor<%>)))
     (or/c false/c (is-a?/c frame:editor<%>)))
    ((filename)
     ((make-default (λ () ((handler:current-create-new-window) filename)))))
    "This function creates a frame or re-uses an existing frame to edit a file. "
    ""
    "If the preference \\scheme{'framework:open-here} is set to \\scheme{#t},"
    "and (send ("
    "@flink group:get-the-frame-group %"
    ") "
    "@link group: get-open-here-frame %"
    ") returns a frame, the "
    "@ilink frame:open-here open-here "
    "method of that frame is used to load"
    "the file in the existing frame."
    ""
    "Otherwise, it invokes the appropriate format"
    "handler to open the file (see"
    "@flink handler:insert-format-handler %"
    ")."
    ""
    "\\begin{itemize}"
    "\\item If \\var{filename} is a string, this function checks the result of"
    "@flink group:get-the-frame-group"
    "to see if the \\var{filename} is already open by a frame in the"
    "group. "
    "\\begin{itemize}"
    "\\item If so, it returns the frame. "
    "\\item If not, this function calls "
    "@flink handler:find-format-handler"
    "with \\var{filename}. "
    "\\begin{itemize}"
    "\\item"
    "If a handler is found, it is applied to"
    "\\var{filename} and it's result is the final result. "
    "\\item"
    "If not, \\var{make-default} is used."
    "\\end{itemize}"
    "\\end{itemize}"
    "\\item"
    "If \\var{filename} is \\rawscm{\\#f}, \\var{make-default} is used."
    "\\end{itemize}")

   (handler:current-create-new-window
    (case->
     (((or/c false/c path?) . -> . (is-a?/c frame%)) . -> . void)
     (-> ((or/c false/c string?) . -> . (is-a?/c frame%))))
    ((new-window-handler) ())
    "This is a parameter that controls how the framework"
    "creates new application windows."
    ""
    "The default setting is this:"
    "\\begin{schemedisplay}"
    "(λ (filename)"
    "  (let ([frame (make-object frame:text-info-file% filename)])"
    "    (send frame show #t)"
    "    frame))"
    "\\end{schemedisplay}")

   (handler:open-file
    (-> (or/c false/c (is-a?/c frame:basic<%>)))
    ()
    "This function queries the user for a filename and opens the file for"
    "editing. It uses "
    "@flink handler:edit-file"
    "to open the file, once the user has chosen it."
    ""
    "Calls"
    "@flink finder:get-file"
    "and"
    "@flink handler:edit-file %"
    ".")
   
   (handler:install-recent-items
    ((is-a?/c menu%) . -> . void?)
    (menu)
    "This function deletes all of the items in the given menu and"
    "adds one menu item for each recently opened file. These menu"
    "items, when selected, call"
    "@flink handler:edit-file"
    "with the filename of the recently opened file."
    ""
    "The menu's size is limited to 10.")
   
   (handler:set-recent-items-frame-superclass
    ((implementation?/c frame:standard-menus<%>) . -> . void?)
    (frame)
    "Sets the superclass for the recently opened files frame."
    "It must be derived from"
    "@ilink frame:standard-menus %"
    ".")

   (handler:add-to-recent
    (path? . -> . void?)
    (filename)
    "Adds a filename to the list of recently opened files.")

   (handler:set-recent-position
    (path? number? number? . -> . void?)
    (filename start end)
    "Sets the selection of the recently opened file to"
    "\\var{start} and \\var{end}.")
   
   (handler:size-recently-opened-files
    (number? . -> . void?)
    (num)
    "Sizes the 'framework:recently-opened-files/pos preference"
    "list length to \\var{num}.")

   (icon:get-paren-highlight-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns the parenthesis highlight "
    "@link bitmap %"
    ". It is only used on black and white screens.")
   (icon:get-eof-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns the"
    "@link bitmap %"
    "used for the clickable ``eof'' icon from"
    "@ilink text:ports %"
    ".")
   (icon:get-autowrap-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns the autowrap's "
    "@link bitmap %"
    "."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method.")
   (icon:get-lock-bitmap 
    (-> (is-a?/c bitmap%))
    ()
    "This returns the lock's "
    "@link bitmap %"
    "."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method.")
   (icon:get-unlock-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns the reset unlocked"
    "@link bitmap %"
    "."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method."
    "")
   (icon:get-anchor-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns the anchor's "
    "@link bitmap %"
    "."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method.")
   (icon:get-left/right-cursor
    (-> (is-a?/c cursor%))
    ()
    "This function returns a "
    "@link cursor"
    "object that indicates left/right sizing is possible,"
    "for use with columns inside a window."
    ""
    "The cursor may not respond \\scm{\\#t} to the"
    "@link cursor ok?"
    "method.")
   (icon:get-up/down-cursor
    (-> (is-a?/c cursor%))
    ()
    "This function returns a "
    "@link cursor"
    "object that indicates up/down sizing is possible,"
    "for use with columns inside a window."
    ""
    "The cursor may not respond \\scm{\\#t} to the"
    "@link cursor ok?"
    "method.")
   (icon:get-gc-on-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns a bitmap to be displayed in an"
    "@ilink frame:info"
    "frame when garbage collection is taking place."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method.")
   (icon:get-gc-off-bitmap
    (-> (is-a?/c bitmap%))
    ()
    "This returns a bitmap to be displayed in an"
    "@ilink frame:info"
    "frame when garbage collection is {\\em not\\/} taking place."
    ""
    "The bitmap may not respond \\scm{\\#t} to the"
    "@link bitmap ok?"
    "method.")

   (keymap:remove-user-keybindings-file
    (-> any/c any)
    (user-keybindings-path)
    "Removes the keymap previously added by"
    "@flink keymap:add-user-keybindings-file %"
    ".")
   (keymap:add-user-keybindings-file
    (-> any/c any)
    (user-keybindings-path-or-require-spec)
    "Chains the keymap defined by \\var{user-keybindings-path-or-require-spec} to "
    "the global keymap, returned by "
    "@flink keymap:get-global %"
    "."
    ""
    "If \\var{user-keybindings-path-or-require-spec} is a path, the module is loaded"
    "directly from that path. Otherwise, \\var{user-keybindings-path-or-require-spec}"
    "is treated like an argument to \\scheme|require|.")
   (keymap:add-to-right-button-menu
    (case->
     (((is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) . -> . void?) . -> . void?)
     (-> ((is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) . -> . void?)))
    ((func) ())
    "When the keymap that "
    "@flink keymap:get-global"
    "returns is installed into an editor, this parameter's value"
    "is used for right button clicks. "
    ""
    "Before calling this procedure, the "
    "function"
    "@flink append-editor-operation-menu-items"
    "is called."
    ""
    "See also"
    "@flink keymap:add-to-right-button-menu/before %"
    ".")

   (keymap:add-to-right-button-menu/before
    (case->
     (((is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) . -> . void?)
      . -> .
      void?)
     (-> ((is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) . -> . void?)))
    ((func) ())
    "When the keymap that "
    "@flink keymap:get-global"
    "returns is installed into an editor, this function is called"
    "for right button clicks. "
    ""
    "After calling this procedure, the "
    "function"
    "@flink append-editor-operation-menu-items"
    "is called."
    ""
    "See also"
    "@flink keymap:add-to-right-button-menu %"
    ".")

   (keymap:call/text-keymap-initializer
    ((-> any/c) . -> . any/c)
    (thunk-proc)
    "Thus function parameterizes the call to \\var{thunk-proc} by"
    " setting the keymap-initialization procedure (see"
    "%"
    "@flink current-text-keymap-initializer  %"
    "%"
    ") to install the framework's standard text bindings.")

   (keymap:canonicalize-keybinding-string
    (string? . -> . string?)
    (keybinding-string)
    "Returns a string that denotes the same keybindings as the input"
    "string, except that it is in canonical form; two canonical keybinding"
    "strings can be compared with \\rawscm{string=?}.")

   (keymap:get-editor
    (-> (is-a?/c keymap%))
    ()
    "This returns a keymap for handling standard editing operations.  It"
    "binds these keys:"
    "\\begin{itemize}"
    "\\item {\\bf z}: undo"
    "\\item {\\bf y}: redo"
    "\\item {\\bf x}: cut"
    "\\item {\\bf c}: copy"
    "\\item {\\bf v}: paste"
    "\\item {\\bf a}: select all"
    "\\end{itemize}"
    "where each key is prefixed with the menu-shortcut key, based on the"
    "platform. Under unix, the shortcut is scm{\"a:\"}; under windows the"
    "shortcut key is \\rawscm{\"c:\"} and under MacOS, the shortcut key is"
    "\\rawscm{\"d:\"}.")

   (keymap:get-file
    (-> (is-a?/c keymap%))
    ()
    "This returns a keymap for handling file operations.")

   (keymap:get-global
    (-> (is-a?/c keymap%))
    ()
    "This returns a keymap for general operations. See"
    "@flink keymap:setup-global"
    "for a list of the bindings this keymap contains.")

   (keymap:get-search
    (-> (is-a?/c keymap%))
    ()
    "This returns a keymap for searching operations")

   (keymap:make-meta-prefix-list
    (string? . -> . (listof string?))
    (key)
    "This prefixes a key with all of the different meta prefixes and"
    "returns a list of the prefixed strings."
    ""
    "takes a keymap, a base key specification, and a function name; it"
    "prefixes the base key with all ``meta'' combination prefixes, and"
    "installs the new combinations into the keymap. For example,"
    "\\rawscm{(\\iscmprocedure{keymap:send-map-function-meta} \\var{keymap} \"a\""
    "\\var{func})} maps all of ``m:a'' and ``ESC;a'' to"
    "\\var{func}.")

   (keymap:send-map-function-meta
    ((is-a?/c keymap%) string? string? . -> . void?)
    (keymap key func)
    "\\index{Meta}"
    "Most keyboard and mouse mappings are inserted into a keymap by calling"
    "the keymap's \\rawscm{map-function} method. However, ``meta'' combinations"
    "require special attention. The ``m:'' prefix recognized by"
    "\\rawscm{map-function} applies only to the Meta key that exists on"
    "some keyboards. By convention, however, ``meta'' combinations can also be"
    "accessed by using ``ESC'' as a prefix."
    ""
    "This procedure binds all of the key-bindings obtained by prefixing"
    "\\var{key} with a meta-prefix to \\var{func} in \\var{keymap}.")

   (keymap:setup-editor
    ((is-a?/c keymap%) . -> . void?)
    (keymap)
    "This sets up the input keymap with the bindings described in "
    "@flink keymap:get-editor %"
    ".")

   (keymap:setup-file
    ((is-a?/c keymap%) . -> . void?)
    (keymap)
    "This extends a "
    "@link keymap"
    "with the bindings for files.")

   (keymap:setup-global
    ((is-a?/c keymap%) . -> . void?)
    (keymap)
    "This extends a "
    "@link keymap"
    "with the general bindings."
    ""
    "This function extends a \\iscmclass{keymap} with the following functions:"
    "\\begin{itemize}"
    "\\CloseLines"
    "\\item \\mapdesc{ring-bell}{any} --- Rings the bell (using \\iscmprocedure{bell}) and"
    "removes the search panel from the frame, if there."
    "\\item \\mapdesc{save-file}{key} --- Saves the buffer. If the buffer has "
    "no name, then \\scmfirst{finder:put-file} is invoked."
    "\\item \\mapdesc{save-file-as}{key} --- Calls \\scmfirst{finder:put-file} to save"
    "the buffer."
    "\\item \\mapdesc{load-file}{key} --- Invokes \\scmfirst{finder:open-file}."
    "\\item \\mapdesc{find-string}{key} --- Opens the search buffer at the bottom"
    "of the frame, unless it is already open, in which case it searches for the"
    "text in the search buffer."
    "\\item \\mapdesc{find-string-reverse}{key} --- Same a ``find-string'', but in"
    "the reverse direction."
    "\\item \\mapdesc{find-string-replace}{key} --- Opens a replace string dialog"
    "box. "
    "\\item \\mapdesc{toggle-anchor}{key} --- Turns selection-anchoring on or off."
    "\\item \\mapdesc{center-view-on-line}{key} --- Centers the buffer in its"
    "display using the currently selected line."
    "\\item \\mapdesc{collapse-space}{key} --- Collapses all non-return whitespace"
    "around the caret into a single space."
    "\\item \\mapdesc{remove-space}{key} --- Removes all non-return whitespace"
    "around the caret."
    "\\item \\mapdesc{collapse-newline}{key} --- Collapses all empty lines"
    "around the caret into a single empty line. If there is only"
    "one empty line, it is removed."
    "\\item \\mapdesc{open-line}{key} --- Inserts a new line."
    "\\item \\mapdesc{transpose-chars}{key} --- Transposes the characters before"
    "and after the caret and moves forward one position."
    "\\item \\mapdesc{transpose-words}{key} --- Transposes words before"
    "and after the caret and moves forward one word."
    "\\item \\mapdesc{capitalize-word}{key} --- Changes the first character"
    "of the next word to a capital letter and moves to the end of the"
    "word."
    "\\item \\mapdesc{upcase-word}{key} --- Changes all characters"
    "of the next word to capital letters and moves to the end of the"
    "word."
    "\\item \\mapdesc{downcase-word}{key} --- Changes all characters"
    "of the next word to lowercase letters and moves to the end of the"
    "word."
    "\\item \\mapdesc{kill-word}{key} --- Kills the next word."
    "\\item \\mapdesc{backward-kill-word}{key} --- Kills the previous word."
    "\\item \\mapdesc{goto-line}{any} --- Queries the user for a line number and moves"
    "the caret there."
    "\\item \\mapdesc{goto-position}{any}  --- Queries the user for a position number "
    "and moves the caret there."
    "\\item \\mapdesc{copy-clipboard}{mouse} --- Copies the current selection to the"
    "clipboard."
    "\\item \\mapdesc{cut-clipboard}{mouse} --- Cuts the current selection to the"
    "clipboard."
    "\\item \\mapdesc{paste-clipboard}{mouse} --- Patses the clipboard to the current"
    "selection."
    "\\item \\mapdesc{copy-click-region}{mouse} --- Copies the region between the"
    "caret and the input mouse event."
    "\\item \\mapdesc{cut-click-region}{mouse} --- Cuts the  region between the"
    "caret and the input mouse event."
    "\\item \\mapdesc{paste-click-region}{mouse} --- Pastes the clipboard into the"
    "position of the input mouse event."
    "\\item \\mapdesc{select-click-word}{mouse} --- Selects the word under the"
    "input mouse event."
    "\\item \\mapdesc{select-click-line}{mouse} --- Selects the line under the"
    "input mouse event."
    "\\item \\mapdesc{start-macro}{key} -- Starts building a keyboard macro"
    "\\item \\mapdesc{end-macro}{key} --- Stops building a keyboard macro"
    "\\item \\mapdesc{do-macro}{key} --- Executes the last keyboard macro"
    "\\item \\mapdesc{toggle-overwrite}{key} --- Toggles overwriting mode"
    "\\end{itemize}"
    ""
    "These functions are bound to the following keys (C = control, S ="
    "shift, A = alt, M = ``meta'', D = command):"
    "\\begin{itemize}"
    "\\CloseLines"
    "\\item C-g : ``ring-bell''"
    "\\item M-C-g : ``ring-bell''"
    "\\item C-c C-g : ``ring-bell''"
    "\\item C-x C-g : ``ring-bell''"
    "\\item C-p : ``previous-line''"
    "\\item S-C-p : ``select-previous-line''"
    "\\item C-n : ``next-line''"
    "\\item S-C-n : ``select-next-line''"
    "\\item C-e : ``end-of-line''"
    "\\item S-C-e : ``select-to-end-of-line''"
    "\\item D-RIGHT : ``end-of-line''"
    "\\item S-D-RIGHT : ``select-to-end-of-line''"
    "\\item M-RIGHT : ``end-of-line''"
    "\\item S-M-RIGHT : ``select-to-end-of-line''"
    "\\item C-a : ``beginning-of-line''"
    "\\item S-C-a : ``select-to-beginning-of-line''"
    "\\item D-LEFT : ``beginning-of-line''"
    "\\item D-S-LEFT : ``select-to-beginning-of-line''"
    "\\item M-LEFT : ``beginning-of-line''"
    "\\item M-S-LEFT : ``select-to-beginning-of-line''"
    "\\item C-h : ``delete-previous-character''"
    "\\item C-d : ``delete-next-character''"
    "\\item C-f : ``forward-character''"
    "\\item S-C-f : ``select-forward-character''"
    "\\item C-b : ``backward-character''"
    "\\item S-C-b : ``select-backward-character''"
    "\\item M-f : ``forward-word''"
    "\\item S-M-f : ``select-forward-word''"
    "\\item A-RIGHT : ``forward-word''"
    "\\item A-S-RIGHT : ``forward-select-word''"
    "\\item M-b : ``backward-word''"
    "\\item S-M-b : ``select-backward-word''"
    "\\item A-LEFT : ``backward-word''"
    "\\item A-S-LEFT : ``backward-select-word''"
    "\\item M-d : ``kill-word''"
    "\\item M-DELETE : ``backward-kill-word''"
    "\\item M-c : ``capitalize-word''"
    "\\item M-u : ``upcase-word''"
    "\\item M-l : ``downcase-word''"
    "\\item M-$<$ : ``beginning-of-file''"
    "\\item S-M-$<$ : ``select-to-beginning-of-file''"
    "\\item M-$>$ : ``end-of-file''"
    "\\item S-M-$>$ : ``select-to-end-of-file''"
    "\\item C-v : ``next-page''"
    "\\item S-C-v : ``select-next-page''"
    "\\item M-v : ``previous-page''"
    "\\item S-M-v : ``select-previous-page''"
    "\\item C-l : ``center-view-on-line''"
    "\\item C-k : ``delete-to-end-of-line''"
    "\\item C-y : ``paste-clipboard'' (Except Windows)"
    "\\item A-v : ``paste-clipboard''"
    "\\item D-v : ``paste-clipboard''"
    "\\item C-\\_ : ``undo''"
    "\\item C-x u : ``undo''"
    "\\item C-+ : ``redo''"
    "\\item C-w : ``cut-clipboard''"
    "\\item M-w : ``copy-clipboard''"
    "\\item C-x C-s : ``save-file''"
    "\\item C-x C-w : ``save-file-as''"
    "\\item C-x C-f : ``load-file''"
    "\\item C-s : ``find-string''"
    "\\item C-r : ``find-string-reverse''"
    "\\item M-\\% : ``find-string-replace''"
    "\\item SPACE : ``collapse-space''"
    "\\item M-{\\Backslash} : ``remove-space''"
    "\\item C-x C-o : ``collapse-newline''"
    "\\item C-o : ``open-line''"
    "\\item C-t : ``transpose-chars''"
    "\\item M-t : ``transpose-words''"
    "\\item C-SPACE : ``toggle-anchor''"
    "\\item M-g : ``goto-line''"
    "\\item M-p : ``goto-position''"
    "\\item LEFTBUTTONTRIPLE : ``select-click-line''"
    "\\item LEFTBUTTONDOUBLE : ``select-click-word''"
    "\\item RIGHTBUTTON : ``copy-click-region''"
    "\\item RIGHTBUTTONDOUBLE : ``cut-click-region''"
    "\\item MIDDLEBUTTON : ``paste-click-region''"
    "\\item C-RIGHTBUTTON : ``copy-clipboard''"
    "\\item INSERT : ``toggle-overwrite''"
    "\\item M-o : ``toggle-overwrite''"
    "\\end{itemize}")

   (keymap:setup-search
    ((is-a?/c keymap%) . -> . void?)
    (keymap)
    "This extends a "
    "@link keymap"
    "with the bindings for searching.")

   (keymap:set-chained-keymaps
    ((is-a?/c keymap:aug-keymap<%>)
     (listof (is-a?/c keymap%))
     . -> .
     void?)
    (keymap children-keymaps)
    "Sets \\var{keymap}'s chained keymaps to \\var{children-keymaps},"
    "unchaining any keymaps that are currently chained to \\var{keymap}.")
   
   (keymap:remove-chained-keymap
    ((is-a?/c editor<%>)
     (is-a?/c keymap:aug-keymap<%>)
     . -> . 
     void?)
    (editor keymap)
    "Removes \\var{keymap} from the keymaps chained to \\var{editor}."
    "Also (indirectly) removes all keymaps chained to \\var{keymap} from \\var{editor},"
    "since they are removed when unchaining \\var{keymap} itself."
    ""
    "Each of the keymaps chained to \\var{editor} must be an"
    "@ilink keymap:aug-keymap"
    "and \\var{keymap} cannot be the result of"
    "\\begin{schemedisplay}"
    "(send editor get-keymap)"
    "\\end{schemedisplay}"
    "That is, \\var{keymap} must be chained to some keymap attached"
    "to the editor.")
    
   (scheme:text-balanced?
    (opt->
     ((is-a?/c text%))
     (number? (or/c false/c number?))
     boolean?)
   ((text)
    ((start 0) (end #f)))
   "Determines if the range in the editor from \\var{start} to \\var{end} in \\var{text}"
   "is a matched set of parenthesis. If \\var{end} is \\scheme{#f}, it"
   "defaults to the last position of the \\var{text}."
   ""
   "The implementation of this function creates a port with"
   "@flink open-input-text-editor"
   "and then uses `read' to parse the range of the buffer.")
   
   (scheme:add-preferences-panel
     (-> void?)
     ()
     "Adds a tabbing preferences panel to the preferences dialog.")

    (scheme:get-keymap
     (-> (is-a?/c keymap%))
     ()
     "Returns a keymap with binding suitable for Scheme.")

    (scheme:add-coloring-preferences-panel
     (-> any)
     ()
     "Installs the ``Scheme'' preferences panel in the ``Syntax Coloring''"
     "section.")
    
    (scheme:get-color-prefs-table
     (-> (listof (list/c symbol? (is-a?/c color%))))
     ()
     "Returns a table mapping from symbols (naming the categories that"
     "the online colorer uses for Scheme mode coloring) to their"
     "colors."
     ""
     "These symbols are suitable for input to"
     "@flink scheme:short-sym->pref-name"
     "and"
     "@flink scheme:short-sym->style-name %"
     "."
     ""
     "See also"
     "@flink scheme:get-white-on-black-color-prefs-table %"
     ".")
    
    (scheme:get-white-on-black-color-prefs-table
     (-> (listof (list/c symbol? (is-a?/c color%))))
     ()
     "Returns a table mapping from symbols (naming the categories that"
     "the online colorer uses for Scheme mode coloring) to their"
     "colors when the user chooses the white-on-black mode in the"
     "preferences dialog."
     ""
     "See also"
     "@flink scheme:get-color-prefs-table %"
     ".")
    
    (scheme:short-sym->pref-name
     (symbol? . -> . symbol?)
     (short-sym)
     "Builds the symbol naming the preference from one of the symbols"
     "in the table returned by"
     "@flink scheme:get-color-prefs-table %"
     ".")
    
    (scheme:short-sym->style-name
     (symbol? . -> . string?)
     (short-sym)
     "Builds the symbol naming the editor style from one of the symbols"
     "in the table returned by"
     "@flink scheme:get-color-prefs-table %"
     ". This style is a named style in the style list"
     "returned by"
     "@flink editor:get-standard-style-list %"
     ".")
    
    (editor:set-default-font-color
     (-> (is-a?/c color%) void?)
     (color)
     "Sets the color of the style named"
     "@flink editor:get-default-color-style-name %"
     ".")
    (editor:get-default-color-style-name
     (-> string?)
     ()
     "The name of the style (in the list returned by "
     "@flink editor:get-standard-style-list %"
     ") that holds the default color.")
    (editor:set-standard-style-list-delta 
     (string? (is-a?/c style-delta%) . -> . void?)
     (name delta)
     "Finds (or creates) the style named by \\var{name} in"
     "the result of "
     "@flink editor:get-standard-style-list"
     "and sets its delta to \\var{delta}."
     ""
     "If the style named by \\var{name} is already in"
     "the style list, it must be a delta style.")
    
    (editor:set-standard-style-list-pref-callbacks
     (-> any)
     ()
     "Installs the font preference callbacks that"
     "update the style list returned by"
     "@flink editor:get-standard-style-list"
     "based on the font preference symbols.")
    
    (editor:get-standard-style-list
     (-> (is-a?/c style-list%))
     ()
     "Returns a style list that is used for all instances of \\iscmintf{editor:standard-style-list}.")

    (scheme:get-wordbreak-map
     (-> (is-a?/c editor-wordbreak-map%))
     ()
     "This method returns a"
     "@link editor-wordbreak-map"
     "that is suitable for Scheme.")

    (scheme:init-wordbreak-map
     ((is-a?/c keymap%) . -> . void?)
     (key)
     "Initializes the workdbreak map for \\var{keymap}.")

    (scheme:setup-keymap
     ((is-a?/c keymap%) . -> . void?)
     (keymap)
     "Initializes \\var{keymap} with Scheme-mode keybindings.")

    (color-model:rgb->xyz
     (number? number? number? . -> . color-model:xyz?)
     (r g b)
     "Converts a color represented as a red-green-blue tuple (each value"
     "from 0 to 255) into an XYZ tuple. This describes a point in the"
     "CIE XYZ color space.")

    (color-model:rgb-color-distance
     (number? number? number? number? number? number? . -> . number?)
     (red-a green-a blue-a red-b green-b blue-b)
     "This calculates a distance between two colors. The smaller the"
     "distance, the closer the colors should appear to the human eye. A"
     "distance of 10 is reasonably close that it could be called the same"
     "color."
     ""
     "This function is not symmetric in red, green, and blue, so it is"
     "important to pass red, green, and blue components of the colors in the"
     "the proper order. The first three arguments are red, green and blue"
     "for the first color, respectively, and the second three arguments are"
     "red green and blue for the second color, respectively.")

    (color-model:xyz->rgb
     (number? number? number? . -> . (list/c number? number? number?))
     (x y z)
     "Converts an XYZ-tuple (in the CIE XYZ colorspace) into a list of"
     "values representing an RGB-tuple.")

    (color-model:xyz?
     (any/c . -> . boolean?)
     (val)
     "Determines if \\var{val} an xyz color record.")

    (color-model:xyz-x
     (color-model:xyz? . -> . number?)
     (xyz)
     "Extracts the x component of \\var{xyz}.")
    (color-model:xyz-y
     (color-model:xyz? . -> . number?)
     (xyz)
     "Extracts the y component of \\var{xyz}.")
    (color-model:xyz-z
     (color-model:xyz? . -> . number?)
     (xyz)
     "Extracts the z component of \\var{xyz}.")
    
    (color-prefs:set-default/color-scheme
     (-> symbol?
         (or/c (is-a?/c color%) string?) 
         (or/c (is-a?/c color%) string?)
         void?)
     (pref-sym black-on-white-color white-on-black-color)
     "Registers a preference whose value will be updated"
     "when the user clicks on one of the color scheme default"
     "settings in the preferences dialog."
     ""
     "Also calls "
     "@flink preferences:set-default"
     "and"
     "@flink preferences:set-un/marshall"
     "with appropriate arguments to register the preference.")
    
    (color-prefs:register-color-preference 
     (opt->
      (symbol? string? (or/c (is-a?/c color%) (is-a?/c style-delta%)))
      ((or/c string? (is-a?/c color%) false/c))
      void?)
     ((pref-name style-name color/sd)
      ((white-on-black-color #f)))
     "This function registers a color preference and initializes the"
     "style list returned from"
     "@flink editor:get-standard-style-list %"
     ". In particular, it calls "
     "@flink preferences:set-default "
     "and "
     "@flink preferences:set-un/marshall "
     "to install the pref for \\var{pref-name}, using"
     "\\var{color/sd} as the default color. The preference"
     "is bound to a \\iscmclass{style-delta}, and initially the \\iscmclass{style-delta}"
     "changes the foreground color to \\var{color/sd}, unless \\var{color/sd} is a style"
     "delta already, in which case it is just used directly."
     "Then, it calls "
     "@flink editor:set-standard-style-list-delta"
     "passing the \\var{style-name} and the current value"
     "of the preference \\var{pref-name}."
     ""
     "Finally, it adds calls"
     "@flink preferences:add-callback "
     "to set a callback for \\var{pref-name} that"
     "updates the style list when the preference changes."
     ""
     "If \\var{white-on-black-color} is not \\scheme|#f|, then the color of the"
     "\\var{color/sd} argument is used in combination with \\var{white-on-black-color}"
     "to register this preference with"
     "@flink color-prefs:set-default/color-scheme %"
     ".")
      
    (color-prefs:add-background-preferences-panel
     (-> void?)
     ()
     "Adds a preferences panel that configures the background"
     "color for"
     "@mixin-link editor:basic-mixin %"
     ".")
    (color-prefs:add-to-preferences-panel
     (string? ((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
     (name func)
     "Calls \\var{func} with the subpanel of the preferences coloring panel that"
     "corresponds to \\var{name}.")
    
    (color-prefs:build-color-selection-panel
     ((is-a?/c area-container<%>) symbol? string? string? . -> . void?)
     (parent pref-sym style-name example-text)
     "Builds a panel with a number of controls for configuring"
     "a font: the color and check boxes for bold, italic, and underline."
     "The \\var{parent} argument specifies where the panel will be"
     "placed. The \\var{pref-sym} should be a preference (suitable for"
     "use with"
     "@flink preferences:get "
     "and"
     "@flink preferences:set %"
     "). The \\var{style-name} specifies the name of a style in the"
     "style list returned from"
     "@flink editor:get-standard-style-list"
     "and \\var{example-text} is shown in the panel so users can see"
     "the results of their configuration.")

    (color-prefs:marshall-style-delta
     (-> (is-a?/c style-delta%) printable/c)
     (style-delta)
     "Builds a printed representation for a style-delta.")
    
    (color-prefs:unmarshall-style-delta
     (-> printable/c (or/c false/c (is-a?/c style-delta%)))
     (marshalled-style-delta)
     "Builds a style delta from its printed representation."
     "Returns \\scheme|#f| if the printed form cannot be parsed.")
    
    ))
