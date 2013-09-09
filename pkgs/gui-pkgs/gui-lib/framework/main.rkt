#lang at-exp racket/base

(require racket/contract/base
         racket/unit
         racket/class
         racket/gui/base
         racket/set
         mred/mred-unit
         framework/framework-unit
         framework/private/sig
         (for-syntax scheme/base)
         scribble/srcdoc
         (for-syntax "private/scheme.rkt")) 

;; these next two lines do a little dance to make the
;; require/doc setup work out properly
(require (prefix-in :: framework/private/focus-table))
(define frame:lookup-focus-table ::frame:lookup-focus-table)

(require framework/preferences
         framework/test
         framework/gui-utils
         framework/decorated-editor-snip
         framework/private/decorated-editor-snip)

(provide (all-from-out framework/preferences
                       framework/test
                       framework/gui-utils
                       framework/decorated-editor-snip
                       framework/private/decorated-editor-snip))

(require (for-doc racket/base scribble/manual framework/private/mapdesc
                  setup/getinfo racket/pretty string-constants))

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
 (prefix racket: framework:racket-class^)
 (prefix main: framework:main-class^))

(define-values/invoke-unit/infer
  (export framework^)
  (link standard-mred@ framework@))

(provide

 (proc-doc
  color:get-parenthesis-colors-table 
  (-> (listof (list/c symbol? string? (vectorof (is-a?/c color%)) (or/c 'low 'high))))
  @{Returns a table of colors that get used for parenthesis highlighting.
    Each entry in the table consists of a symbolic name, a name to show
    in a GUI, the color to use, and the @racket[_priority] argument to
    pass to @racket[text:basic<%> highlight-range] when highlighting the parens.
    Generally the priority should be @racket['low] if the color is solid
    (α=1) but can be @racket['high] if the α component is small.
    
    When an entry in the table has multiple colors, they are used to show the nesting
    structure in the parentheses.})
 
 (thing-doc
  color:misspelled-text-color-style-name
  string?
  @{The name of the style used to color misspelled words. See also 
    @method[color:text<%> get-spell-check-strings].})
 
 (proc-doc/names
  text:range? (-> any/c boolean?) (arg)
  @{Determines if @racket[arg] is an instance of the @tt{range} struct.})

 (proc-doc/names
  text:range-start
  (-> text:range? exact-nonnegative-integer?)
  (range)
  @{Returns the start position of the range.})

 (proc-doc/names
  text:range-end
  (-> text:range? exact-nonnegative-integer?)
  (range)
  @{Returns the end position of the range.})

 (proc-doc/names
  text:range-caret-space?
  (-> text:range? boolean?)
  (range)
  @{Returns a boolean indicating where the caret-space in the range goes.
    See also @method[text:basic<%> highlight-range].})

 (proc-doc/names
  text:range-style
  (-> text:range? exact-nonnegative-integer?)
  (range)
  @{Returns the style of the range.
    See also @method[text:basic<%> highlight-range].})

 (proc-doc/names
  text:range-color
  (-> text:range? (or/c string? (is-a?/c color%)))
  (range)
  @{Returns the color of the highlighted range.})

 (parameter-doc
  text:autocomplete-append-after
  (parameter/c string?)
  suffix
  @{A string that is inserted after a completion is inserted by a
    @racket[text:autocomplete] instance.
    
    Defaults to @racket[""].})

 (parameter-doc
  text:autocomplete-limit
  (parameter/c (and/c integer? exact? positive?))
  count
  @{Controls the number of completions visible at a time in the menu produced
    by @racket[text:autocomplete] instances.
    
    Defaults to 15.})

 (proc-doc/names
  text:get-completions/manuals
  (-> (or/c false/c (listof symbol?)) (listof string?))
  (manuals)
  @{Returns the list of keywords for the manuals from @racket[manuals] by
    extracting all of the documented exports of the manuals.  The symbols are
    meant to be module paths, e.g., the quoted form of the argument to
    @racket[require].

    If @racket[manuals] is false, then all of the documented names are used.})

 (proc-doc/names
  text:lookup-port-name
  (-> symbol? (or/c (is-a?/c editor:basic<%>) false/c))
  (manuals)
  @{Returns the editor instance whose port-name matches the given symbol.
    If no editor can be found, then returns @racket[false].})

 (proc-doc/names
  number-snip:make-repeating-decimal-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix?)
  @{Makes a number snip that shows the decimal expansion for @racket[number].
    The boolean indicates if a @litchar{#e} prefix appears on the number.
    
    See also @racket[number-snip:make-fraction-snip].})

 (proc-doc/names
  number-snip:make-fraction-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix-in-decimal-view?)
  @{Makes a number snip that shows a fractional view of @racket[number].
    The boolean indicates if a @litchar{#e} prefix appears on the number, when
    shown in the decimal state.
    
    See also @racket[number-snip:make-repeating-decimal-snip].})
 
 (thing-doc
  comment-box:snipclass
  (is-a?/c snip-class%)
  @{The @racket[snip-class%] object used by @racket[comment-box:snip%].})

 (proc-doc/names
  version:add-spec
  (any/c any/c . -> . void?)
  (spec revision)
  @{The two values are appended to the version string.  @racket[write] is used
    to transform them to strings.  For example:
    
    @racket[(version:add-spec 's 1)]
    
    in version 205 will make the version string be @litchar{205s1}.  The
    symbols @racket['f] and @racket['d] were used internally for framework and
    drscheme revisions in the past.})

 (proc-doc/names
  version:version
  (-> string?)
  ()
  @{This function returns a string describing the version of this application.
    See also @racket[version:add-spec].})

 (parameter-doc
  application:current-app-name
  (parameter/c string?)
  name
  @{This is a parameter specifying the name of the current application.  It is
    used in the help menu (see @racket[frame:standard-menus%]) and in frame
    titles (see @racket[frame:editor%]).  The first case in the case-lambda
    returns the current name, and the second case in the case-lambda sets the
    name of the application to @racket[name].})

 (proc-doc/names
  preferences:put-preferences/gui
  (-> (listof symbol?)
      (listof any/c)
      any)
  (name-list val-list)
  @{Like @racket[put-preferences], but has more sophisticated error handling.
    In particular, when it fails to grab a lock, it
    @itemize[
      @item{waits for three consecutive failures before informing the user}
      @item{gives the user the opportunity to ``steal'' the lockfile after the
            third failure, and}
      @item{when lock failures occur, it remembers what its arguments were and if
            any preference save eventually succeeds, all of the past failures
            are also written at that point.}]
    
    In addition when an error is raised trying to save a preference to the preference
    file, @racket[preferences:put-preferences/gui] logs the error using @racket[log-warning],
    instead of raising an exception.
    })

 (proc-doc/names
  preferences:get-preference/gui
  (->* (symbol?)
       ((-> void?))
       any/c)
  ((sym)
   ((default (λ () (error 'get-preference/gui "unknown pref ~s" sym)))))
  @{Like @racket[get-preference], but has more sophisticated error handling.
    In particular, it passes a @racket[#:timeout-lock-there] argument that
    informs the user that the preferences file is locked (and offers the alternative
    of not showing the message again).})

 
 (proc-doc/names
  preferences:add-panel
  (-> (or/c string? (cons/c string? (listof string?)))
      (->i ([parent (is-a?/c area-container-window<%>)])
           ()
           [_ (parent)
              (let ([old-children (send parent get-children)])
                (and/c (is-a?/c area-container-window<%>)
                       (λ (child)
                         (andmap eq?
                                 (append old-children (list child))
                                 (send parent get-children)))))])
      void?)
  (labels f)
  @{@racket[preferences:add-preference-panel] adds the result of @racket[f]
    with name @racket[labels] to the preferences dialog box.
    
    The labels determine where this preference panel is placed in the dialog.
    If the list is just one string, the preferences panel is placed at the top
    level of the dialog.  If there are more strings, a hierarchy of nested
    panels is created and the new panel is added at the end.  If multiple calls
    to @racket[preferences:add-preference-panel] pass the same prefix of
    strings, those panels are placed in the same children.
    
    When the preference dialog is opened for the first time, the function
    @racket[f] is called with a panel, and @racket[f] is expected to add a new
    child panel to it and add whatever preferences configuration controls it
    wants to that panel.  Then, @racket[f]'s should return the panel it added.})

 (proc-doc/names
  preferences:add-editor-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options related to editing.})

 (proc-doc/names
  preferences:add-general-checkbox-panel
  (-> void?)
  ()
  @{Adds a catch-all preferences panel for options.})

 (proc-doc/names
  preferences:add-warnings-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options relating to warnings.})

 (proc-doc/names
  preferences:add-scheme-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options related to Racket.})

 (proc-doc/names
  preferences:add-to-warnings-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @racket[proc] until the preferences panel is created, when it is
    called with the Misc. panel to add new children to the panel.})

 (proc-doc/names
  preferences:add-to-scheme-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @racket[proc] until the preferences panel is created, when it is
    called with the Racket preferences panel to add new children to the
    panel.})

 (proc-doc/names
  preferences:add-to-editor-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @racket[proc] until the preferences panel is created, when it is
    called with the editor preferences panel to add new children to the
    panel.})

 (proc-doc/names
  preferences:add-to-general-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @racket[proc] until the preferences panel is created, when it is
    called with the general preferences panel to add new children to the
    panel.})

 (proc-doc/names
  preferences:add-font-panel
  (-> void?)
  ()
  @{Adds a font selection preferences panel to the preferences dialog.})

 (proc-doc/names
  preferences:show-dialog
  (-> void?)
  ()
  @{Shows the preferences dialog.})

 (proc-doc/names
  preferences:hide-dialog
  (-> void?)
  ()
  @{Hides the preferences dialog.})

 (proc-doc/names
  preferences:add-on-close-dialog-callback
  ((-> void?) . -> . void?)
  (cb)
  @{Registers @racket[cb].  Next time the user clicks the OK button the
    preferences dialog, all of the @racket[cb] functions are called, assuming
    that each of the callbacks passed to
    @racket[preferences:add-can-close-dialog-callback] succeed.})

 (proc-doc/names
  preferences:add-can-close-dialog-callback
  ((-> boolean?) . -> . void?)
  (cb)
  @{Registers @racket[cb].  Next time the user clicks the OK button the
    preferences dialog, all of the @racket[cb] functions are called.  If any of
    them return @racket[#f], the dialog is not closed.
    
    See also @racket[preferences:add-on-close-dialog-callback].})
 
 (proc-doc/names
  preferences:add-check
  (->* ((is-a?/c area-container<%>) symbol? string?)
       ((-> boolean? any/c)
        (-> any/c boolean?))
       void?)
  ((parent pref-key label) ((from-boolean values) (to-boolean values)))
  @{Adds a @racket[radio-box%] object (with @racket[label] as its label)
    to @racket[parent] that, when checked
    adjusts the preference with the key @racket[pref-key].
    
    The @racket[to-boolean] and @racket[from-boolean] functions
    are used to convert from the preferences value to a booleans
    when checking/unchecking the @racket[radio-box%] object. 
    The defaults amount to treating the preference as a boolean such
    that checking the @racket[radio-box%] sets the preference to
    @racket[#t] and unchecking it sets the preference to @racket[#f].
    })

 (proc-doc/names
  autosave:register
  ((and/c (is-a?/c autosave:autosavable<%>)
          (is-a?/c editor<%>))
   . -> .
   void?)
  (obj)
  @{Adds @racket[obj] to the list of objects to be autosaved.  When it is time
    to autosave, the @racket[do-autosave] method of the object is called.  This
    method is responsible for performing the autosave.
    
    There is no need to de-register an object because the autosaver keeps a
    ``weak'' pointer to the object; i.e., the autosaver does not keep an object
    from garbage collection.})

 (thing-doc
  autosave:toc-path
  path?
  @{The path to the a table-of-contents file for the autosave files that DrRacket has created.})
 
 (proc-doc/names
  autosave:restore-autosave-files/gui
  (-> void?)
  ()
  @{Opens a GUI to ask the user about recovering any autosave files left around
    from crashes and things.
    
    This function doesn't return until the user has finished restoring the
    autosave files.  (It uses yield to handle events however.)})

 (proc-doc/names
  exit:exiting?
  (-> boolean?)
  ()
  @{Returns @racket[#t] to indicate that an exit operation is taking place.
    Does not indicate that the app will actually exit, since the user may
    cancel the exit.
    
    See also @racket[exit:insert-on-callback] and
    @racket[exit:insert-can?-callback].})

 (proc-doc/names
  exit:set-exiting
  (boolean? . -> . void?)
  (exiting?)
  @{Sets a flag that affects the result of @racket[exit:exiting?].})

 (proc-doc/names
  exit:insert-on-callback
  ((-> void?) . -> . (-> void?))
  (callback)
  @{Adds a callback to be called when exiting.  This callback must not fail.
    If a callback should stop an exit from happening, use
    @racket[exit:insert-can?-callback].})

 (proc-doc/names
  exit:insert-can?-callback
  ((-> boolean?) . -> . (-> void?))
  (callback)
  @{Use this function to add a callback that determines if an attempted exit
    can proceed.  This callback should not clean up any state, since another
    callback may veto the exit.  Use @racket[exit:insert-on-callback] for
    callbacks that clean up state.})

 (proc-doc/names
  exit:can-exit?
  (-> boolean?)
  ()
  @{Calls the ``can-callbacks'' and returns their results.  See
    @racket[exit:insert-can?-callback] for more information.})

 (proc-doc/names
  exit:on-exit
  (-> void?)
  ()
  @{Calls the ``on-callbacks''.  See @racket[exit:insert-on-callback] for more
    information.})

 (proc-doc/names
  exit:exit
  (-> any)
  ()
  @{@racket[exit:exit] performs four actions:
    @itemize[
      @item{sets the result of the @racket[exit:exiting?] function to
            @racket[#t].}
      @item{invokes the exit-callbacks, with @racket[exit:can-exit?] if none of
            the ``can?'' callbacks return @racket[#f],}
      @item{invokes @racket[exit:on-exit] and then}
      @item{queues a callback that calls @racket[exit] (a racket procedure)
            and (if @racket[exit] returns) sets the result of
            @racket[exit:exiting?] back to @racket[#f].}]})

 (proc-doc/names
  exit:user-oks-exit
  (-> boolean?)
  ()
  @{Opens a dialog that queries the user about exiting.  Returns the user's
    decision.})

 (proc-doc/names
  path-utils:generate-autosave-name
  (string? . -> . string?)
  (filename)
  @{Generates a name for an autosave file from @racket[filename].})

 (proc-doc/names
  path-utils:generate-backup-name
  (path? . -> . path?)
  (filename)
  @{Generates a name for an backup file from @racket[filename].})

 (parameter-doc
  finder:dialog-parent-parameter
  (parameter/c (or/c false/c (is-a?/c dialog%) (is-a?/c frame%)))
  parent
  @{This parameter determines the parent of the dialogs created by
    @racket[finder:get-file], @racket[finder:put-file],
    @racket[finder:common-get-file], @racket[finder:common-put-file],
    @racket[finder:common-get-file-list], @racket[finder:std-get-file],
    and @racket[finder:std-put-file].})

 (parameter-doc
  finder:default-extension
  (parameter/c string?)
  extension
  @{This parameter controls the default extension for the framework's
    @racket[finder:put-file] and @racket[finder:get-file] dialog.  Its value gets passed as the
    @racket[_extension] argument to @racket[put-file] and @racket[get-file].
    
    Its default value is @racket[""].})

 (parameter-doc
  finder:default-filters
  (parameter/c (listof (list/c string? string?)))
  filters
  @{This parameter controls the default filters for the framework's
    @racket[finder:put-file] dialog.  Its value gets passed as the
    @racket[default-filters] argument to @racket[put-file].
    
    Its default value is @racket['(("Any" "*.*"))].})

 (proc-doc/names
  finder:common-put-file
  (->* ()
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
  @{This procedure queries the user for a single filename, using a
    platform-independent dialog box.  Consider using @racket[finder:put-file]
    instead of this function.})

 (proc-doc/names
  finder:common-get-file
  (->* ()
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
  @{This procedure queries the user for a single filename, using a
    platform-independent dialog box.  Consider using
    @racket[finder:get-file] instead of this function.})

 (proc-doc/names
  finder:std-put-file
  (->* ()
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
  @{This procedure queries the user for a single filename, using a
    platform-dependent dialog box.  Consider using @racket[finder:put-file]
    instead of this function.})

 (proc-doc/names
  finder:std-get-file
  (->* ()
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
  @{This procedure queries the user for a single filename, using a
    platform-dependent dialog box.  Consider using @racket[finder:get-file]
    instead of this function.})

 (proc-doc/names
  finder:put-file
  (->* ()
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
  @{Queries the user for a filename.
    
    If the result of @racket[(preferences:get 'framework:file-dialogs)] is
    @racket['std] this calls @racket[finder:std-put-file], and if it is
    @racket['common], @racket[finder:common-put-file] is called.})

 (proc-doc/names
  finder:get-file
  (->* ()
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
  @{Queries the user for a filename.
    
    If the result of @racket[(preferences:get 'framework:file-dialogs)] is
    @racket['std] this calls @racket[finder:std-get-file], and if it is
    @racket['common], @racket[finder:common-get-file] is called.})

 (proc-doc/names
  finder:common-get-file-list
  (->* ()
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
  @{This procedure queries the user for a list of filenames, using a
    platform-independent dialog box.})

 (proc-doc/names
  frame:setup-size-pref
  (->* (symbol? number? number?) 
       (#:maximized? 
        boolean?
        #:position-preferences 
        (or/c #f symbol?))
       void?)
  ((size-pref-sym width height)
   ((maximized? #f)
    (position-preferences-sym #f)))
  @{Initializes a preference for the @racket[frame:size-pref] mixin.
    
    The first argument should be the preferences symbol, and the second and
    third should be the default width and height, respectively. If the
    window should be maximized by default, pass @racket[#t] for the
    @racket[maximized?] argument.
    
    If @racket[position-preferences-sym] is passed, then that symbol will be
    used to track the position of the window.
    })

 (proc-doc/names
  frame:add-snip-menu-items
  (->* ((is-a?/c menu%) (subclass?/c menu-item%))
       ((-> (is-a?/c menu-item%) void?))
       void?)
  ((menu menu-item%)
   ((func void)))
  @{Inserts three menu items into @racket[menu], one that inserts a text box,
    one that inserts a pasteboard box, and one that inserts an image into the
    currently focused editor (if there is one).  Uses @racket[menu-item%] as
    the class for the menu items.
    
    Calls @racket[func] right after inserting each menu item.})

 (proc-doc/names
  frame:reorder-menus
  (-> (is-a?/c frame%) void?)
  (frame)
  @{Re-orders the menus in a frame.  It moves the ``File'' and ``Edit'' menus
    to the front of the menubar and moves the ``Windows'' and ``Help'' menus to
    the end of the menubar.
    
    This is useful in conjunction with the frame classes.  After instantiating
    the class and adding ones own menus, the menus will be mis-ordered.  This
    function fixes them up.})

 (proc-doc/names
  frame:remove-empty-menus
  ((is-a?/c frame%) . -> . void?)
  (frame)
  @{Removes empty menus in a frame.})

 (parameter-doc
  frame:current-icon
  (parameter/c (or/c #f
                     (is-a?/c bitmap%)
                     (cons/c (is-a?/c bitmap%)
                             (is-a?/c bitmap%))))
  icon-spec
  @{The value of this parameter is used by the initialization code of
    @racket[frame:basic-mixin].
    @itemize[
      @item{If it is @racket[#f], then its value is ignored.}
      @item{If it is a @racket[bitmap%], then the @method[top-level-window<%> set-icon] is
            called with the bitmap, the result of invoking the
            @racket[bitmap% get-loaded-mask] method, and @racket['both].}
      @item{If it is a pair of bitmaps, then the @method[top-level-window<%> set-icon]
            method is invoked twice, once with each bitmap in the pair. The
            first bitmap is passed (along with the result of its
            @racket[bitmap% get-loaded-mask]) and @racket['small], and then the
            second bitmap is passed (also along with the result of its
            @racket[bitmap% get-loaded-mask]) and @racket['large].}]
    
    Defaults to @racket[#f].})
 
 (proc-doc/names
  frame:lookup-focus-table
  (->* () (eventspace?) (listof (is-a?/c frame:focus-table<%>)))
  (()
   ((eventspace (current-eventspace))))
  @{Returns a list of the frames in @racket[eventspace], where the first element of the list
    is the frame with the focus.
    
    The order and contents of the list are maintained by
    the methods in @racket[frame:focus-table-mixin], meaning that the
    OS-level callbacks that track the focus of individual frames is 
    ignored.
    
    See also @racket[test:use-focus-table] and @racket[test:get-active-top-level-window].
    
    })
 
 (proc-doc/names
  group:get-the-frame-group
  (-> (is-a?/c group:%))
  ()
  @{This returns the frame group.})

 (proc-doc/names
  group:on-close-action
  (-> void?)
  ()
  @{See also @racket[group:can-close-check].
    
    Call this function from the @method[top-level-window<%> can-close?]
    callback of a frame in order for the group to properly close the
    application.})

 (proc-doc/names
  group:can-close-check
  (-> boolean?)
  ()
  @{See also @racket[group:on-close-action].
    
    Call this function from the @method[top-level-window<%> can-close?]
    callback of a frame in order for the group to properly close the
    application.})

 (proc-doc/names
  group:add-to-windows-menu
  (-> (-> (is-a?/c menu%) any) any)
  (proc)
  @{Procedures passed to this function are called when the @onscreen{Windows}
    menu is created. Use it to add additional menu items.})

  (proc-doc/names
   group:create-windows-menu
   (-> (is-a?/c menu-item-container<%>) (is-a?/c menu%))
   (mb)
   @{Creates a windows menu, registers it (internally) with
     the frame group (see @racket[(get-the-frame-group)]), and
     returns it.})
 
 (proc-doc/names
  handler:handler?
  (any/c . -> . boolean?)
  (obj)
  @{This predicate determines if its input is a handler.})

 (proc-doc/names
  handler:handler-name
  (handler:handler? . -> . string?)
  (handler)
  @{Extracts the name from a handler.})

 (proc-doc/names
  handler:handler-extension
  (handler:handler?
   . -> . (or/c (path? . -> . boolean?) (listof string?)))
  (handler)
  @{Extracts the extension from a handler.})

 (proc-doc/names
  handler:handler-handler
  (handler:handler? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
  (handler)
  @{Extracts the handler's handling function.})

 (proc-doc/names
  handler:insert-format-handler
  (string?
   (or/c string? (listof string?) (path? . -> . boolean?))
   (path? . -> . (or/c false/c (is-a?/c frame:editor<%>)))
   . -> .
   void?)
  (name pred handler)
  @{This function inserts a format handler.
    
    The string, @racket[name] names the format handler for use with
    @racket[handler:find-named-format-handler].  If @racket[pred] is a string,
    it is matched with the extension of a filename by
    @racket[handler:find-format-handler].  If @racket[pred] is a list of
    strings, they are each matched with the extension of a filename by
    @racket[handler:find-format-handler].  If it is a function, the filename is
    applied to the function and the functions result determines if this is the
    handler to use.
    
    The most recently added format handler takes precedence over all other
    format handlers.})

 (proc-doc/names
  handler:find-named-format-handler
  (string? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
  (name)
  @{This function selects a format handler.  See also
    @racket[handler:insert-format-handler].
    
    It finds a handler based on @racket[name].})

 (proc-doc/names
  handler:find-format-handler
  (path? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
  (filename)
  @{This function selects a format handler.  See also
    @racket[handler:insert-format-handler].
    
    It finds a handler based on @racket[filename].})

 (proc-doc/names
  handler:edit-file
  (->* ((or/c path? false/c))
       ((-> (is-a?/c frame:editor<%>)))
       (or/c false/c (is-a?/c frame:editor<%>)))
  ((filename)
   ((make-default
     (λ () ((handler:current-create-new-window) filename)))))
  @{This function invokes the appropriate format handler to open the file (see
    @racket[handler:insert-format-handler]).
    
    @itemize[
      @item{If @racket[filename] is a string, this function checks the result
            of @racket[group:get-the-frame-group] to see if the
            @racket[filename] is already open by a frame in the group.
            @itemize[
              @item{If so, it returns the frame.}
                   @item{If not, this function calls
                         @racket[handler:find-format-handler] with
                         @racket[filename].
                         @itemize[
                           @item{If a handler is found, it is applied to
                                 @racket[filename] and its result is the final
                                 result.}
                           @item{If not, @racket[make-default] is used.}]}]}
      @item{If @racket[filename] is @racket[#f], @racket[make-default] is
            used.}]})

 (parameter-doc
  handler:current-create-new-window
  (parameter/c (-> (or/c false/c path?) (is-a?/c frame%)))
  proc
  @{This is a parameter that controls how the framework creates new application
    windows.
    
    The default setting is this:
    @racketblock[(λ (filename)
                   (let ([frame (make-object frame:text-info-file% filename)])
                     (send frame show #t)
                     frame))]})

 (proc-doc/names
  handler:open-file
  (->* ()
       ((or/c false/c path? string?))
       (or/c false/c (is-a?/c frame:basic<%>)))
  (()
   ((dir #f)))
  @{This function queries the user for a filename and opens the file for
    editing.  It uses @racket[handler:edit-file] to open the file, once the
    user has chosen it.
    
    Calls @racket[finder:get-file] and @racket[handler:edit-file], passing
    along @racket[dir].})

 (proc-doc/names
  handler:install-recent-items
  ((is-a?/c menu%) . -> . void?)
  (menu)
  @{This function deletes all of the items in the given menu and adds one menu
    item for each recently opened file.  These menu items, when selected, call
    @racket[handler:edit-file] with the filename of the recently opened file.
    
    The menu's size is limited to 10.})

 (proc-doc/names
  handler:set-recent-items-frame-superclass
  ((implementation?/c frame:standard-menus<%>) . -> . void?)
  (frame)
  @{Sets the superclass for the recently opened files frame.  It must be
    derived from @racket[frame:standard-menus].})

 (proc-doc/names
  handler:add-to-recent
  (path? . -> . void?)
  (filename)
  @{Adds a filename to the list of recently opened files.})

 (proc-doc/names
  handler:set-recent-position
  (path? number? number? . -> . void?)
  (filename start end)
  @{Sets the selection of the recently opened file to @racket[start] and
    @racket[end].})

 (proc-doc/names
  handler:size-recently-opened-files
  (number? . -> . void?)
  (num)
  @{Sizes the @racket['framework:recently-opened-files/pos] preference list
    length to @racket[num].})

 (proc-doc/names
  icon:get-paren-highlight-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the parenthesis highlight @racket[bitmap%].  It is only used
    on black and white screens.})

 (proc-doc/names
  icon:get-eof-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the @racket[bitmap%] used for the clickable ``eof'' icon from
    @racket[text:ports].})

 (proc-doc/names
  icon:get-autowrap-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the autowrap's @racket[bitmap%].
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  icon:get-lock-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the lock's @racket[bitmap].
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  icon:get-unlock-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the reset unlocked @racket[bitmap].
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  icon:get-anchor-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the anchor's @racket[bitmap].
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  icon:get-left/right-cursor
  (-> (is-a?/c cursor%))
  ()
  @{This function returns a @racket[cursor%] object that indicates left/right
    sizing is possible, for use with columns inside a window.
    
    The cursor may not respond @racket[#t] to the @method[cursor% ok?]
    method.})

 (proc-doc/names
  icon:get-up/down-cursor
  (-> (is-a?/c cursor%))
  ()
  @{This function returns a @racket[cursor%] object that indicates up/down
    sizing is possible, for use with columns inside a window.
    
    The cursor may not respond @racket[#t] to the @method[cursor% ok?]
    method.})

 (proc-doc/names
  icon:get-gc-on-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns a bitmap to be displayed in an @racket[frame:info<%>] frame
    when garbage collection is taking place.
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  icon:get-gc-off-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns a bitmap to be displayed in an @racket[frame:info<%>] frame
    when garbage collection is not taking place.
    
    The bitmap may not respond @racket[#t] to the @method[bitmap% ok?]
    method.})

 (proc-doc/names
  keymap:remove-user-keybindings-file
  (-> any/c any)
  (user-keybindings-path)
  @{Removes the keymap previously added by
    @racket[keymap:add-user-keybindings-file].})

 (proc-doc/names
  keymap:add-user-keybindings-file
  (-> any/c any)
  (user-keybindings-path-or-require-spec)
  @{Chains the keymap defined by @racket[user-keybindings-path-or-require-spec]
    to the global keymap, returned by @racket[keymap:get-global].
    
    If @racket[user-keybindings-path-or-require-spec] is a path, the module is
    loaded directly from that path.  Otherwise,
    @racket[user-keybindings-path-or-require-spec] is treated like an argument
    to @racket[require].})

 (parameter-doc
  keymap:add-to-right-button-menu
  (parameter/c
   (-> (is-a?/c popup-menu%)
       (is-a?/c editor<%>)
       (is-a?/c event%)
       void?))
  proc
  @{When the keymap that @racket[keymap:get-global] returns is installed into
    an editor, this parameter's value is used for right button clicks.
    
    Before calling this procedure, the function
    @racket[append-editor-operation-menu-items] is called.
    
    See also @racket[keymap:add-to-right-button-menu/before].})

 (parameter-doc
  keymap:add-to-right-button-menu/before
  (parameter/c
   (-> (is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) void?))
  proc
  @{When the keymap that @racket[keymap:get-global] returns is installed into
    an editor, this function is called for right button clicks.
    
    After calling this procedure, the function
    @racket[append-editor-operation-menu-items] is called.
    
    See also @racket[keymap:add-to-right-button-menu].})

 (proc-doc/names
  keymap:call/text-keymap-initializer
  ((-> any/c) . -> . any/c)
  (thunk-proc)
  @{This function parameterizes the call to @racket[thunk-proc] by setting the
    keymap-initialization procedure (see
    @racket[current-text-keymap-initializer]) to install the framework's
    standard text bindings.})

 (proc-doc/names
  keymap:canonicalize-keybinding-string
  (string? . -> . string?)
  (keybinding-string)
  @{Returns a string that denotes the same keybindings as the input string,
    except that it is in canonical form; two canonical keybinding strings can
    be compared with @racket[string=?].})

 (proc-doc/names
  keymap:get-editor
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for handling standard editing operations.  It binds
    these keys:
    
    @itemize[
      @item{@racket["z"]: undo}
      @item{@racket["y"]: redo}
      @item{@racket["x"]: cut}
      @item{@racket["c"]: copy}
      @item{@racket["v"]: paste}
      @item{@racket["a"]: select all}]
    where each key is prefixed with the menu-shortcut key, based on the
    platform.  Under Unix, the shortcut is @racket["a:"]; under windows the
    shortcut key is @racket["c:"] and under MacOS, the shortcut key is
    @racket["d:"].})

 (proc-doc/names
  keymap:get-file
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for handling file operations.})

 (proc-doc/names
  keymap:get-user
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap that contains all of the keybindings in the keymaps
    loaded via @racket[keymap:add-user-keybindings-file]})

 (proc-doc/names
  keymap:get-global
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for general operations.  See
    @racket[keymap:setup-global] for a list of the bindings this keymap
    contains.})

 (proc-doc/names
  keymap:get-search
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for searching operations.})

 (proc-doc/names
  keymap:make-meta-prefix-list
  (->* (string?) (boolean?) (listof string?))
  ((key)
   ((mask-control? #f)))
  @{This prefixes a key with all of the different meta prefixes and returns a
    list of the prefixed strings. If @racket[mask-control?] is @racket[#t],
    then the result strings include @racket["~c:"] in them 
    (see @racket[keymap:send-map-function-meta]) for a fuller discussion of this
    boolean).
    
    Takes a keymap, a base key specification, and a function name; it prefixes
    the base key with all ``meta'' combination prefixes, and installs the new
    combinations into the keymap.  For example,
    @racket[(keymap:send-map-function-meta keymap "a" func)] maps
    @racket["m:a"] and @racket["ESC;a"] to @racket[func].})

 (proc-doc/names
  keymap:send-map-function-meta
  (->* ((is-a?/c keymap%) string? string?) (boolean?) void?)
  ((keymap key func)
   ((mask-control? #f)))
  @{@index{Meta} Most keyboard and mouse mappings are inserted into a keymap by
    calling the keymap's @method[keymap% map-function] method.  However,
    ``meta'' combinations require special attention.  The @racket["m:"] prefix
    recognized by @method[keymap% map-function] applies only to the Meta key
    that exists on some keyboards.  By convention, however, ``meta''
    combinations can also be accessed by using ``ESC'' as a prefix.
    
    This procedure binds all of the key-bindings obtained by prefixing
    @racket[key] with a meta-prefix to @racket[func] in @racket[keymap].
    
    If @racket[mask-control?] is @racket[#t],
    then the result strings include @racket["~c:"] in them.
    This is important under Windows where international keyboards
    often require characters that are unmodified on US keyboards to
    be typed with the AltGr key; such keys come into the system as
    having both the control and the meta modified applied to them and, 
    generally speaking, keybindings should not change the behavior of
    those keys.})

 (proc-doc/names
  keymap:setup-editor
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This sets up the input keymap with the bindings described in
    @racket[keymap:get-editor].})

 (proc-doc/names
  keymap:setup-file
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This extends a @racket[keymap%] with the bindings for files.})

 (proc-doc/names
  keymap:setup-global
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This function extends a @racket[keymap%] with the following functions:
    @itemize[
      @item{@mapdesc[ring-bell any] --- Rings the bell (using @racket[bell])
            and removes the search panel from the frame, if there.}
      @item{@mapdesc[save-file key] --- Saves the buffer.  If the buffer has no
            name, then @racket[finder:put-file]@index["finder:put-file"] is
            invoked.}
      @item{@mapdesc[save-file-as key] --- Calls
            @racket[finder:put-file]@index["finder:put-file"] to save the
            buffer.}
      @item{@mapdesc[load-file key] --- Invokes
            @racket[finder:open-file]@index["finder:open-file"].}
      @item{@mapdesc[find-string key] --- Opens the search buffer at the bottom
            of the frame, unless it is already open, in which case it searches
            for the text in the search buffer.}
      @item{@mapdesc[find-string-reverse key] --- Same as ``find-string'', but
            in the reverse direction.}
      @item{@mapdesc[find-string-replace key] --- Opens a replace string dialog
            box.}
      @item{@mapdesc[toggle-anchor key] --- Turns selection-anchoring on or
            off.}
      @item{@mapdesc[center-view-on-line key] --- Centers the buffer in its
            display using the currently selected line.}
      @item{@mapdesc[collapse-space key] --- Collapses all non-return
            whitespace around the caret into a single space.}
      @item{@mapdesc[remove-space key] --- Removes all non-return whitespace
            around the caret.}
      @item{@mapdesc[collapse-newline key] --- Collapses all empty lines around
            the caret into a single empty line.  If there is only one empty
            line, it is removed.}
      @item{@mapdesc[open-line key] --- Inserts a new line.}
      @item{@mapdesc[transpose-chars key] --- Transposes the characters before
            and after the caret and moves forward one position.}
      @item{@mapdesc[transpose-words key] --- Transposes words before and after
            the caret and moves forward one word.}
      @item{@mapdesc[capitalize-word key] --- Changes the first character of
            the next word to a capital letter and moves to the end of the
            word.}
      @item{@mapdesc[upcase-word key] --- Changes all characters of the next
            word to capital letters and moves to the end of the word.}
      @item{@mapdesc[downcase-word key] --- Changes all characters of the next
            word to lowercase letters and moves to the end of the word.}
      @item{@mapdesc[kill-word key] --- Kills the next word.}
      @item{@mapdesc[backward-kill-word key] --- Kills the previous word.}
      @item{@mapdesc[goto-line any] --- Queries the user for a line number and
            moves the caret there.}
      @item{@mapdesc[goto-position any] --- Queries the user for a position
            number and moves the caret there.}
      @item{@mapdesc[copy-clipboard mouse] --- Copies the current selection to
            the clipboard.}
      @item{@mapdesc[cut-clipboard mouse] --- Cuts the current selection to the
            clipboard.}
      @item{@mapdesc[paste-clipboard mouse] --- Pastes the clipboard to the
            current selection.}
      @item{@mapdesc[copy-click-region mouse] --- Copies the region between the
            caret and the input mouse event.}
      @item{@mapdesc[cut-click-region mouse] --- Cuts the region between the
            caret and the input mouse event.}
      @item{@mapdesc[paste-click-region mouse] --- Pastes the clipboard into
            the position of the input mouse event.}
      @item{@mapdesc[select-click-word mouse] --- Selects the word under the
            input mouse event.}
      @item{@mapdesc[select-click-line mouse] --- Selects the line under the
            input mouse event.}
      @item{@mapdesc[start-macro key] -- Starts recording a keyboard macro}
      @item{@mapdesc[end-macro key] --- Stops recording a keyboard macro}
      @item{@mapdesc[do-macro key] --- Executes the last keyboard macro}
      @item{@mapdesc[toggle-overwrite key] --- Toggles overwriting mode}]
    
    These functions are bound to the following keys
    (C = control, S = shift, A = alt, M = ``meta'', D = command):
    
    @itemize[
      @item{C-g : ``ring-bell''}
      @item{M-C-g : ``ring-bell''}
      @item{C-c C-g : ``ring-bell''}
      @item{C-x C-g : ``ring-bell''}
      @item{C-p : ``previous-line''}
      @item{S-C-p : ``select-previous-line''}
      @item{C-n : ``next-line''}
      @item{S-C-n : ``select-next-line''}
      @item{C-e : ``end-of-line''}
      @item{S-C-e : ``select-to-end-of-line''}
      @item{D-RIGHT : ``end-of-line''}
      @item{S-D-RIGHT : ``select-to-end-of-line''}
      @item{M-RIGHT : ``end-of-line''}
      @item{S-M-RIGHT : ``select-to-end-of-line''}
      @item{C-a : ``beginning-of-line''}
      @item{S-C-a : ``select-to-beginning-of-line''}
      @item{D-LEFT : ``beginning-of-line''}
      @item{D-S-LEFT : ``select-to-beginning-of-line''}
      @item{M-LEFT : ``beginning-of-line''}
      @item{M-S-LEFT : ``select-to-beginning-of-line''}
      @item{C-h : ``delete-previous-character''}
      @item{C-d : ``delete-next-character''}
      @item{C-f : ``forward-character''}
      @item{S-C-f : ``select-forward-character''}
      @item{C-b : ``backward-character''}
      @item{S-C-b : ``select-backward-character''}
      @item{M-f : ``forward-word''}
      @item{S-M-f : ``select-forward-word''}
      @item{A-RIGHT : ``forward-word''}
      @item{A-S-RIGHT : ``forward-select-word''}
      @item{M-b : ``backward-word''}
      @item{S-M-b : ``select-backward-word''}
      @item{A-LEFT : ``backward-word''}
      @item{A-S-LEFT : ``backward-select-word''}
      @item{M-d : ``kill-word''}
      @item{M-DELETE : ``backward-kill-word''}
      @item{M-c : ``capitalize-word''}
      @item{M-u : ``upcase-word''}
      @item{M-l : ``downcase-word''}
      @item{M-< : ``beginning-of-file''}
      @item{S-M-< : ``select-to-beginning-of-file''}
      @item{M-> : ``end-of-file''}
      @item{S-M-> : ``select-to-end-of-file''}
      @item{C-v : ``next-page''}
      @item{S-C-v : ``select-next-page''}
      @item{M-v : ``previous-page''}
      @item{S-M-v : ``select-previous-page''}
      @item{C-l : ``center-view-on-line''}
      @item{C-k : ``delete-to-end-of-line''}
      @item{C-y : ``paste-clipboard'' (Except Windows)}
      @item{A-v : ``paste-clipboard''}
      @item{D-v : ``paste-clipboard''}
      @item{C-_ : ``undo''}
      @item{C-x u : ``undo''}
      @item{C-+ : ``redo''}
      @item{C-w : ``cut-clipboard''}
      @item{M-w : ``copy-clipboard''}
      @item{C-x C-s : ``save-file''}
      @item{C-x C-w : ``save-file-as''}
      @item{C-x C-f : ``load-file''}
      @item{C-s : ``find-string''}
      @item{C-r : ``find-string-reverse''}
      @item{M-% : ``find-string-replace''}
      @item{SPACE : ``collapse-space''}
      @item{M-Backslash : ``remove-space''}
      @item{C-x C-o : ``collapse-newline''}
      @item{C-o : ``open-line''}
      @item{C-t : ``transpose-chars''}
      @item{M-t : ``transpose-words''}
      @item{C-SPACE : ``toggle-anchor''}
      @item{M-g : ``goto-line''}
      @item{M-p : ``goto-position''}
      @item{LEFTBUTTONTRIPLE : ``select-click-line''}
      @item{LEFTBUTTONDOUBLE : ``select-click-word''}
      @item{RIGHTBUTTON : ``copy-click-region''}
      @item{RIGHTBUTTONDOUBLE : ``cut-click-region''}
      @item{MIDDLEBUTTON : ``paste-click-region''}
      @item{C-RIGHTBUTTON : ``copy-clipboard''}
      @item{INSERT : ``toggle-overwrite''}
      @item{M-o : ``toggle-overwrite''}]})

 (proc-doc/names
  keymap:setup-search
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This extends a @racket[keymap%] with the bindings for searching.})

 (proc-doc/names
  keymap:set-chained-keymaps
  ((is-a?/c keymap:aug-keymap<%>)
   (listof (is-a?/c keymap%))
   . -> .
   void?)
  (keymap children-keymaps)
  @{Sets @racket[keymap]'s chained keymaps to @racket[children-keymaps],
    unchaining any keymaps that are currently chained to @racket[keymap].})

 (proc-doc/names
  keymap:remove-chained-keymap
  ((is-a?/c editor<%>)
   (is-a?/c keymap:aug-keymap<%>)
   . -> .
   void?)
  (editor keymap)
  @{Removes @racket[keymap] from the keymaps chained to @racket[editor].
    Also (indirectly) removes all keymaps chained to @racket[keymap] from
    @racket[editor], since they are removed when unchaining @racket[keymap]
    itself.
    
    Each of the keymaps chained to @racket[editor] must be an
    @racket[keymap:aug-keymap%] and @racket[keymap] cannot be the result of
    @racket[(send editor get-keymap)] That is, @racket[keymap] must be chained
    to some keymap attached to the editor.})

 (proc-doc/names
  keymap:region-click
  (-> any/c any/c (-> number? boolean? number? number? any)
      any)
  (text mouse-event f)
  @{Calls @racket[f] after computing where the @racket[event]
    corresponds to in the @racket[text]. If @racket[event] is
    not a @racket[mouse-event%] object or if @racket[text] is not a
    @racket[text%] object, this function does nothing, returning
    @racket[(void)]. 
    
    The arguments to @racket[f] are:
    @itemize[@item{the position where the click occurred}
             @item{a boolean indicating if the position is at
                   the right-hand edge of the screen (to
                   cover the eol ambiguity)}]})
 
 (proc-doc/names
  racket:text-balanced?
  (->* ((is-a?/c text%))
       (number? (or/c false/c number?))
       boolean?)
  ((text)
   ((start 0) (end #f)))
  @{Determines if the range in the editor from @racket[start] to @racket[end]
    in @racket[text] has at least one complete s-expression and there are no
    incomplete s-expressions.  If @racket[end] is @racket[#f], it defaults to
    the last position of the @racket[text]. The designation ``complete'' is
    defined to be something that does not cause @racket[read] to raise a
    @racket[exn:fail:read:eof?] exception, so there may be all kinds of strange
    read-level (not to speak of parse level) errors in the expressions.
    
    The implementation of this function creates a port with
    @racket[open-input-text-editor] and then uses @racket[read] to parse the
    range of the buffer.})

 (proc-doc/names
  racket:add-preferences-panel
  (-> void?)
  ()
  @{Adds a tabbing preferences panel to the preferences dialog.})

 (proc-doc/names
  racket:get-keymap
  (-> (is-a?/c keymap%))
  ()
  @{Returns a keymap with binding suitable for Racket.})

 (proc-doc/names
  racket:add-coloring-preferences-panel
  (-> any)
  ()
  @{Installs the ``Racket'' preferences panel in the ``Syntax Coloring''
    section.})

 (proc-doc/names
  racket:get-color-prefs-table
  (-> (listof (list/c symbol? (is-a?/c color%))))
  ()
  @{Returns a table mapping from symbols (naming the categories that the online
    colorer uses for Racket mode coloring) to their colors.
    
    These symbols are suitable for input to
    @racket[racket:short-sym->pref-name] and
    @racket[racket:short-sym->style-name].
    
    See also @racket[racket:get-white-on-black-color-prefs-table].})

 (proc-doc/names
  racket:get-white-on-black-color-prefs-table
  (-> (listof (list/c symbol? (is-a?/c color%))))
  ()
  @{Returns a table mapping from symbols (naming the categories that the online
    colorer uses for Racket mode coloring) to their colors when the user
    chooses the white-on-black mode in the preferences dialog.
    
    See also @racket[racket:get-color-prefs-table].})

 (proc-doc/names
  racket:short-sym->pref-name
  (symbol? . -> . symbol?)
  (short-sym)
  @{Builds the symbol naming the preference from one of the symbols in the
    table returned by @racket[racket:get-color-prefs-table].})

 (proc-doc/names
  racket:short-sym->style-name
  (symbol? . -> . string?)
  (short-sym)
  @{Builds the symbol naming the editor style from one of the symbols in the
    table returned by @racket[racket:get-color-prefs-table].  This style is a
    named style in the style list returned by
    @racket[editor:get-standard-style-list].})

 (proc-doc/names
  racket:get-wordbreak-map
  (-> (is-a?/c editor-wordbreak-map%))
  ()
  @{This method returns a @racket[editor-wordbreak-map%] that is suitable for
    Racket.})

 (proc-doc/names
  racket:init-wordbreak-map
  ((is-a?/c keymap%) . -> . void?)
  (key)
  @{Initializes the workdbreak map for @racket[keymap].})

 (proc-doc/names
  racket:setup-keymap
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{Initializes @racket[keymap] with Racket-mode keybindings.})

 (proc-doc/names
  editor:set-current-preferred-font-size
  (-> exact-nonnegative-integer? void?)
  (new-size)
  @{Sets the font preference for the current monitor configuration to
    @racket[new-size]. 
    
    See also @racket[editor:get-current-preferred-font-size]
    and @racket[editor:font-size-pref->current-font-size].})
 
 (proc-doc
  editor:get-current-preferred-font-size
  (-> exact-nonnegative-integer?)
  @{Gets the current setting for the font size preference. Calls
    @racket[editor:font-size-pref->current-font-size] with the
    current preference setting.
    
    See also @racket[editor:set-current-preferred-font-size] and
    @racket[editor:get-change-font-size-when-monitors-change?].
    })
 
 (proc-doc/names
  editor:font-size-pref->current-font-size
  (-> (vector/c 
       ;; font sizes for specific monitor configurations
       (hash/c 
        ;; a particular monitor configuration: the widths and heights
        (non-empty-listof (list/c exact-nonnegative-integer? 
                                  exact-nonnegative-integer?))
        ;; the font size for that configuration
        exact-nonnegative-integer?
        #:flat? #t)
       
       ;; default font size, when none of the configs above apply
       exact-nonnegative-integer?
       #:flat? #t)
      exact-nonnegative-integer?)
  (font-preference)
  @{Determines the current monitor configuration and uses that to pick
    one of the sizes from its argument. The argument is expected
    to come from the preference value of @racket['framework:standard-style-list:font-size].
    
    Except if @racket[editor:get-change-font-size-when-monitors-change?] returns
    @racket[#f], in which case the current monitor configuration is not considered
    and the last-set size (the second position in the vector) is always returned.
    
    As background, the font size
    preference is actually saved on a per-monitor configuration basis; specifically
    the preference value (using the same contract as the argument of this function)
    contains a table mapping a list of monitor sizes (but not their
    positions) obtained by @racket[get-display-size] to the preferred font size
    (plus a default size used for new configurations). 
    
    See also @racket[editor:get-current-preferred-font-size], 
    @racket[editor:get-current-preferred-font-size], and
    @racket[editor:get-change-font-size-when-monitors-change?].})
 
 (proc-doc/names
  editor:get-change-font-size-when-monitors-change?
  (-> boolean?)
  ()
  @{Returns @racket[#t] when the framework will automatically
            adjust the current font size in the @racket["Standard"]
            style of the result of @racket[editor:get-standard-style-list]
            based on the monitor configuration.
            
            Defaults to @racket[#f]
            
            See also @racket[editor:set-change-font-size-when-monitors-change?];
            @racket[editor:font-size-pref->current-font-size].})
 
 (proc-doc/names
  editor:set-change-font-size-when-monitors-change?
  (-> boolean? void?)
  (b?)
  @{Controls the result of @racket[editor:get-change-font-size-when-monitors-change?].
                           
                           See also @racket[editor:get-change-font-size-when-monitors-change?].})
 
 (proc-doc/names
  editor:set-default-font-color
  (->* ((is-a?/c color%)) ((or/c #f (is-a?/c color%))) void?)
  ((fg-color) ((bg-color #f)))
  @{Sets the foreground color of the style named
    @racket[editor:get-default-color-style-name] to @racket[fg-color].
    If @racket[bg-color] is not @racket[#f], then @racket[editor:set-default-font-color]
    sets the background color to @racket[bg-color].})

 (proc-doc/names
  editor:get-default-color-style-name
  (-> string?)
  ()
  @{The name of the style (in the list returned by
    @racket[editor:get-standard-style-list]) that holds the default color.})

 (proc-doc/names
  editor:set-standard-style-list-delta
  (-> string? (is-a?/c style-delta%) void?)
  (name delta)
  @{Finds (or creates) the style named by @racket[name] in the result of
    @racket[editor:get-standard-style-list] and sets its delta to
    @racket[delta].
    
    If the style named by @racket[name] is already in the style list, it must
    be a delta style.})

 (proc-doc/names
  editor:set-standard-style-list-pref-callbacks
  (-> any)
  ()
  @{Installs the font preference callbacks that update the style list returned
    by @racket[editor:get-standard-style-list] based on the font preference
    symbols.})

 (proc-doc/names
  editor:get-standard-style-list
  (-> (is-a?/c style-list%))
  ()
  @{Returns a style list that is used for all instances of
    @racket[editor:standard-style-list%].})

 (proc-doc/names
  editor:add-after-user-keymap
  (-> (is-a?/c keymap%) (listof (is-a?/c keymap%)) (listof (is-a?/c keymap%)))
  (keymap keymaps)
  @{Returns a list that contains all of the keymaps in @racket[keymaps], in the
    same relative order, but also with @racket[keymap], where @racket[keymap]
    is now the first keymap after @racket[keymap:get-user] (if that keymap is
    in the list.)})

 (proc-doc/names
  panel:dragable-container-size
  (-> (listof (list/c real? real? boolean? boolean?)) real? boolean?
      (values real? real?))
  (container-info bar-thickness vertical?)
  @{Returns the minimum width and height for a @racket[panel:dragable<%>] object
    where @racket[container-info] (see @method[area-container<%> container-size] for
    more details on that argument) is the children's info, and @racket[bar-thickness] and
    @racket[vertical?] indicate the properties of the panel.
    
    This function is exported mostly for the test suite.})

 (proc-doc/names
  panel:dragable-place-children
  (-> (listof (list/c real? real? boolean? boolean?)) 
      real?
      real?
      (listof (between/c 0 1)) 
      real?
      boolean?
      (values (listof (list/c (integer-in 0 10000)
                              (integer-in 0 10000)
                              (integer-in 0 10000)
                              (integer-in 0 10000)))
              (listof (list/c (integer-in 0 10000)
                              (integer-in 0 10000)))))
  (container-info width height percentages bar-thickness vertical?)
  @{Returns the geometry information for a dragable panel. The inputs
    are the @racket[container-info] (see @method[area-container<%> place-children] for more info),
    the @racket[width] and @racket[height] of the window, the @racket[percentages] for the spacing
    of the children, and a real and a boolean indicating the thickness of the bar between
    the child panels and whether or not this is a vertical panel, respectively.
    
    This function is exported mostly for the test suite.})
 
 (proc-doc/names
  color-model:rgb->xyz
  (number? number? number? . -> . color-model:xyz?)
  (r g b)
  @{Converts a color represented as a red-green-blue tuple (each value from 0
    to 255) into an XYZ tuple.  This describes a point in the CIE
    XYZ color space.})

 (proc-doc/names
  color-model:rgb-color-distance
  (number? number? number? number? number? number? . -> . number?)
  (red-a green-a blue-a red-b green-b blue-b)
  @{This calculates a distance between two colors.  The smaller the distance,
    the closer the colors should appear to the human eye.  A distance of 10 is
    reasonably close that it could be called the same color.
    
    This function is not symmetric in red, green, and blue, so it is important
    to pass red, green, and blue components of the colors in the proper order.
    The first three arguments are red, green and blue for the first color,
    respectively, and the second three arguments are red green and blue for the
    second color, respectively.})

 (proc-doc/names
  color-model:xyz->rgb
  (number? number? number? . -> . (list/c number? number? number?))
  (x y z)
  @{Converts an XYZ-tuple (in the CIE XYZ colorspace) into a list of values
    representing an RGB-tuple.})

 (proc-doc/names
  color-model:xyz?
  (any/c . -> . boolean?)
  (val)
  @{Determines if @racket[val] an xyz color record.})

 (proc-doc/names
  color-model:xyz-x
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the x component of @racket[xyz].})

 (proc-doc/names
  color-model:xyz-y
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the y component of @racket[xyz].})

 (proc-doc/names
  color-model:xyz-z
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the z component of @racket[xyz].})

 (proc-doc/names
  color-prefs:set-default/color-scheme
  (-> symbol?
      (or/c (is-a?/c color%) string?)
      (or/c (is-a?/c color%) string?)
      void?)
  (pref-sym black-on-white-color white-on-black-color)
  @{Registers a preference whose value will be updated when the user clicks on
    one of the color scheme default settings in the preferences dialog.
    
    Also calls @racket[preferences:set-default] and
    @racket[preferences:set-un/marshall] with appropriate arguments to register
    the preference.})

 (proc-doc/names 
  color-prefs:register-color-preference
  (->* (symbol? string? (or/c (is-a?/c color%) (is-a?/c style-delta%)))
       ((or/c string? (is-a?/c color%) #f)
        #:background (or/c (is-a?/c color%) #f))
       void?)
  ((pref-name style-name color/sd)
   ((white-on-black-color #f)
    (background #f)))
  @{This function registers a color preference and initializes the style list
    returned from @racket[editor:get-standard-style-list].  In particular, it
    calls @racket[preferences:set-default] and
    @racket[preferences:set-un/marshall] to install the pref for
    @racket[pref-name], using @racket[color/sd] as the default color.  The
    preference is bound to a @racket[style-delta%], and initially the
    @racket[style-delta%] changes the foreground color to @racket[color/sd],
    unless @racket[color/sd] is a style delta already, in which case it is just
    used directly.  Then, it calls
    @racket[editor:set-standard-style-list-delta] passing the
    @racket[style-name] and the current value of the preference
    @racket[pref-name].
    
    Finally, it adds calls @racket[preferences:add-callback] to set a callback
    for @racket[pref-name] that updates the style list when the preference
    changes.
    
    If @racket[white-on-black-color] is not @racket[#f], then the color of the
    @racket[color/sd] argument is used in combination with
    @racket[white-on-black-color] to register this preference with
    @racket[color-prefs:set-default/color-scheme].
    
    If @racket[background] is not @racket[#f], then it is used to construct the
    default background color for the style delta.
    })

 (proc-doc/names
  color-prefs:add-background-preferences-panel
  (-> void?)
  ()
  @{Adds a preferences panel that configures the background color for
    @racket[editor:basic-mixin].})

 (proc-doc/names
  color-prefs:add-to-preferences-panel
  (string? ((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (name func)
  @{Calls @racket[func] with the subpanel of the preferences coloring panel
    that corresponds to @racket[name].})

 (proc-doc/names
  color-prefs:build-color-selection-panel
  (->* ((is-a?/c area-container<%>) symbol? string? string?)
       (#:background? boolean?)
       void?)
  ((parent pref-sym style-name example-text)
   ((background? #f)))
  @{Builds a panel with a number of controls for configuring a font: its color
    (including a background configuration if @racket[background] is @racket[#t])
    and check boxes for bold, italic, and underline.  The @racket[parent]
    argument specifies where the panel will be placed.  The @racket[pref-sym]
    should be a preference (suitable for use with @racket[preferences:get] and
    @racket[preferences:set]).  The @racket[style-name] specifies the name of a
    style in the style list returned from
    @racket[editor:get-standard-style-list] and @racket[example-text] is shown
    in the panel so users can see the results of their configuration.})

 (proc-doc/names
  color-prefs:marshall-style-delta
  (-> (is-a?/c style-delta%) printable/c)
  (style-delta)
  @{Builds a printed representation for a style-delta.})

 (proc-doc/names
  color-prefs:unmarshall-style-delta
  (-> printable/c (or/c false/c (is-a?/c style-delta%)))
  (marshalled-style-delta)
  @{Builds a style delta from its printed representation.  Returns @racket[#f]
    if the printed form cannot be parsed.})

 (proc-doc/names
  color-prefs:white-on-black
  (-> any)
  ()
  @{Sets the colors registered by @racket[color-prefs:register-color-preference]
    to their white-on-black variety.})

 (proc-doc/names
  color-prefs:black-on-white
  (-> any)
  ()
  @{Sets the colors registered by @racket[color-prefs:register-color-preference]
    to their black-on-white variety.})
 
 (proc-doc
  color-prefs:add-color-scheme-entry
  (->i ([name symbol?]
        [black-on-white-color (or/c string? (is-a?/c color%))]
        [white-on-black-color (or/c string? (is-a?/c color%))])
       (#:style 
        [style (or/c #f string?)]
        #:bold? [bold? (style) (if style boolean? #f)]
        #:underline? [underline? (style) (if style boolean? #f)]
        #:italic? [italic? (style) (if style boolean? #f)]
        #:background
        [background (style)
                    (if style
                        (or/c #f string? (is-a?/c color%))
                        #f)])
       [result void?])
  (#f #f #f #f #f)
  @{Registers a new color or style named @racket[name] for use in the color schemes. 
    If @racket[style] is provided, a new style is registered; if not a color is
    registered.})
 
 (proc-doc/names
  color-prefs:add-color-scheme-preferences-panel
  (->* () (#:extras (-> (is-a?/c panel%) any)) void?)
  (() ((extras void)))
  @{Adds a panel for choosing a color-scheme to the preferences dialog.
    
    The @racket[extras] argument is called after the color schemes have been added
    to the preferences panel. It is passed the panel containing the color schemes
    and can add items to it.})
 
 (proc-doc
  color-prefs:register-info-based-color-schemes
  (-> void?)
  @{Reads 
    the @filepath{info.rkt} file in each collection, looking for the key
    @index{framework:color-schemes}
    @racket['framework:color-schemes]. Each definition must bind
    a list of hash tables, each of which introduces a new
    color scheme. Each hash table should have keys that specify
    details of the color scheme, as follows:
    @itemlist[@item{@racket['name]: must be either a string or a symbol;
                     if it is a symbol and @racket[string-constant?], 
                     it is passed to @racket[dynamic-string-constant]
                     to get the name; otherwise it is used as the name directly.
                     If absent, the name of the directory containing the @filepath{info.rkt}
                     file is used as the name.}
               @item{@racket['white-on-black-base?]: must be a boolean indicating if this
                      color-scheme is based on an inverted color scheme. If absent, it
                      is @racket[#f].}
               @item{@racket['example]: must be a string and is used in the preferences dialog
                      to show an example of the color scheme. If absent, the string used in
                      the ``Classic'' color scheme is used.}
               @item{@racket['colors]: must be a non-empty list whose first position
                      is a symbol, naming a color or style. The rest of the elements describe
                      the style or color. In either case, an element may be a vector of three
                      bytes: this describes a color (in r/g/b order) with an alpha value of
                      @racket[1.0]. The vector may also have three bytes followed by a real
                      number between @racket[0] and @racket[1], which is used as the alpha
                      value. If the name corresponds to a style, then the list may also contain
                      the symbols @racket['bold], @racket['italic], or @racket['underline].}]
    
    The names of the colors and styles are extensible; new ones can be added by calling
    @racket[color-prefs:add-color-scheme-entry]. When
    @racket[color-prefs:register-info-based-color-schemes]
    is called, it logs the active set of color names and style names to the @tt{color-scheme}
    logger at the info level. So, for example, starting up DrRacket like this:
    @tt{racket -W info@"@"color-scheme -l drracket} will print out the styles used in your
    version of DrRacket.})
 
 (proc-doc/names
  color-prefs:set-current-color-scheme
  (-> symbol? void?)
  (name)
  @{Sets
    the current color scheme to the scheme named @racket[name], 
    if @racket[name] is @racket[color-prefs:known-color-scheme-name?].
    Otherwise, does nothing.})
 
 (proc-doc
  color-prefs:get-current-color-scheme-name
  (-> color-prefs:color-scheme-style-name?)
  @{Returns the current color scheme's name.})
 
 (proc-doc/names
  color-prefs:known-color-scheme-name?
  (-> any/c boolean?)
  (name)
  @{Returns @racket[#t] if the input is a @racket[symbol?] that names
            a color or style that is part of the current color scheme.
            
            In order to return @racket[#t], @racket[name] must have been
            passed as the first argument to @racket[color-prefs:add-color-scheme-entry].})
 
 (proc-doc/names
  color-prefs:color-scheme-style-name?
  (-> any/c boolean?)
  (name)
  @{Returns @racket[#t] if @racket[name] is a known color scheme name,
            and is connected to a style. 
            
            In order to return @racket[#t], @racket[name] must have been
            passed as the first argument to @racket[color-prefs:add-color-scheme-entry]
            and the @racket[#:style] argument must have also been passed.})
 
 (proc-doc 
  color-prefs:lookup-in-color-scheme
  (->i ([name color-prefs:known-color-scheme-name?])
       ()
       [result (name)
               (if (color-prefs:color-scheme-style-name? name)
                   (is-a?/c style-delta%)
                   (is-a?/c color%))])
  @{Returns the current style delta or color associated with @racket[name].})
 
 (proc-doc
  color-prefs:set-in-color-scheme
  (->i ([name color-prefs:known-color-scheme-name?]
        [new-val (name)
                 (if (color-prefs:color-scheme-style-name? name)
                     (is-a?/c style-delta%)
                     (is-a?/c color%))])
       ()
       [result void?])
  @{Updates the current color or style delta associated with 
    @racket[name] in the current color scheme.})
 
 (proc-doc
  color-prefs:register-color-scheme-entry-change-callback
  (->i ([name color-prefs:known-color-scheme-name?]
        [fn (name)
            (-> (if (color-prefs:color-scheme-style-name? name)
                    (is-a?/c style-delta%)
                    (is-a?/c color%))
                any)])
       ([weak? boolean?])
       [result void?])
  (#f)
  @{Registers a callback that is invoked whenever the color mapped by
    @racket[name] changes. Changes may happen due to calls to
    @racket[color-prefs:set-in-color-scheme] or due to calls to 
    @racket[color-prefs:set-current-color-scheme].
    
    If @racket[weak?] is @racket[#t], the @racket[fn] argument is held
    onto weakly; otherwise it is held onto strongly.})
 
 (proc-doc
  color-prefs:get-color-scheme-names
  (-> (values set? set?))
  @{Returns two sets; the first is the known color scheme names that are just colors
    and the second is the known color scheme names that are styles.
    
    These are all of the names that have been passed to @racket[color-prefs:add-color-scheme-entry].})
 )


(define-syntax (racket:-reprovides stx)
  #`(provide
     (rename-out #,@(for/list ([suffix (in-list racket:ids)])
                      (define rkt (string->symbol (format "racket:~a" suffix)))
                      (define scm (string->symbol (format "scheme:~a" suffix)))
                      #`[#,rkt #,scm]))))
(racket:-reprovides)
