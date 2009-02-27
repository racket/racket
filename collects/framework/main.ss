#lang at-exp scheme/gui

(require mred/mred-unit
         mred/mred-sig
         framework/framework-unit
         framework/private/sig
         (for-syntax scheme/base)
         scribble/srcdoc)

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

(require/doc scheme/base scribble/manual framework/private/mapdesc)

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
 (prefix color: framework:color^)
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

(define-compound-unit/infer framework+mred@
  (import)
  (export framework^)
  (link standard-mred@ framework@))

(define-values/invoke-unit/infer framework+mred@)

(provide/doc
 (parameter-doc
  text:autocomplete-append-after
  (parameter/c string?)
  suffix
  @{A string that is inserted after a completion is inserted by a
      @scheme[text:autocomplete] instance.
      
      Defaults to @scheme[""].})
 
 (parameter-doc
  text:autocomplete-limit
  (parameter/c (and/c integer? exact? positive?))
  count
  @{Controls the number of completions visible at a time in the menu
             produced by @scheme[text:autocomplete] instances.
             
             Defaults to 15.})
 
 (proc-doc/names
  text:get-completions/manuals
  (-> (or/c false/c (listof symbol?)) (listof string?))
  (manuals)
  @{Returns the list of keywords for the manuals from @scheme[manuals]
            by extracting all of the documented exports of the manuals.  The
            symbols are meant to be module paths, eg the quoted
            form of the argument to @scheme[require].

  If @scheme[manuals] is false,
            then all of the documented names are used.})
 
 (proc-doc/names
  text:lookup-port-name
  (-> symbol? (or/c (is-a?/c editor:basic<%>) false/c))
  (manuals)
  @{Returns the editor instance whose port-name matches the given symbol.  If no
            editor can be found, then returns @scheme[false].})
 
 (proc-doc/names
  number-snip:make-repeating-decimal-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix?)
  @{Makes a number snip that shows the decimal expansion for
          @scheme[number] The boolean indicates if a @litchar{#e} prefix
          appears on the number.
          
          See also @scheme[number-snip:make-fraction-snip].})
 
 (proc-doc/names
  number-snip:make-fraction-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix-in-decimal-view?)
  @{Makes a number snip that shows a fractional view of @scheme[number].
          The boolean indicates if a @litchar{#e} prefix appears on the
          number, when shown in the decimal state.
          
          See also @scheme[number-snip:make-repeating-decimal-snip].})
 
 (proc-doc/names
  version:add-spec
  (any/c any/c . -> . void?)
  (spec revision)
  @{These two values are appended to the version string.  @scheme[write]
          is used to transform them to strings.  For example:
          
          @scheme[(version:add-spec 's 1)]
          
          in version 205 will make the version string be @litchar{205s1}.  The
          symbols @scheme['f] and @scheme['d] are used internally for
          framework and drscheme revisions.})
 
 (proc-doc/names
  version:version
  (-> string?)
  ()
  @{This function returns a string describing the version of this
         application.  See also @scheme[version:add-spec].})
 
 (parameter-doc
  application:current-app-name
  (parameter/c string?)
  name
  @{This is a parameter specifying the name of the current
         application.  It is used in the help menu 
         (see @scheme[frame:standard-menus%]) and in frame titles 
         (see @scheme[frame:editor%]).
         The first case in the case-lambda returns the current name, and the
         second case in the case-lambda sets the name of the application to
         @scheme[name].})
 
 (proc-doc/names
  preferences:put-preferences/gui
  (-> (listof symbol?)
      (listof any/c)
      any)
  (name-list val-list)
  @{Like @scheme[put-preferences], but has more sophisticated error
         handling.  In particular, it
         @itemize{
                  @item{waits for three consecutive failures before informing the
                              user}
                       @item{gives the user the opportunity to ``steal'' the lockfile
                                   after the third failure, and}
                       @item{when failures occur, it remembers what its arguments were
                              and if any preference save eventually succeeds, all of the
                              past failures are also written at that point.}}})
 
 (proc-doc/names
  preferences:add-panel
  (-> (or/c string? (cons/c string? (listof string?)))
      (->d ([parent (is-a?/c area-container-window<%>)])
           ()
           [_
            (let ([old-children (send parent get-children)])
              (and/c (is-a?/c area-container-window<%>)
                     (λ (child)
                       (andmap eq?
                               (append old-children (list child))
                               (send parent get-children)))))])
      void?)
  (labels f)
  @{@scheme[preferences:add-preference-panel] adds the result of
           @scheme[f] with name @scheme[labels] to the preferences dialog box.
           
           The labels determine where this preference panel is placed in the
           dialog.  If the list is just one string, the preferences panel is
           placed at the top level of the dialog.  If there are more strings, a
           hierarchy of nested panels is created and the new panel is added at
           the end.  If multiple calls to
           @scheme[preferences:add-preference-panel] pass the same prefix of
           strings, those panels are placed in the same children.
           
           When the preference dialog is opened for the first time, the
           function @scheme[f] is called with a panel, and @scheme[f] is
           expected to add a new child panel to it and add whatever preferences
           configuration controls it wants to that panel.  Then, @scheme[f]'s
           should return the panel it added.})
 
 (proc-doc/names
  preferences:add-editor-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options related to
         editing.})
 
 (proc-doc/names
  preferences:add-warnings-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options relating to
         warnings.})
 
 (proc-doc/names
  preferences:add-scheme-checkbox-panel
  (-> void?)
  ()
  @{Adds a preferences panel for configuring options related to
         Scheme.})
 
 (proc-doc/names
  preferences:add-to-warnings-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @scheme[proc] until the preferences panel is created, when it
          is called with the Misc. panel to add new children to the panel.})
 
 (proc-doc/names
  preferences:add-to-scheme-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @scheme[proc] until the preferences panel is created, when it
          is called with the Scheme preferences panel to add new children to
          the panel.})
 
 (proc-doc/names
  preferences:add-to-editor-checkbox-panel
  (((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (proc)
  @{Saves @scheme[proc] until the preferences panel is created, when it
          is called with the Echeme preferences panel to add new children to
          the panel.})
 
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
  @{Registers @scheme[cb].  Next time the user clicks the OK button the
              preferences dialog, all of the @scheme[cb] functions are called,
              assuming that each of the callbacks passed to
              @scheme[preferences:add-can-close-dialog-callback] succeed.})
 
 (proc-doc/names
  preferences:add-can-close-dialog-callback
  ((-> boolean?) . -> . void?)
  (cb)
  @{Registers @scheme[cb].  Next time the user clicks the OK button the
              preferences dialog, all of the @scheme[cb] functions are called.  If
              any of them return @scheme[#f], the dialog is not closed.
              
              See also @scheme[preferences:add-on-close-dialog-callback].})
 
 (proc-doc/names
  autosave:register
  ((and/c (is-a?/c autosave:autosavable<%>)
          (is-a?/c editor<%>))
   . -> .
   void?)
  (obj)
  @{Adds @scheme[obj] to the list of objects to be autosaved.  When it
         is time to autosave, the @scheme[do-autosave] method of the object
         is called.  This method is responsible for performing the autosave.
         
         There is no need to de-register an object because the autosaver
         keeps a ``weak'' pointer to the object; i.e., the autosaver does not
         keep an object from garbage collection.})
 
 (proc-doc/names
  autosave:restore-autosave-files/gui
  (-> void?)
  ()
  @{Opens a GUI to ask the user about recovering any autosave files left
          around from crashes and things.
          
          This function doesn't return until the user has finished restoring
          the autosave files.  (It uses yield to handle events however.)})
 
 (proc-doc/names
  exit:exiting?
  (-> boolean?)
  ()
  @{Returns @scheme[#t] to indicate that an exit operation is taking
            place.  Does not indicate that the app will actually exit, since the
            user may cancel the exit.
            
            See also @scheme[exit:insert-on-callback] and
            @scheme[exit:insert-can?-callback].})
 
 (proc-doc/names
  exit:set-exiting
  (boolean? . -> . void?)
  (exiting?)
  @{Sets a flag that affects the result of @scheme[exit:exiting?].})
 
 (proc-doc/names
  exit:insert-on-callback
  ((-> void?) . -> . (-> void?))
  (callback)
  @{Adds a callback to be called when exiting.  This callback must not
         fail.  If a callback should stop an exit from happening, use
         @scheme[exit:insert-can?-callback].})
 
 (proc-doc/names
  exit:insert-can?-callback
  ((-> boolean?) . -> . (-> void?))
  (callback)
  @{Use this function to add a callback that determines if an attempted
        exit can proceed.  This callback should not clean up any state,
        since another callback may veto the exit.  Use
        @scheme[exit:insert-on-callback] for callbacks that clean up
        state.})
 
 (proc-doc/names
  exit:can-exit?
  (-> boolean?)
  ()
  @{Calls the ``can-callbacks'' and returns their results.  See
          @scheme[exit:insert-can?-callback] for more information.})
 
 (proc-doc/names
  exit:on-exit
  (-> void?)
  ()
  @{Calls the ``on-callbacks''.  See @scheme[exit:insert-on-callback]
          for more information.})
 
 (proc-doc/names
  exit:exit
  (-> any)
  ()
  @{@scheme[exit:exit] performs four actions:
           @itemize{
                    @item{sets the result of the @scheme[exit:exiting?] function to
                               @scheme[#t].}
                         @item{invokes the exit-callbacks, with @scheme[exit:can-exit?] if
                                       none of the ``can?'' callbacks return @scheme[#f],}
                         @item{invokes @scheme[exit:on-exit] and then}
                         @item{queues a callback that calls @scheme[exit] 
                                      (a mzscheme procedure) and (if @scheme[exit] returns) sets the result of
                                      @scheme[exit:exiting?] back to @scheme[#t].}}})
 
 (proc-doc/names
  exit:user-oks-exit
  (-> boolean?)
  ()
  @{Opens a dialog that queries the user about exiting.  Returns the
          user's decision.})
 
 (proc-doc/names
  path-utils:generate-autosave-name
  (string? . -> . string?)
  (filename)
  @{Generates a name for an autosave file from @scheme[filename].})
 
 (proc-doc/names
  path-utils:generate-backup-name
  (path? . -> . path?)
  (filename)
  @{Generates a name for an backup file from @scheme[filename].})
 
 (parameter-doc
  finder:dialog-parent-parameter
  (parameter/c (or/c false/c (is-a?/c dialog%) (is-a?/c frame%)))
  parent
  @{This parameter determines the parent of the dialogs created by
         @scheme[finder:get-file], @scheme[finder:put-file],
         @scheme[finder:common-get-file], @scheme[finder:common-put-file],
         @scheme[finder:common-get-file-list], @scheme[finder:std-get-file],
         and @scheme[finder:std-put-file].})
 
 (parameter-doc
  finder:default-extension
  (parameter/c string?)
  extension
  @{This parameter controls the default extension for the framework's
         @scheme[finder:put-file] dialog.  Its value gets passed as the
         @scheme[default-extension] argument to @scheme[put-file].
         
         Its default value is @scheme[""].})
 
 (parameter-doc
  finder:default-filters
  (parameter/c (listof (list/c string? string?)))
  filters
  @{
    This parameter controls the default filters for the framework's
         @scheme[finder:put-file] dialog.  Its value gets passed as the
         @scheme[default-filters] argument to @scheme[put-file].
         
         Its default value is @scheme['(("Any" "*.*"))].})
 
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
         platform-independent dialog box.  Consider using
         @scheme[finder:put-file] instead of this function.})
 
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
         @scheme[finder:get-file] instead of this function.})
 
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
         platform-dependent dialog box.  Consider using
         @scheme[finder:put-file] instead of this function.})
 
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
         platform-dependent dialog box.  Consider using
         @scheme[finder:get-file] instead of this function.})
 
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
            
            If the result of @scheme[(preferences:get 'framework:file-dialogs)]
            is @scheme['std] this calls @scheme[finder:std-put-file], and if it
            is @scheme['common], @scheme[finder:common-put-file] is called.})
 
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
            
            If the result of @scheme[(preferences:get 'framework:file-dialogs)]
            is @scheme['std] this calls @scheme[finder:std-get-file], and if it
            is @scheme['common], @scheme[finder:common-get-file] is called.})
 
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
  (symbol? number? number? . -> . void)
  (size-pref-sym width height)
  @{Initializes a preference for the @scheme[frame:size-pref] mixin.
                
                The first argument should be the preferences symbol, and the second
                an third should be the default width and height, respectively.})
 
 (proc-doc/names
  frame:add-snip-menu-items
  (->* ((is-a?/c menu%) (subclass?/c menu-item%))
       ((-> (is-a?/c menu-item%) void?))
       void?)
  ((menu menu-item%)
   ((func void)))
  @{Inserts three menu items into @scheme[menu], one that inserts a text
            box, one that inserts a pasteboard box, and one that inserts an
            image into the currently focused editor (if there is one).  Uses
            @scheme[menu-item%] as the class for the menu items.
            
            Calls @scheme[func] right after inserting each menu item.})
 
 (proc-doc/names
  frame:reorder-menus
  ((is-a?/c frame%) . -> . void?)
  (frame)
  @{Re-orders the menus in a frame.  It moves the ``File'' and ``Edit''
              menus to the front of the menubar and moves the ``Windows'' and
              ``Help'' menus to the end of the menubar.
              
              This is useful in conjunction with the frame classes.  After
              instantiating the class and adding ones own menus, the menus will be
              mis-ordered.  This function fixes them up.})
 
 (proc-doc/names
  frame:remove-empty-menus
  ((is-a?/c frame%) . -> . void?)
  (frame)
  @{Removes empty menus in a frame.})
 
 (proc-doc/names
  group:get-the-frame-group
  (-> (is-a?/c group:%))
  ()
  @{This returns the frame group.})
 
 (proc-doc/names
  group:on-close-action
  (-> void?)
  ()
  @{See also @scheme[group:can-close-check].
        
        Call this function from the @method[top-level-window<%> can-close?]
        callback of a frame in order for the group to properly close the
        application.})
 
 (proc-doc/names
  group:can-close-check
  (-> boolean?)
  ()
  @{See also @scheme[group:on-close-action].
        
        Call this function from the @method[top-level-window<%> can-close?]
        callback of a frame in order for the group to properly close the
        application.})
 
 (proc-doc/names
  group:add-to-windows-menu
  (-> (-> (is-a?/c menu%) any) any)
  (proc)
  @{Procedures passed to this function are called when the @onscreen{Windows} menu is
               created. Use it to add additional menu items.})
 
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
  @{Extracs the handler's handling function.})
 
 (proc-doc/names
  handler:insert-format-handler
  (string?
   (or/c string? (listof string?) (path? . -> . boolean?))
   (path? . -> . (or/c false/c (is-a?/c frame:editor<%>)))
   . -> .
   void?)
  (name pred handler)
  @{This function inserts a format handler.
         
         The string, @scheme[name] names the format handler for use with
         @scheme[handler:find-named-format-handler].  If @scheme[pred] is a
         string, it is matched with the extension of a filename by
         @scheme[handler:find-format-handler].  If @scheme[pred] is a list of
         strings, they are each matched with the extension of a filename by
         @scheme[handler:find-format-handler].  If it is a function, the
         filename is applied to the function and the functions result
         determines if this is the handler to use.
         
         The most recently added format handler takes precedence over all
         other format handlers.})
 
 (proc-doc/names
  handler:find-named-format-handler
  (string? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
  (name)
  @{This function selects a format handler.  See also
         @scheme[handler:insert-format-handler].
         
         It finds a handler based on @scheme[name].})
 
 (proc-doc/names
  handler:find-format-handler
  (path? . -> . (path? . -> . (is-a?/c frame:editor<%>)))
  (filename)
  @{This function selects a format handler.  See also
         @scheme[handler:insert-format-handler].
         
         It finds a handler based on @scheme[filename].})
 
 (proc-doc/names
  handler:edit-file
  (->* ((or/c path? false/c))
       ((-> (is-a?/c frame:editor<%>)))
       (or/c false/c (is-a?/c frame:editor<%>)))
  ((filename)
   ((make-default
     (λ () ((handler:current-create-new-window) filename)))))
  @{This function creates a frame or re-uses an existing frame to edit a
         file.
         
         If the preference @scheme['framework:open-here] is set to
         @scheme[#t], and 
         @scheme[(send (group:get-the-frame-group) get-open-here-frame)] 
         returns a frame, the
         @method[frame:open-here<%> open-here] method of that frame is used
         to load the file in the existing frame.
         
         Otherwise, it invokes the appropriate format handler to open the
         file (see @scheme[handler:insert-format-handler]).
         
         @itemize{
                  @item{If @scheme[filename] is a string, this function checks the
                           result of @scheme[group:get-the-frame-group] to see if the
                           @scheme[filename] is already open by a frame in the group.
                           @itemize{
                                    @item{If so, it returns the frame.}
                                         @item{If not, this function calls
                                                  @scheme[handler:find-format-handler] with
                                                  @scheme[filename].
                                                  @itemize{
                                                           @item{If a handler is found, it is applied to
                                                                    @scheme[filename] and it's result is the
                                                                    final result.}
                                                                @item{If not, @scheme[make-default] is used.}}}}}
                       @item{If @scheme[filename] is @scheme[#f], @scheme[make-default]
                                is used.}}})
 
 (parameter-doc
  handler:current-create-new-window
  (parameter/c (-> (or/c false/c path?) (is-a?/c frame%)))
  proc
  @{This is a parameter that controls how the framework creates new
         application windows.
         
         The default setting is this:
         @schemeblock[
                      (λ (filename)
                        (let ([frame (make-object frame:text-info-file% filename)])
                          (send frame show #t)
                          frame))
                      ]})
 
 (proc-doc/names
  handler:open-file
  (->* ()
       ((or/c false/c path? string?))
       (or/c false/c (is-a?/c frame:basic<%>)))
  (() 
   ((dir #f)))
  @{This function queries the user for a filename and opens the file for
         editing.  It uses @scheme[handler:edit-file] to open the file, once
         the user has chosen it.
         
         Calls @scheme[finder:get-file] and @scheme[handler:edit-file], passing along @scheme[dir].})
 
 (proc-doc/names
  handler:install-recent-items
  ((is-a?/c menu%) . -> . void?)
  (menu)
  @{This function deletes all of the items in the given menu and adds
         one menu item for each recently opened file.  These menu items, when
         selected, call @scheme[handler:edit-file] with the filename of the
         recently opened file.
         
         The menu's size is limited to 10.})
 
 (proc-doc/names
  handler:set-recent-items-frame-superclass
  ((implementation?/c frame:standard-menus<%>) . -> . void?)
  (frame)
  @{Sets the superclass for the recently opened files frame.  It must be
         derived from @scheme[frame:standard-menus].})
 
 (proc-doc/names
  handler:add-to-recent
  (path? . -> . void?)
  (filename)
  @{Adds a filename to the list of recently opened files.})
 
 (proc-doc/names
  handler:set-recent-position
  (path? number? number? . -> . void?)
  (filename start end)
  @{Sets the selection of the recently opened file to @scheme[start] and
         @scheme[end].})
 
 (proc-doc/names
  handler:size-recently-opened-files
  (number? . -> . void?)
  (num)
  @{Sizes the @scheme['framework:recently-opened-files/pos] preference
          list length to @scheme[num].})
 
 (proc-doc/names
  icon:get-paren-highlight-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the parenthesis highlight @scheme[bitmap%].  It is only
         used on black and white screens.})
 
 (proc-doc/names
  icon:get-eof-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the @scheme[bitmap%] used for the clickable ``eof''
         icon from @scheme[text:ports].})
 
 (proc-doc/names
  icon:get-autowrap-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the autowrap's @scheme[bitmap%].
         
         The bitmap may not respond @scheme[#t] to the @method[bitmap% ok?]
         method.})
 
 (proc-doc/names
  icon:get-lock-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the lock's @scheme[bitmap].
         
         The bitmap may not respond @scheme[#t] to the @method[bitmap% ok?]
         method.})
 
 (proc-doc/names
  icon:get-unlock-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the reset unlocked @scheme[bitmap].
         
         The bitmap may not respond @scheme[#t] to the @link bitmap ok?
         method.})
 
 (proc-doc/names
  icon:get-anchor-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns the anchor's @scheme[bitmap].
         
         The bitmap may not respond @scheme[#t] to the @method[bitmap% ok?]
         method.})
 
 (proc-doc/names
  icon:get-left/right-cursor
  (-> (is-a?/c cursor%))
  ()
  @{This function returns a @link cursor object that indicates
         left/right sizing is possible, for use with columns inside a window.
         
         The cursor may not respond @scheme[#t] to the @link cursor ok?
         method.})
 
 (proc-doc/names
  icon:get-up/down-cursor
  (-> (is-a?/c cursor%))
  ()
  @{This function returns a @scheme[cursor%] object that indicates
         up/down sizing is possible, for use with columns inside a window.
         
         The cursor may not respond @scheme[#t] to the @method[cursor% ok?]
         method.})
 
 (proc-doc/names
  icon:get-gc-on-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns a bitmap to be displayed in an @scheme[frame:info<%>]
         frame when garbage collection is taking place.
         
         The bitmap may not respond @scheme[#t] to the @link bitmap ok?
         method.})
 
 (proc-doc/names
  icon:get-gc-off-bitmap
  (-> (is-a?/c bitmap%))
  ()
  @{This returns a bitmap to be displayed in an @scheme[frame:info<%>]
         frame when garbage collection is not taking place.
         
         The bitmap may not respond @scheme[#t] to the @method[bitmap% ok?]
         method.})
 
 (proc-doc/names
  keymap:remove-user-keybindings-file
  (-> any/c any)
  (user-keybindings-path)
  @{Removes the keymap previously added by
            @scheme[keymap:add-user-keybindings-file].})
 
 (proc-doc/names
  keymap:add-user-keybindings-file
  (-> any/c any)
  (user-keybindings-path-or-require-spec)
  @{Chains the keymap defined by
           @scheme[user-keybindings-path-or-require-spec] to the global keymap,
           returned by @scheme[keymap:get-global].
           
           If @scheme[user-keybindings-path-or-require-spec] is a path, the
           module is loaded directly from that path.  Otherwise,
           @scheme[user-keybindings-path-or-require-spec] is treated like an
           argument to @scheme[require].})
 
 (parameter-doc
  keymap:add-to-right-button-menu
  (parameter/c
   (-> (is-a?/c popup-menu%)
       (is-a?/c editor<%>)
       (is-a?/c event%)
       void?))
  proc
  @{When the keymap that @scheme[keymap:get-global] returns is installed
         into an editor, this parameter's value is used for right button
         clicks.
         
         Before calling this procedure, the function
         @scheme[append-editor-operation-menu-items] is called.
         
         See also @scheme[keymap:add-to-right-button-menu/before].})
 
 (parameter-doc
  keymap:add-to-right-button-menu/before
  (parameter/c
   (-> (is-a?/c popup-menu%) (is-a?/c editor<%>) (is-a?/c event%) void?))
  proc
  @{When the keymap that @scheme[keymap:get-global] returns is installed
         into an editor, this function is called for right button clicks.
         
         After calling this procedure, the function
         @scheme[append-editor-operation-menu-items] is called.
         
         See also @scheme[keymap:add-to-right-button-menu].})
 
 (proc-doc/names
  keymap:call/text-keymap-initializer
  ((-> any/c) . -> . any/c)
  (thunk-proc)
  @{Thus function parameterizes the call to @scheme[thunk-proc] by
         setting the keymap-initialization procedure (see
                                                      @scheme[current-text-keymap-initializer]) to install the framework's
                                                                                                standard text bindings.})
 
 (proc-doc/names
  keymap:canonicalize-keybinding-string
  (string? . -> . string?)
  (keybinding-string)
  @{Returns a string that denotes the same keybindings as the input
            string, except that it is in canonical form; two canonical
            keybinding strings can be compared with @scheme[string=?].})
 
 (proc-doc/names
  keymap:get-editor
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for handling standard editing operations.  It
         binds these keys:
         
         @itemize{
                  @item{@scheme["z"]: undo}
                       @item{@scheme["y"]: redo}
                       @item{@scheme["x"]: cut}
                       @item{@scheme["c"]: copy}
                       @item{@scheme["v"]: paste}
                       @item{@scheme["a"]: select all}}
         where each key is prefixed with the menu-shortcut key, based on the
         platform.  Under unix, the shortcut is @scheme["a:"]; under windows
         the shortcut key is @scheme["c:"] and under MacOS, the shortcut key
         is @scheme["d:"].})
 
 (proc-doc/names
  keymap:get-file
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for handling file operations.})

 (proc-doc/names
  keymap:get-user
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap that contains all of the keybindings in the keymaps loaded via @scheme[keymap:add-user-keybindings-file]})

 
 (proc-doc/names
  keymap:get-global
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for general operations.  See
         @scheme[keymap:setup-global] for a list of the bindings this keymap
         contains.})
 
 (proc-doc/names
  keymap:get-search
  (-> (is-a?/c keymap%))
  ()
  @{This returns a keymap for searching operations.})
 
 (proc-doc/names
  keymap:make-meta-prefix-list
  (string? . -> . (listof string?))
  (key)
  @{This prefixes a key with all of the different meta prefixes and
         returns a list of the prefixed strings.
         
         takes a keymap, a base key specification, and a function name; it
         prefixes the base key with all ``meta'' combination prefixes, and
         installs the new combinations into the keymap.  For example,
         @scheme[(keymap:send-map-function-meta keymap "a" func)] maps
         @scheme["m:a"] and @scheme["ESC;a"] to @scheme[func].})
 
 (proc-doc/names
  keymap:send-map-function-meta
  ((is-a?/c keymap%) string? string? . -> . void?)
  (keymap key func)
  @{@index{Meta} Most keyboard and mouse mappings are inserted into a
          keymap by calling the keymap's @method[keymap% map-function] method.
          However, ``meta'' combinations require special attention.  The
          @scheme["m:"] prefix recognized by @method[keymap% map-function]
          applies only to the Meta key that exists on some keyboards.  By
          convention, however, ``meta'' combinations can also be accessed by
          using ``ESC'' as a prefix.
          
          This procedure binds all of the key-bindings obtained by prefixing
          @scheme[key] with a meta-prefix to @scheme[func] in
          @scheme[keymap].})
 
 (proc-doc/names
  keymap:setup-editor
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This sets up the input keymap with the bindings described in
         @scheme[keymap:get-editor].})
 
 (proc-doc/names
  keymap:setup-file
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This extends a @scheme[keymap%] with the bindings for files.})
 
 (proc-doc/names
  keymap:setup-global
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This extends a @scheme[keymap%] with the general bindings.
            
            This function extends a @scheme[keymap%] with the following
            functions:
            @itemize{
                     @item{@mapdesc[ring-bell any] --- Rings the bell 
                                   (using @scheme[bell]) and removes the search panel from the frame,
                                   if there.}
                          @item{@mapdesc[save-file key] --- Saves the buffer.  If the buffer
                                        has no name, then
                                        @scheme[finder:put-file]@index["finder:put-file"] is
                                        invoked.}
                          @item{@mapdesc[save-file-as key] --- Calls
                                        @scheme[finder:put-file]@index["finder:put-file"] to save
                                        the buffer.}
                          @item{@mapdesc[load-file key] --- Invokes
                                        @scheme[finder:open-file]@index["finder:open-file"].}
                          @item{@mapdesc[find-string key] --- Opens the search buffer at the
                                        bottom of the frame, unless it is already open, in which
                                        case it searches for the text in the search buffer.}
                          @item{@mapdesc[find-string-reverse key] --- Same as
                                        ``find-string'', but in the reverse direction.}
                          @item{@mapdesc[find-string-replace key] --- Opens a replace string
                                        dialog box.}
                          @item{@mapdesc[toggle-anchor key] --- Turns selection-anchoring on
                                        or off.}
                          @item{@mapdesc[center-view-on-line key] --- Centers the buffer in
                                        its display using the currently selected line.}
                          @item{@mapdesc[collapse-space key] --- Collapses all non-return
                                        whitespace around the caret into a single space.}
                          @item{@mapdesc[remove-space key] --- Removes all non-return
                                        whitespace around the caret.}
                          @item{@mapdesc[collapse-newline key] --- Collapses all empty lines
                                        around the caret into a single empty line.  If there is only
                                        one empty line, it is removed.}
                          @item{@mapdesc[open-line key] --- Inserts a new line.}
                          @item{@mapdesc[transpose-chars key] --- Transposes the characters
                                        before and after the caret and moves forward one position.}
                          @item{@mapdesc[transpose-words key] --- Transposes words before
                                        and after the caret and moves forward one word.}
                          @item{@mapdesc[capitalize-word key] --- Changes the first
                                        character of the next word to a capital letter and moves to
                                        the end of the word.}
                          @item{@mapdesc[upcase-word key] --- Changes all characters of the
                                        next word to capital letters and moves to the end of the
                                        word.}
                          @item{@mapdesc[downcase-word key] --- Changes all characters
                                        of the next word to lowercase letters and moves to the end
                                        of the word.}
                          @item{@mapdesc[kill-word key] --- Kills the next word.}
                          @item{@mapdesc[backward-kill-word key] --- Kills the previous
                                        word.}
                          @item{@mapdesc[goto-line any] --- Queries the user for a line
                                        number and moves the caret there.}
                          @item{@mapdesc[goto-position any]  --- Queries the user for a
                                        position number and moves the caret there.}
                          @item{@mapdesc[copy-clipboard mouse] --- Copies the current
                                        selection to the clipboard.}
                          @item{@mapdesc[cut-clipboard mouse] --- Cuts the current selection
                                        to the clipboard.}
                          @item{@mapdesc[paste-clipboard mouse] --- Patses the clipboard to
                                        the current selection.}
                          @item{@mapdesc[copy-click-region mouse] --- Copies the region
                                        between the caret and the input mouse event.}
                          @item{@mapdesc[cut-click-region mouse] --- Cuts the  region
                                        between the caret and the input mouse event.}
                          @item{@mapdesc[paste-click-region mouse] --- Pastes the clipboard
                                        into the position of the input mouse event.}
                          @item{@mapdesc[select-click-word mouse] --- Selects the word under
                                        the input mouse event.}
                          @item{@mapdesc[select-click-line mouse] --- Selects the line under
                                        the input mouse event.}
                          @item{@mapdesc[start-macro key] -- Starts building a keyboard
                                        macro}
                          @item{@mapdesc[end-macro key] --- Stops building a keyboard macro}
                          @item{@mapdesc[do-macro key] --- Executes the last keyboard macro}
                          @item{@mapdesc[toggle-overwrite key] --- Toggles overwriting
                                        mode}}
            
            These functions are bound to the following keys 
            (C = control, S = shift, A = alt, M = ``meta'', D = command):
            
            @itemize{
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
                          @item{M-o : ``toggle-overwrite''}}})
 
 (proc-doc/names
  keymap:setup-search
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{This extends a @link keymap with the bindings for searching.})
 
 (proc-doc/names
  keymap:set-chained-keymaps
  ((is-a?/c keymap:aug-keymap<%>)
   (listof (is-a?/c keymap%))
   . -> .
   void?)
  (keymap children-keymaps)
  @{Sets @scheme[keymap]'s chained keymaps to @scheme[children-keymaps],
         unchaining any keymaps that are currently chained to
         @scheme[keymap].})
 
 (proc-doc/names
  keymap:remove-chained-keymap
  ((is-a?/c editor<%>)
   (is-a?/c keymap:aug-keymap<%>)
   . -> .
   void?)
  (editor keymap)
  @{Removes @scheme[keymap] from the keymaps chained to @scheme[editor].
            Also (indirectly) removes all keymaps chained to @scheme[keymap]
            from @scheme[editor], since they are removed when unchaining
            @scheme[keymap] itself.
            
            Each of the keymaps chained to @scheme[editor] must be an
            @scheme[keymap:aug-keymap%] and @scheme[keymap] cannot be the result
            of @scheme[(send editor get-keymap)] That is, @scheme[keymap] must
            be chained to some keymap attached to the editor.})
 
 (proc-doc/names
  scheme:text-balanced?
  (->* ((is-a?/c text%))
       (number? (or/c false/c number?))
       boolean?)
  ((text)
   ((start 0) (end #f)))
  @{Determines if the range in the editor from @scheme[start] to
               @scheme[end] in @scheme[text] is a matched set of parenthesis.  If
               @scheme[end] is @scheme[#f], it defaults to the last position of the
               @scheme[text].
               
               The implementation of this function creates a port with
               @scheme[open-input-text-editor] and then uses `read' to parse the
               range of the buffer.})
 
 (proc-doc/names
  scheme:add-preferences-panel
  (-> void?)
  ()
  @{Adds a tabbing preferences panel to the preferences dialog.})
 
 (proc-doc/names
  scheme:get-keymap
  (-> (is-a?/c keymap%))
  ()
  @{Returns a keymap with binding suitable for Scheme.})
 
 (proc-doc/names
  scheme:add-coloring-preferences-panel
  (-> any)
  ()
  @{
    Installs the ``Scheme'' preferences panel in the ``Syntax Coloring''
             section.})
 
 (proc-doc/names
  scheme:get-color-prefs-table
  (-> (listof (list/c symbol? (is-a?/c color%))))
  ()
  @{Returns 
    a table mapping from symbols 
    (naming the categories that the online colorer uses for Scheme mode coloring) to their colors.
    
    These symbols are suitable for input to
    @scheme[scheme:short-sym->pref-name] and
    @scheme[scheme:short-sym->style-name].
    
    See also @scheme[scheme:get-white-on-black-color-prefs-table].})
 
 (proc-doc/names
  scheme:get-white-on-black-color-prefs-table
  (-> (listof (list/c symbol? (is-a?/c color%))))
  ()
  @{Returns 
    a table mapping from symbols 
    (naming the categories that the online colorer uses for Scheme mode coloring) to their colors when
    the user chooses the white-on-black mode in the preferences dialog.
    
    See also @scheme[scheme:get-color-prefs-table].})
 
 (proc-doc/names
  scheme:short-sym->pref-name
  (symbol? . -> . symbol?)
  (short-sym)
  @{Builds the symbol naming the preference from one of the symbols in
           the table returned by @scheme[scheme:get-color-prefs-table].})
 
 (proc-doc/names
  scheme:short-sym->style-name
  (symbol? . -> . string?)
  (short-sym)
  @{Builds the symbol naming the editor style from one of the symbols in
           the table returned by @scheme[scheme:get-color-prefs-table].  This
           style is a named style in the style list returned by
           @scheme[editor:get-standard-style-list].})
 
 (proc-doc/names
  scheme:get-wordbreak-map
  (-> (is-a?/c editor-wordbreak-map%))
  ()
  @{This method returns a @link editor-wordbreak-map that is suitable
         for Scheme.})
 
 (proc-doc/names
  scheme:init-wordbreak-map
  ((is-a?/c keymap%) . -> . void?)
  (key)
  @{Initializes the workdbreak map for @scheme[keymap].})
 
 (proc-doc/names
  scheme:setup-keymap
  ((is-a?/c keymap%) . -> . void?)
  (keymap)
  @{Initializes @scheme[keymap] with Scheme-mode keybindings.})
 
 (proc-doc/names
  editor:set-default-font-color
  (-> (is-a?/c color%) void?)
  (color)
  @{Sets the color of the style named
         @scheme[editor:get-default-color-style-name].})
 
 (proc-doc/names
  editor:get-default-color-style-name
  (-> string?)
  ()
  @{The name of the style (in the list returned by
                              @scheme[editor:get-standard-style-list]) that holds the default
                                                                       color.})
 
 (proc-doc/names
  editor:set-standard-style-list-delta
  (string? (is-a?/c style-delta%) . -> . void?)
  (name delta)
  @{Finds (or creates) the style named by @scheme[name] in the result of
          @scheme[editor:get-standard-style-list] and sets its delta to
          @scheme[delta].
          
          If the style named by @scheme[name] is already in the style list, it
          must be a delta style.})
 
 (proc-doc/names
  editor:set-standard-style-list-pref-callbacks
  (-> any)
  ()
  @{Installs the font preference callbacks that update the style list
             returned by @scheme[editor:get-standard-style-list] based on the
             font preference symbols.})
 
 (proc-doc/names
  editor:get-standard-style-list
  (-> (is-a?/c style-list%))
  ()
  @{Returns a style list that is used for all instances of
            @scheme[editor:standard-style-list%].})
 
 (proc-doc/names
  editor:add-after-user-keymap
  (-> (is-a?/c keymap%) (listof (is-a?/c keymap%)) (listof (is-a?/c keymap%)))
  (keymap keymaps)
  @{Returns a list that contains all of the keymaps in @scheme[keymaps], in the
    same relative order, but also with @scheme[keymap], where @scheme[keymap]
    is now the first keymap after @scheme[keymap:get-user] (if that keymap is
    in the list.)})
 
 (proc-doc/names
  color-model:rgb->xyz
  (number? number? number? . -> . color-model:xyz?)
  (r g b)
  @{Converts a color represented as a red-green-blue tuple (each value
                                                                 from 0 to 255) into an XYZ tuple.  This describes a point in the CIE
                                                                                XYZ color space.})
 
 (proc-doc/names
  color-model:rgb-color-distance
  (number? number? number? number? number? number? . -> . number?)
  (red-a green-a blue-a red-b green-b blue-b)
  @{This calculates a distance between two colors.  The smaller the
         distance, the closer the colors should appear to the human eye.  A
         distance of 10 is reasonably close that it could be called the same
         color.
         
         This function is not symmetric in red, green, and blue, so it is
         important to pass red, green, and blue components of the colors in
         the the proper order.  The first three arguments are red, green and
         blue for the first color, respectively, and the second three
         arguments are red green and blue for the second color,
         respectively.})
 
 (proc-doc/names
  color-model:xyz->rgb
  (number? number? number? . -> . (list/c number? number? number?))
  (x y z)
  @{Converts an XYZ-tuple (in the CIE XYZ colorspace) into a list of
             values representing an RGB-tuple.})
 
 (proc-doc/names
  color-model:xyz?
  (any/c . -> . boolean?)
  (val)
  @{Determines if @scheme[val] an xyz color record.})
 
 (proc-doc/names
  color-model:xyz-x
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the x component of @scheme[xyz].})
 
 (proc-doc/names
  color-model:xyz-y
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the y component of @scheme[xyz].})
 
 (proc-doc/names
  color-model:xyz-z
  (color-model:xyz? . -> . number?)
  (xyz)
  @{Extracts the z component of @scheme[xyz].})
 
 (proc-doc/names
  color-prefs:set-default/color-scheme
  (-> symbol?
      (or/c (is-a?/c color%) string?)
      (or/c (is-a?/c color%) string?)
      void?)
  (pref-sym black-on-white-color white-on-black-color)
  @{Registers a preference whose value will be updated when the user
              clicks on one of the color scheme default settings in the
              preferences dialog.
              
              Also calls @scheme[preferences:set-default] and
              @scheme[preferences:set-un/marshall] with appropriate arguments to
              register the preference.})
 
 (proc-doc/names
  color-prefs:register-color-preference
  (->* (symbol? string? (or/c (is-a?/c color%) (is-a?/c style-delta%)))
       ((or/c string? (is-a?/c color%) false/c))
       void?)
  ((pref-name style-name color/sd)
   ((white-on-black-color #f)))
  @{This function registers a color preference and initializes the style
         list returned from @scheme[editor:get-standard-style-list].  In
         particular, it calls @scheme[preferences:set-default] and
         @scheme[preferences:set-un/marshall] to install the pref for
         @scheme[pref-name], using @scheme[color/sd] as the default
         color.  The preference is bound to a @scheme[style-delta%], and
         initially the @scheme[style-delta%] changes the foreground color to
         @scheme[color/sd], unless @scheme[color/sd] is a style delta
         already, in which case it is just used directly.  Then, it calls
         @scheme[editor:set-standard-style-list-delta] passing the
         @scheme[style-name] and the current value of the preference
         @scheme[pref-name].
         
         Finally, it adds calls @scheme[preferences:add-callback] to set a
         callback for @scheme[pref-name] that updates the style list when the
         preference changes.
         
         If @scheme[white-on-black-color] is not @scheme[#f], then the color
         of the @scheme[color/sd] argument is used in combination with
         @scheme[white-on-black-color] to register this preference with
         @scheme[color-prefs:set-default/color-scheme].})
 
 (proc-doc/names
  color-prefs:add-background-preferences-panel
  (-> void?)
  ()
  @{Adds a preferences panel that configures the background color for
         @scheme[editor:basic-mixin].})
 
 (proc-doc/names
  color-prefs:add-to-preferences-panel
  (string? ((is-a?/c vertical-panel%) . -> . void?) . -> . void?)
  (name func)
  @{Calls @scheme[func] with the subpanel of the preferences coloring
          panel that corresponds to @scheme[name].})
 
 (proc-doc/names
  color-prefs:build-color-selection-panel
  ((is-a?/c area-container<%>) symbol? string? string? . -> . void?)
  (parent pref-sym style-name example-text)
  @{Builds a panel with a number of controls for configuring a font: the
           color and check boxes for bold, italic, and underline.  The
           @scheme[parent] argument specifies where the panel will be placed.
           The @scheme[pref-sym] should be a preference
           (suitable for use with @scheme[preferences:get] and @scheme[preferences:set]).  
           The
           @scheme[style-name] specifies the name of a style in the style list
           returned from @scheme[editor:get-standard-style-list] and
           @scheme[example-text] is shown in the panel so users can see the
           results of their configuration.})
 
 (proc-doc/names
  color-prefs:marshall-style-delta
  (-> (is-a?/c style-delta%) printable/c)
  (style-delta)
  @{Builds a printed representation for a style-delta.})
 
 (proc-doc/names
  color-prefs:unmarshall-style-delta
  (-> printable/c (or/c false/c (is-a?/c style-delta%)))
  (marshalled-style-delta)
  @{Builds a style delta from its printed representation.  Returns
           @scheme[#f] if the printed form cannot be parsed.})
 
 (proc-doc/names
  color-prefs:white-on-black
  (-> any)
  ()
  @{Sets the colors registered by @scheme[color-prefs:register-color-preference] to their white-on-black variety. })
 
 (proc-doc/names
  color-prefs:black-on-white
  (-> any)
  ()
  @{Sets the colors registered by @scheme[color-prefs:register-color-preference] to their black-on-white variety. }))
