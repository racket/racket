#lang scribble/doc 
@(require scribble/manual scribble/extract
          (for-label framework scheme/gui)
          framework/private/gen-standard-menus)

@title{Frame}

@definterface[frame:basic<%> (frame%)]{
  Classes matching this interface support the basic 
  @racket[frame%]
  functionality required by the framework.
  @defmethod*[(((get-area-container%) (implementation?/c area-container<%>)))]{
    The class that this method returns is used to create the
    @racket[area-container<%>]
    in this frame.

  }
  @defmethod*[(((get-area-container) (is-a?/c area-container<%>)))]{
    This returns the main 
    @racket[area-container<%>]
    in the frame

  }
  @defmethod*[(((get-menu-bar%) (subclass?/c menu-bar%)))]{
    The result of this method is used to create the initial menu bar for
    this frame.

    Return @racket[menu-bar%].
  }
  @defmethod*[(((make-root-area-container (class (implementation?/c area-container<%>))
                                          (parent (is-a?/c area-container<%>)))
                (is-a?/c area-container<%>)))]{
    Override this method to insert a panel in between the panel used by
    the clients of this frame and the frame itself. For example, to insert
    a status line panel override this method with something like this:

    @racketblock[
    (class ...
      ...
      (define status-panel #f)
      (define/override (make-root-area-container cls parent)
        (set! status-panel
              (super make-root-area-container vertical-panel% parent))
        (let ([root (make-object cls status-panel)])

           (code:comment "... add other children to status-panel ...")

           root))
      ...)]

    In this example, status-panel will contain a root panel for the other
    classes, and whatever panels are needed to display status information.

    The searching frame is implemented using this method.

    Calls @racket[make-object] with @racket[class] and @racket[parent].
  }
  @defmethod*[(((close) void?))]{
    This method closes the frame by calling the
    @method[top-level-window<%> can-close?],
    @method[top-level-window<%> on-close], and
    @method[top-level-window<%> show]
    methods. 

    It's implementation is:
    @racketblock[
      (inherit can-close? on-close)
      (public
        [show
          (lambda ()
            (when (can-close?)
              (on-close)
              (show #f)))])]

  }
  @defmethod*[(((editing-this-file? (filename path?)) boolean?))]{
    Indicates if this frame contains this buffer (and can edit
    that file).

    Returns @racket[#f].
  }
  @defmethod*[(((get-filename (temp (or/c #f (box boolean?)) #f)) (or/c #f path?)))]{
    This returns the filename that the frame is currently being saved as,
    or @racket[#f] if there is no appropriate filename.

    Returns @racket[#f] by default.

    If @racket[temp] is a box, it is filled with @racket[#t] or @racket[#f],
    depending if the filename is a temporary filename.
  }
  @defmethod*[(((make-visible (filename string?)) void?))]{
    Makes the file named by @racket[filename] visible (intended for
    use with tabbed editing).

  }
}
@defmixin[frame:basic-mixin (frame%) (frame:basic<%>)]{
  This mixin provides the basic functionality that the framework expects. It
  helps manage the list of frames in the @racket[group:%] object returned by
  @racket[group:get-the-frame-group].

  Do not give @racket[panel%] or @racket[control<%>] objects this frame as
  parent. Instead, use the result of the @method[frame:basic<%>
  get-area-container] method.

  @index{Windows menu}

  This mixin also creates a menu bar for the frame, as the
  frame is initialized. It uses the class returned by
  @method[frame:basic<%> get-menu-bar%]. It only passes the frame as an
  initialization argument.  In addition, it creates the windows menu in the
  menu bar.

  This mixin calls its @method[window<%> accept-drop-files] with @racket[#t].

  It also calls its @method[top-level-window<%> set-icon] method according to the current
  value of @racket[frame:current-icon].

  See also @racket[frame:reorder-menus].

  @defmethod*[#:mode override (((show (on? boolean?)) void?))]{

    Calls the super method.

    When @racket[on?] is @racket[#t], inserts the frame into the
    frame group and when it is @racket[#f], removes the frame
    from the group.
  }
  @defmethod*[#:mode override (((can-exit?) boolean?))]{

    This, together with @method[frame:basic-mixin on-exit] mimics
    @racket[exit:exit].

    First, it calls @racket[exit:set-exiting] with @racket[#t].  Then, it calls
    @racket[exit:can-exit?]. If it returns @racket[#t], so does this method. If
    it returns @racket[#f], this method calls @racket[exit:set-exiting] with
    @racket[#f].
  }
  @defmethod*[#:mode override (((on-exit) void?))]{

    Together with @method[frame:basic-mixin can-exit?]  this mimics the
    behavior of @racket[exit:exit].

    Calls @racket[exit:on-exit] and then queues a callback to call Racket's
    @racket[exit] function. If that returns, it calls @racket[exit:set-exiting]
    to reset that flag to @racket[#f].
  }
  @defmethod*[#:mode override (((on-superwindow-show (shown? any/c)) void?))]{

    Notifies the result of @racket[(group:get-the-frame-group)] that a frame
    has been shown, by calling the @method[group:% frame-shown/hidden] method.

  }
  @defmethod*[#:mode override (((on-drop-file (pathname string?)) void?))]{

    Calls @racket[handler:edit-file] with @racket[pathname] as an argument.
  }
  @defmethod*[#:mode override (((after-new-child) void?))]{

    Raises an exception if attempting to add a child to this frame (except if
    using the @method[frame:basic<%> make-root-area-container] method).
  }
}

@definterface[frame:focus-table<%> (top-level-window<%>)]{}

@defmixin[frame:focus-table-mixin (frame%) (frame:focus-table<%>)]{

  Instances of classes returned from this mixin track how frontmost they are
  based on calls made to methods at the Racket level, instead of using
  the calls made by the operating system as it tracks the focus.
  
  See also @racket[frame:lookup-focus-table], @racket[test:use-focus-table]
  and @racket[test:get-active-top-level-window].
                                                         
  @defmethod[#:mode override (show [on? boolean?]) void?]{
    When @racket[on?] is @racket[#t], adds this frame to the 
    front of the list of frames stored with the frame's eventspace. When 
    @racket[on?] is @racket[#f], this method removes this frame
    from the list.

    See also @racket[frame:lookup-focus-table], @racket[test:use-focus-table]
    and @racket[test:get-active-top-level-window].
  }
  @defmethod[#:mode augment (on-close) void?]{
    Removes this frame from the list of frames stored with the frame's eventspace.
    
    See also @racket[frame:lookup-focus-table], @racket[test:use-focus-table]
    and @racket[test:get-active-top-level-window].
  }
}
                                                     
@definterface[frame:size-pref<%> (frame:basic<%>)]{
  @defmethod[(adjust-size-when-monitor-setup-changes?) boolean?]{
     Determines if the frame's size should be automatically adjusted
     when the monitors configuration changes.
     
     Defaults to returning @racket[#f].
   }
}
@defmixin[frame:size-pref-mixin (frame:basic<%>) (frame:size-pref<%>)]{
  @defconstructor/auto-super[([size-preferences-key symbol?]
                              [position-preferences-key (or/c symbol? #f) #f]
                              [width (or/c dimension-integer? #f) #f]
                              [height (or/c dimension-integer? #f) #f]
                              [x (or/c position-integer? #f) #f]
                              [y (or/c position-integer? #f) #f])]{

    The  @racket[size-preferences-key] symbol is used with
    @racket[preferences:get] and @racket[preferences:set] to track the current
    size.
    
    If present, the @racket[position-preferences-key] symbol is used with
    @racket[preferences:get] and @racket[preferences:set] to track the current
    position.

    Both preferences are tracked on a per-monitor-configuration basis. That is,
    the preference value saved is a mapping from the current monitor configuration
    (derived from the results of @racket[get-display-count], @racket[get-display-left-top-inset],
    and @racket[get-display-size]).
    
    Passes the @racket[x], @racket[y], and @racket[width] and @racket[height] 
    initialization arguments to the superclass and calls @method[frame% maximize]
    based on the current values of the preferences.

    See also @racket[frame:setup-size-pref].

  }

  @defmethod[#:mode override (on-size (width dimension-integer?) 
                                      (height dimension-integer?))
                    void?]{
    Updates the preferences, according to the width and
    height. The preferences key is the one passed
    to the initialization argument of the class.
  }
  @defmethod[#:mode override (on-move (width position-integer?)
                                      (height position-integer?))
                    void?]{
    Updates the preferences according to the width and
    height, if @racket[position-preferences-key] is not @racket[#f], using
    it as the preferences key.
  }
}

@definterface[frame:register-group<%> ()]{
  Frames that implement this interface are registered with the group. See
  @racket[group:get-the-frame-group] and @racket[frame:register-group-mixin].
}
@defmixin[frame:register-group-mixin (frame:basic<%>) (frame:register-group<%>)]{
  During initialization, calls
  @method[group:% insert-frame]with @racket[this].
  @defmethod*[#:mode augment (((can-close?) boolean?))]{

    Calls the inner method, with a default of @racket[#t].  If that returns
    @racket[#t], it checks for one of the these three conditions:

    @itemize[
    @item{@racket[exit:exiting?] returns @racket[#t]}
    @item{there is more than one frame in the group returned by
      @racket[group:get-the-frame-group], or}
    @item{the procedure @racket[exit:user-oks-exit] returns @racket[#t].}]

    If any of those conditions hold, the method returns @racket[#t].
  }
  @defmethod*[#:mode augment (((on-close) void?))]{

    First calls the inner method.  Next, calls the @method[group:%
    remove-frame] method of the result of @racket[group:get-the-frame-group]
    with @racket[this] as an argument.  Finally, unless @racket[exit:exiting?]
    returns @racket[#t], and if there are no more frames open, it calls
    @racket[exit:exit].
  }
  @defmethod*[#:mode override (((on-activate (on? boolean?)) void?))]{

    Calls @method[group:% set-active-frame] with @racket[this] when
    @racket[on?] is true.

  }
}
@definterface[frame:status-line<%> (frame:basic<%>)]{
  The mixin that implements this interface provides an interface to a set of
  status lines at the bottom of this frame.

  Each status line must be opened with @method[frame:status-line<%>
  open-status-line] before any messages are shown in the status line and once
  @method[frame:status-line<%> close-status-line] is called, no more messages
  may be displayed, unless the status line is re-opened.

  The screen space for status lines is not created until
  @method[frame:status-line<%> update-status-line] is called with a
  string. Additionally, the screen space for one status line is re-used when by
  another status line when the first passes @racket[#f] to
  @method[frame:status-line<%> update-status-line]. In this manner, the status
  line frame avoids opening too many status lines and avoids flashing the
  status lines open and closed too often.

  @defmethod*[(((open-status-line (id symbol?)) void?))]{
    Creates a new status line identified by the symbol
    argument. The line will not appear in the frame until a
    message is put into it, via
    @method[frame:status-line<%> update-status-line].
  }

  @defmethod*[(((close-status-line (id symbol?)) void?))]{
    Closes the status line @racket[id].
  }
  @defmethod*[(((update-status-line (id symbol?) (status (or/c #f string?))) void?))]{
    Updates the status line named by @racket[id] with @racket[status]. If
    @racket[status] is @racket[#f], the status line is becomes blank (and may
    be used by other ids).
  }
}
@defmixin[frame:status-line-mixin (frame:basic<%>) (frame:status-line<%>)]{

  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c panel%)) 
                                                          (parent (is-a?/c panel%)))
                                (is-a?/c panel%)))]{

    Adds a panel at the bottom of the frame to hold the status
    lines.

  }
}
@definterface[frame:info<%> (frame:basic<%>)]{
  Frames matching this interface support a status line.

  The preference @racket['framework:show-status-line] controls the visibility
  of the status line. If it is @racket[#t], the status line is visible and if
  it is @racket[#f], the status line is not visible (see
  @racket[preferences:get] for more info about preferences)

  @defmethod*[(((determine-width (str string) (canvas (is-a?/c editor-canvas%)) (text (is-a?/c text%))) integer))]{
    This method is used to calculate the size of an @racket[editor-canvas%]
    with a particular set of characters in it.  It is used to calculate the
    sizes of the edits in the status line.
  }

  @defmethod*[(((lock-status-changed) void?))]{
    This method is called when the lock status of the @racket[editor<%>]
    changes.

    Updates the lock icon in the status line panel.
  }
  @defmethod*[(((update-info) void?))]{
    This method updates all of the information in the panel.
  }
  @defmethod*[(((set-info-canvas (canvas (or/c (is-a?/c canvas:basic%) #f))) void?))]{
    Sets this canvas to be the canvas that the info frame shows info about. The
    @method[canvas:info-mixin% on-focus] and @method[canvas:info-mixin%
    set-editor] methods call this method to ensure that the info canvas is set
    correctly.
  }

  @defmethod*[(((get-info-canvas) (or/c (is-a?/c canvas:basic%) #f)))]{
    Returns the canvas that the @racket[frame:info<%>] currently shows info
    about. See also @method[frame:info<%> set-info-canvas]
  }

  @defmethod*[(((get-info-editor) (or/c #f (is-a?/c editor<%>))))]{
    Override this method to specify the editor that the status line
    contains information about.

    Returns the result of @method[frame:editor<%> get-editor].
  }
  @defmethod*[(((get-info-panel) (is-a?/c horizontal-panel%)))]{
    This method returns the panel where the information about this editor is
    displayed.
  }

  @defmethod*[(((show-info) void?))]{
    Shows the info panel.

    See also @method[frame:info<%> is-info-hidden?].
  }

  @defmethod*[(((hide-info) void?))]{
    Hides the info panel.

    See also @method[frame:info<%> is-info-hidden?].
  }
  @defmethod*[(((is-info-hidden?) boolean?))]{
    Result indicates if the show info panel has been explicitly hidden with
    @method[frame:info<%> hide-info].

    If this method returns @racket[#t] and @racket[(preferences:get
    'framework:show-status-line)] is @racket[#f], then the info panel will not
    be visible.  Otherwise, it is visible.
  }
}
@defmixin[frame:info-mixin (frame:basic<%>) (frame:info<%>)]{
  This mixin provides support for displaying various info in the status line of
  the frame.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.

  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c area-container<%>))
                                                          (parent (is-a?/c area-container<%>)))
                                (is-a?/c area-container<%>)))]{

    Builds an extra panel for displaying various information.
  }
  @defmethod*[#:mode augment (((on-close) void?))]{
    Removes the GC icon with @racket[unregister-collecting-blit] and cleans up
    other callbacks.
  }

}
@definterface[frame:text-info<%> (frame:info<%>)]{
  Objects matching this interface receive information from editors constructed
  with @racket[editor:info-mixin] and display it.

  @defmethod*[(((set-macro-recording (on? boolean?)) void?))]{
    Shows/hides the icon in the info bar that indicates if a macro recording is
    in progress.
  }

  @defmethod*[(((overwrite-status-changed) void?))]{
    This method is called when the overwrite mode is turned either on or off in
    the @racket[editor<%>] in this frame.
  }

  @defmethod*[(((anchor-status-changed) void?))]{
    This method is called when the anchor is turned either on or off in the 
    @racket[editor<%>]
    in this frame.
  }

  @defmethod*[(((editor-position-changed) void?))]{
    This method is called when the position in the @racket[editor<%>] changes.
  }

  @defmethod[(add-line-number-menu-items [menu (is-a?/c menu-item-container<%>)]) void?]{
    This method is called when the line/column display in the info bar is
    clicked.  It is passed a @racket[menu-item-container<%>] that can be filled
    in with menu items; those menu items will appear in the menu that appears
    when line/colun display is clicked.
  }
}

@defmixin[frame:text-info-mixin (frame:info<%>) (frame:text-info<%>)]{
  This mixin adds status information to the info panel relating to an
  edit.

  @defmethod*[#:mode augment (((on-close) void?))]{

    removes a preferences callback for @racket['framework:line-offsets].
    See @racket[preferences:add-callback] for more information.
  }
  @defmethod*[#:mode override (((update-info) void?))]{

    Calls
    @method[frame:text-info<%> overwrite-status-changed],
    @method[frame:text-info<%> anchor-status-changed], and
    @method[frame:text-info<%> editor-position-changed].
  }
}
@definterface[frame:pasteboard-info<%> (frame:info<%>)]{

}
@defmixin[frame:pasteboard-info-mixin (frame:basic<%>) (frame:pasteboard-info<%>)]{

}

@(generate-standard-menus-docs)

@defmixin[frame:standard-menus-mixin (frame:basic<%>) (frame:standard-menus<%>)]{
  The result of this mixin implements @racket[frame:standard-menus<%>].

  @defmethod*[#:mode augment (((on-close) void?))]{
    Removes the preferences callbacks for the menu items
  }
}

@definterface[frame:editor<%> (frame:standard-menus<%>)]{
  Frame classes matching this interface support embedded editors.
  @defmethod*[(((get-entire-label) string))]{
    This method returns the entire label for the frame.  See also
    @method[window<%> set-label] and @method[frame:editor<%> set-label-prefix].
  }

  @defmethod*[(((get-label-prefix) string?))]{
    This returns the prefix for the frame's label.
  }

  @defmethod*[(((set-label-prefix (prefix string?)) void?))]{
    Sets the prefix for the label of the frame.
  }

  @defmethod*[(((get-canvas%) (subclass?/c editor-canvas%)))]{
    The result of this method is used to create the canvas for the
    @racket[editor<%>] in this frame.

    Returns @racket[editor-canvas%].
  }

  @defmethod*[(((get-canvas<%>) (is-a?/c canvas:basic%)))]{
    The result of this method is used to guard the result of the
    @method[frame:editor<%> get-canvas%] method.
  }

  @defmethod*[(((get-editor%) (implementation?/c editor<%>)))]{
    The result of this class is used to create the @racket[editor<%>] in this
    frame.

    Override this method to specify a different editor class.

    Returns the value of the init-field @racket[editor%].
  }

  @defmethod*[(((get-editor<%>) interface?))]{
    The result of this method is used by @method[frame:editor<%> make-editor]
    to check that @method[frame:editor<%> get-editor%] is returning a
    reasonable editor.

    Returns @racket[editor<%>].
  }

  @defmethod*[(((make-editor) (is-a?/c editor<%>)))]{
    This method is called to create the editor in this frame.  It calls
    @method[frame:editor<%> get-editor<%>] and uses that interface to make sure
    the result of @method[frame:editor<%> get-editor%] is reasonable.

    Calls @racket[(make-object @#,method[frame:editor<%> get-editor%])].
  }

  @defmethod*[(((revert) void?))]{
    Loads the most recently saved version of the file to the disk. If the
    @racket[editor<%>] is a @racket[text%], the start and end positions are
    restored.
  }

  @defmethod*[(((save (format (or/c 'guess 'standard 'text 'text-force-cr 'same 'copy) 'same))
                boolean?))]{
    Saves the file being edited, possibly calling
    @method[frame:editor<%> save-as]
    if the editor has no filename yet.

    Returns @racket[#f] if the user cancels this operation (only possible when
    the file has not been saved before and the user is prompted for a new
    filename) and returns @racket[#t] if not.
  }

  @defmethod*[(((save-as (format (or/c 'guess 'standard 'text 'text-force-cr 'same 'copy) 'same))
                boolean?))]{
    Queries the use for a file name and saves the file with that name.

    Returns @racket[#f] if the user cancells the file-choosing
    dialog and returns @racket[#t] otherwise.
  }
  @defmethod*[(((get-canvas) (is-a?/c canvas%)))]{
    Returns the canvas used to display the 
    @racket[editor<%>]
    in this frame.


  }
  @defmethod*[(((get-editor) (is-a?/c editor<%>)))]{
    Returns the editor in this frame.
  }
}
@defmixin[frame:editor-mixin (frame:standard-menus<%>) (frame:editor<%>)]{
  This mixin adds functionality to support an 
  @racket[editor<%>]
  in the frame. This
  includes management of the title, implementations of some of the menu
  items, a reasonable initial size, and access to the 
  @racket[editor<%>]
  itself.

  The size of this frame with be either 600 by 650 or 65 less than the
  width and height of the screen, whichever is smaller.

  @defconstructor[((filename string?)
                   (editor% (implementation?/c editor:basic<%>))
                   (parent (or/c (is-a?/c frame%) false/c) #f)
                   (width (or/c dimension-integer? false/c) #f)
                   (height (or/c dimension-integer? false/c) #f)
                   (x (or/c position-integer? false/c) #f)
                   (y (or/c position-integer? false/c) #f)
                   (style (listof (or/c 'no-resize-border
                                        'no-caption
                                        'no-system-menu
                                        'hide-menu-bar
                                        'mdi-parent
                                        'mdi-child
                                        'toolbar-button
                                        'float
                                        'metal))
                          null)
                   (enabled any/c #t)
                   (border spacing-integer? 0)
                   (spacing spacing-integer? 0)
                   (alignment (list/c (or/c 'left 'center 'right) (or/c 'top 'center 'bottom)) '(center top))
                   (min-width dimension-integer? graphical-minimum-width)
                   (min-height dimension-integer? graphical-minimum-height)
                   (stretchable-width any/c #t)
                   (stretchable-height any/c #t))]{

  }
  @defmethod*[#:mode override (((get-filename) (or/c #f path?)))]{
    Returns the filename in the editor returned by
    @method[frame:editor<%> get-editor].
  }

  @defmethod*[#:mode override (((editing-this-file? (filename path?)) boolean?))]{
    Returns @racket[#t] if the filename is the file that this frame is editing.
  }

  @defmethod*[#:mode augment (((on-close) void?))]{
    Calls the @racket[editor:basic<%>]'s method @method[editor:basic<%>
    on-close].
  }

  @defmethod*[#:mode augment (((can-close?) void?))]{
    Calls the @racket[editor:basic<%>]'s method @method[editor:basic<%>
    can-close?].
  }

  @defmethod*[#:mode override (((get-label) string?))]{
    Returns the portion of the label after the hyphen. See also
    @method[frame:editor<%> get-entire-label].
  }

  @defmethod*[#:mode override (((set-label (label string?)) void?))]{
    Sets the label, but preserves the label's prefix. See also
    @method[frame:editor<%> set-label-prefix].
  }

  @defmethod*[#:mode override (((file-menu:open-callback (item (is-a?/c menu-item<%>)) (evt (is-a?/c mouse-event%))) void?))]{
    Calls @racket[handler:open-file] with the directory of the saved file
    associated with this editor (if any).
  }

  @defmethod*[#:mode override (((file-menu:revert-on-demand) void?))]{
    Disables the menu item when the editor is locked.
  }

  @defmethod*[#:mode override (((file-menu:revert-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void?))]{
    Informs the user that this action is not undoable and, if they still want
    to continue, calls @method[frame:editor<%> revert].
  }
  @defmethod*[#:mode override (((file-menu:create-revert?) boolean?))]{
    returns @racket[#t].
  }
  @defmethod*[#:mode override (((file-menu:save-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void?))]{
    Saves the file in the editor.
  }

  @defmethod*[#:mode override (((file-menu:create-save?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((file-menu:save-as-callback (item (is-a?/c menu-item%))
                                                            (evt (is-a?/c control-event%)))
                                void?))]{
    Prompts the user for a file name and uses that filename to save the buffer.
    Calls @method[frame:editor<%> save-as] with no arguments.
  }

  @defmethod*[#:mode override (((file-menu:create-save-as?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((file-menu:print-callback (item (is-a?/c menu-item%)) 
                                                          (evt (is-a?/c control-event%)))
                                void?))]{
    Calls the @method[editor<%> print] method of @racket[editor<%>] with the
    default arguments, except that the @racket[output-mode] argument is the
    result of calling @racket[preferences:get] with
    @racket['framework:print-output-mode].
  }

  @defmethod*[#:mode override (((file-menu:create-print?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((file-menu:between-save-as-and-print (file-menu (is-a?/c menu%))) void?))]{
    Creates a Print Setup menu item if @racket[can-get-page-setup-from-user?]
    and @racket[file-menu:create-print?] both return true.
  }

  @defmethod*[#:mode override (((edit-menu:between-select-all-and-find (edit-menu (is-a?/c menu%))) void?))]{
    Adds a menu item for toggling @method[editor<%> auto-wrap] in the focused
    text.
  }

  @defmethod*[#:mode override (((help-menu:about-callback (item (is-a?/c menu-item%)) 
                                                          (evt (is-a?/c control-event%)))
                                void?))]{
    Calls @racket[message-box] with a message welcoming the user to the
    application named by @racket[application:current-app-name]
  }

  @defmethod*[#:mode override (((help-menu:about-string) string))]{
    Returns the result of (@racket[application:current-app-name])
  }

  @defmethod*[#:mode override (((help-menu:create-about?) boolean?))]{
    returns @racket[#t].
  }
}

@definterface[frame:text<%> (frame:editor<%>)]{
  Frames matching this interface provide support for @racket[text%]s.
}

@defmixin[frame:text-mixin (frame:editor<%>) (frame:text<%>)]{
  This mixins adds support for @racket[text%]s in the frame.

  @defconstructor[((editor% (extends text%)))]{
    Calls the super initialization with either the value of the
    @racket[editor%] init or, if none was supplied, it passes @racket[text%].
  }

  @defmethod*[#:mode override (((get-editor<%>) interface))]{
    Returns @racket[(class->interface text%)].
  }
}

@definterface[frame:pasteboard<%> (frame:editor<%>)]{
  Frames matching this interface provide support for 
  @racket[pasteboard%]s.
}

@defmixin[frame:pasteboard-mixin (frame:editor<%>) (frame:pasteboard<%>)]{
  This mixin provides support for pasteboards in a frame.

  @defconstructor[((editor% (extends pasteboard%)))]{
    Calls the super initialization with either the value of the
    @racket[editor%] init or, if none was supplied, it passes
    @racket[pasteboard%].
  }

  @defmethod*[#:mode override (((get-editor<%>) interface))]{
    Returns @racket[(class->interface pasteboard%)].
  }
}

@definterface[frame:delegate<%> (frame:status-line<%> frame:text<%>)]{
  Frames that implement this interface provide a 20,000 feet overview of the
  text in the main editor. The term @bold{delegate} in these method
  descriptions refers to the original editor and the term @bold{delegatee}
  refers to the editor showing the 20,000 feet overview.

  @defmethod[(get-delegated-text) (or/c #f (is-a?/c text:delegate<%>))]{
    Returns the current delegate text, if any.
  }
  
  @defmethod[(set-delegated-text [d (or/c #f (is-a?/c text:delegate<%>))]) 
             void?]{
    Sets the delegate text to @racket[d].
  }

  @defmethod*[(((delegated-text-shown?) boolean?))]{
    Returns @racket[#t] if the delegate is visible, and @racket[#f] if it
    isn't.
  }

  @defmethod*[(((hide-delegated-text) void?))]{
    Hides the delegated text.

    When the delegated text is hidden, it is not being updated. This is
    accomplished by calling the @method[text:delegate<%> set-delegate] method
    of @method[frame:editor<%> get-editor]with @racket[#f].

    See also @method[frame:delegate<%> show-delegated-text]
  }
  @defmethod*[(((show-delegated-text) void?))]{
    Makes the delegated text visible.

    When the delegated text is shown, the @method[text:delegate<%>
    set-delegate] method of @method[frame:delegate<%> get-delegated-text]is
    called with the text to delegate messages to.

    See also @method[frame:delegate<%> hide-delegated-text].
  }

  @defmethod*[(((delegate-moved) void?))]{
    This method is called when the visible region of the delegate editor
    changes, so that the blue region in the delegatee is updated.
  }
}

@defmixin[frame:delegate-mixin (frame:status-line<%> frame:text<%>) (frame:delegate<%>)]{
  Adds support for a 20,000-feet view via @racket[text:delegate<%>] and
  @racket[text:delegate-mixin].

  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c panel%))
                                                          (parent (is-a?/c panel%)))
                                (is-a?/c panel%)))]{
    Adds a panel outside to hold the delegate @racket[editor-canvas%] and
    @racket[text%].
  }

  @defmethod*[#:mode override (((get-editor<%>) interface))]{
    Returns @racket[text:delegate<%>].
  }

  @defmethod*[#:mode override (((get-editor%) (is-a?/c text:delegate<%>)))]{
    returns the super result, with the @racket[text:delegate-mixin] mixed in.
  }
}

@definterface[frame:searchable<%> (frame:basic<%>)]{
  Frames that implement this interface support searching.

  @defmethod[(search (direction (symbols 'forward 'backward))) void?]{
    Searches for the text in the search edit in the result of
    @method[frame:searchable<%> get-text-to-search].

    If the text is found and it sets the selection to the found text.
  }

  @defmethod[(search-replace) boolean?]{
    If there is a dark purple bubble (ie, if the replace portion of the search
    bar is visible and there is a search hit after the insertion point), then
    this will replace it with the contents of the replace editor and move the
    insertion point to just after that, or to the end of the editor (if there
    are no more search hits after the insertion point, but there are search
    hits before it).
  }

  @defmethod[(replace-all) void?]{
    Loops through the text from the beginning to the end, replacing all
    occurrences of the search string with the contents of the replace edit.
  }

  @defmethod[(get-text-to-search) (is-a?/c text%)]{
    Returns the last value passed to
    @method[frame:searchable<%> set-text-to-search].
  }

  @defmethod[(set-text-to-search [txt (or/c false/c (is-a?/c (subclass?/c text%)))]) void?]{
    Sets the current text to be searched.
  }

  @defmethod[(search-hidden?) boolean?]{
    Returns @racket[#t] if the search subwindow is visiable and @racket[#f]
    otherwise.
  }

  @defmethod[(hide-search) void?]{
    This method hides the searching information on the bottom of the frame.
  }

  @defmethod[(unhide-search [move-focus? boolean?]
                            [#:new-search-string-from-selection? new-search-string-from-selection? boolean? #f])
             void?]{
    When the searching sub window is hidden, makes it visible. If
    @racket[move-focus?] is @racket[#f], the focus is not moved, but if it is
    any other value, the focus is moved to the find window.
    
    If @racket[new-search-string-from-selection?] is a true value and the selection
    in the result of @method[frame:searchable<%> get-text-to-search] is not empty, 
    then the search editor is replaced with the selection.
  }

  @defmethod[(unhide-search-and-toggle-focus
              [#:new-search-string-from-selection? new-search-string-from-selection? boolean? #f])
             void?]{
     Like @method[frame:searchable<%> unhide-search], except it also moves the focus into the
          text to be searched, or into the search string text, depending on where it 
          currently is.
  }
                   
  @defmethod[(get-case-sensitive-search?) boolean?]{
    Returns @racket[#t] if the search is currently case-sensitive.  (This
    method's value depends on the preference
    @racket['framework:case-sensitive-search?], but the preference is only
    consulted when the frame is created.)
  }

  @defmethod[#:mode public-final (search-hits-changed) void?]{
    This method is called when the number of search matches changes and it
    updates the GUI.
  }
}

@defmixin[frame:searchable-mixin (frame:standard-menus<%>) (frame:searchable<%>)]{
  This mixin adds support for searching in the @racket[editor<%>] in this
  frame.

  @defmethod*[#:mode override (((edit-menu:find-callback) boolean?))]{
     Toggles the focus between the find window and the window being searched.
     When moving to the window with the search string, selects the entire range
     in the buffer.
  }

  @defmethod*[#:mode override (((edit-menu:create-find?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override
              (((edit-menu:find-again-callback (item (is-a?/c menu-item%))
                                               (evt (is-a?/c control-event%)))
                void?))]{
    Calls @method[frame:searchable unhide-search] and then
    @method[frame:searchable<%> search].
  }

  @defmethod*[#:mode override (((edit-menu:create-find-again?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((edit-menu:find-again-backwards-callback
                                 (item (is-a?/c menu-item%))
                                 (evt (is-a?/c control-event%)))
                                void?))]{
    Calls @method[frame:searchable unhide-search] and then
    @method[frame:searchable<%> search].
  }

  @defmethod*[#:mode override (((edit-menu:create-find-again-backwards?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((edit-menu:replace-all-callback) boolean?))]{
    Calls @method[frame:searchable<%> replace-all].
  }

  @defmethod*[#:mode override (((edit-menu:replace-all-on-demand (item menu-item%)) void?))]{
    Disables @racket[item] when @method[frame:searchable<%> search-hidden?]
    returns @racket[#t] and enables it when that method returns @racket[#f].
  }

  @defmethod*[#:mode override (((edit-menu:create-replace-all?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((edit-menu:find-case-sensitive-callback) boolean?))]{
     Updates the state of the case-sensitive searching for this frame, and sets
     the @racket['framework:case-sensitive-search?] preference for later
     frames.
  }

  @defmethod*[#:mode override (((edit-menu:find-case-sensitive-on-demand (item menu-item%)) void?))]{
    Checks @racket[item] when searching is case-sensitive and unchecks it
    otherwise.
  }

  @defmethod*[#:mode override (((edit-menu:create-find-case-sensitive?) boolean?))]{
    returns @racket[#t].
  }

  @defmethod*[#:mode override (((make-root-area-container) (is-a?/c area-container<%>)))]{
    Builds a panel for the searching information.
  }

  @defmethod*[#:mode augment (((on-close) void?))]{
    Cleans up after the searching frame.
  }
}

@definterface[frame:searchable-text<%> (frame:searchable<%> frame:text<%>)]{
}

@defmixin[frame:searchable-text-mixin (frame:text<%> frame:searchable<%>) (frame:searchable-text<%>)]{

  @defmethod*[#:mode override-final (((get-text-to-search) (is-a?/c text%)))]{
    Returns the result of @method[frame:editor<%> get-editor].
  }

  @defmethod*[#:mode override (((get-editor<%>) (is-a?/c editor<%>)))]{
    Returns @racket[text:searching<%>].
  }
  @defmethod*[#:mode override (((get-editor%) (is-a?/c editor<%>)))]{
    Returns @racket[(text:searching-mixin (super get-editor%))].
  }
}

@defclass[frame:basic% (frame:register-group-mixin (frame:basic-mixin frame%)) ()]{}
@defclass[frame:size-pref% (frame:size-pref-mixin frame:basic%) ()]{}
@defclass[frame:info% (frame:info-mixin frame:basic%) ()]{}
@defclass[frame:text-info% (frame:text-info-mixin frame:info%) ()]{}
@defclass[frame:pasteboard-info% (frame:pasteboard-info-mixin frame:text-info%) ()]{}
@defclass[frame:status-line% (frame:status-line-mixin frame:text-info%) ()]{}
@defclass[frame:standard-menus% (frame:standard-menus-mixin frame:status-line%) ()]{}
@defclass[frame:editor% (frame:editor-mixin frame:standard-menus%) ()]{}
@defclass[frame:text% (frame:text-mixin frame:editor%) ()]{}
@defclass[frame:searchable% (frame:searchable-text-mixin (frame:searchable-mixin frame:text%)) ()]{}
@defclass[frame:delegate% (frame:delegate-mixin frame:searchable%) ()]{}
@defclass[frame:pasteboard% (frame:pasteboard-mixin frame:editor%) ()]{}

@(include-previously-extracted "main-extracts.rkt" #rx"^frame:")
