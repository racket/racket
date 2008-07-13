#lang scribble/doc
@(require scribble/manual scribble/extract scheme/include)
@(require (for-label framework))
@(require (for-label scheme/gui))
@(require (for-syntax (prefix-in s: scribble/reader)))
@title{Frame}

@definterface[frame:basic<%> (frame%)]{
  Classes matching this interface support the basic 
  @scheme[frame%]
  functionality required by the framework.
  @defmethod*[(((get-area-container%) (is-a?/c area-container<%>)))]{
    The class that this method returns is used to create the
    @scheme[area-container<%>]
    in this frame.

  }
  @defmethod*[(((get-area-container) (instance (is-a?/c area-container<%>))))]{
    This returns the main 
    @scheme[area-container<%>]
    in the frame

  }
  @defmethod*[(((get-menu-bar%) (subclass?/c menu-bar%)))]{
    The result of this method is used to create the initial menu bar for
    this frame.


    Return
    @scheme[menu-bar%].
  }
  @defmethod*[(((make-root-area-container (class (is-a?/c area-container<%>)) (parent (instance (is-a?/c area-container<%>)))) (instance (is-a?/c area-container<%>))))]{
    Override this method to insert a panel in between the panel used by
    the clients of this frame and the frame itself. For example, to insert
    a status line panel override this method with something like this:

    @schemeblock[
    (class ...
      ...
     (rename [super-make-root-area-container 
              make-root-area-container])
     (field
       [status-panel #f])
     (define/override (make-root-area-container cls parent)
       (set! status-panel
             (super-make-root-area-container vertical-panel% parent))
       (let ([root (make-object cls status-panel)])

          (code:comment "... add other children to status-panel ...")

          root))
      ...)]

    In this example, status-panel will contain a root panel for the other
    classes, and whatever panels are needed to display status information.

    The searching frame is implemented using this method.

    Calls @scheme[make-object] with @scheme[class] and @scheme[parent].
  }
  @defmethod*[(((close) void))]{
    This method closes the frame by calling the
    @method[top-level-window<%> can-close?],
    @method[top-level-window<%> on-close], and
    @method[top-level-window<%> show]
    methods. 

    It's implementation is:
    @schemeblock[
      (inherit can-close? on-close)
      (public
        [show
          (lambda ()
            (when (can-close?)
              (on-close)
              (show #f)))])]

  }
  @defmethod*[(((editing-this-file? (filename path)) boolean))]{
    Indicates if this frame contains this buffer (and can edit
    that file).


    Returns @scheme[#f].
  }
  @defmethod*[(((get-filename (temp (union |#f| (box boolean)) |#f|)) (union |#f| path)))]{
    This returns the filename that the frame is currently being saved as,
    or @scheme[#f] if there is no appropriate filename.


    Defaultly returns @scheme[#f]. 

    If @scheme[temp] is a box, it is filled with @scheme[#t] or @scheme[#f],
    depending if the filename is a temporary filename.
  }
  @defmethod*[(((make-visible (filename string)) void))]{
    Makes the file named by @scheme[filename] visible (intended for
    use with tabbed editing).

  }
}
@defmixin[frame:basic-mixin (frame%) (frame:basic<%>)]{
  This mixin provides the basic functionality that the framework
  expects. It helps manage the list of frames in the
  @scheme[group:%]
  object returned by
  @scheme[group:get-the-frame-group].

  Do not give @scheme[panel%]s or @scheme[control<%>]s this frame as
  parent. Instead, use the result of the
  @method[frame:basic<%> get-area-container]
  method.

  @index{Windows menu}
  This mixin also creates a menu bar for the frame, as the
  frame is initialized. It uses the class returned by
  @method[frame:basic<%> get-menu-bar%]. It only passes the frame as an initialization argument.
  In addition, it creates the windows menu in the menu bar.

  See also
  @scheme[frame:reorder-menus].
  @defmethod*[#:mode override (((show (on? boolean)) void))]{

    Calls the super method.

    When @scheme[on?] is @scheme[#t], inserts the frame into the
    frame group and when it is @scheme[#f], removes the frame
    from the group.
  }
  @defmethod*[#:mode override (((can-exit?) boolean))]{

    This, together with
    @method[frame:basic-mixin on-exit]
    mimics
    @scheme[exit:exit].

    First, it calls
    @scheme[exit:set-exiting]
    with @scheme[#t].
    Then, it calls 
    @scheme[exit:can-exit?]. If it returns @scheme[#t], 
    so does this method. If
    it returns @scheme[#f], 
    this method calls
    @scheme[exit:set-exiting]
    with @scheme[#f].
  }
  @defmethod*[#:mode override (((on-exit) void))]{

    Together with
    @method[frame:basic-mixin can-exit?]
    this mimics the behavior of
    @scheme[exit:exit].

    Calls
    @scheme[exit:on-exit]
    and then queues a callback
    to call MzScheme's @scheme[exit]
    function. If that returns, it
    calls
    @scheme[exit:set-exiting]
    to reset that flag to
    @scheme[#f].
  }
  @defmethod*[#:mode override (((on-superwindow-show (shown? any/c)) void))]{

    Notifies the result of (@scheme[group:get-the-frame-group]) that a frame has been shown, by calling
    the
    @method[group:% frame-shown/hidden]
    method.

  }
  @defmethod*[#:mode override (((on-drop-file (pathname string)) void))]{

    Calls 
    @scheme[handler:edit-file]
    with @scheme[pathname] as an argument.
  }
  @defmethod*[#:mode override (((after-new-child) void))]{

    Raises an exception if attempting to add a child to this frame (except if using the
    @method[frame:basic<%> make-root-area-container]
    method).
  }
}
@definterface[frame:size-pref<%> (frame:basic<%>)]{

}
@defmixin[frame:size-pref-mixin (frame:basic<%>) (frame:size-pref<%>)]{
  @defconstructor[((size-preferences-key symbol?) (label label-string?) (parent (or/c (is-a?/c frame%) false/c) #f) (x (or/c (integer-in -10000 10000) false/c) #f) (y (or/c (integer-in -10000 10000) false/c) #f) (style (listof (one-of/c (quote no-resize-border) (quote no-caption) (quote no-system-menu) (quote hide-menu-bar) (quote mdi-parent) (quote mdi-child) (quote toolbar-button) (quote float) (quote metal))) null) (enabled any/c #t) (border (integer-in 0 1000) 0) (spacing (integer-in 0 1000) 0) (alignment (list/c (one-of/c (quote left) (quote center) (quote right)) (one-of/c (quote top) (quote center) (quote bottom))) (quote (center top))) (min-width (integer-in 0 10000) graphical-minimum-width) (min-height (integer-in 0 10000) graphical-minimum-height) (stretchable-width any/c #t) (stretchable-height any/c #t))]{

    The size @scheme[size-preferences-key] symbol is used with 
    @scheme[preferences:get]
    and
    @scheme[preferences:set]
    to track the current size.

    Passes the @scheme[width] and @scheme[height] initialization
    arguments to the superclass based on the current value 
    of the preference.

    See also
    @scheme[frame:setup-size-pref].

  }
  @defmethod*[#:mode override (((on-size (width number) (height number)) void))]{

    Updates the preferences, according to the width and
    height. The preferences key is the one passed
    to the initialization argument of the class.
  }
}
@definterface[frame:register-group<%> ()]{
  Frames that implement this interface are registered with the group. See
  @scheme[group:get-the-frame-group]
  and
  @scheme[frame:register-group-mixin].
}
@defmixin[frame:register-group-mixin (frame:basic<%>) (frame:register-group<%>)]{
  During initialization, calls
  @method[group:% insert-frame]with @scheme[this].
  @defmethod*[#:mode augment (((can-close?) bool))]{

    Calls the inner method, with a default of @scheme[#t].
    If that returns @scheme[#t], 
    it checks for one of the these three conditions:

    @itemize{
    @item{
    @scheme[exit:exiting?]
    returns @scheme[#t]}
    @item{there is more than one
    frame in the group returned
    by
    @scheme[group:get-the-frame-group], or}
    @item{the procedure
    @scheme[exit:user-oks-exit]
    returns @scheme[#t].}}
    If any of those conditions hold, the 
    method returns @scheme[#t].
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    First calls the inner method.
    Next, calls the
    @method[group:% remove-frame]
    method of the result of
    @scheme[group:get-the-frame-group]
    with @scheme[this] as an argument.
    Finally, unless
    @scheme[exit:exiting?]
    returns @scheme[#t],
    and if there are no more
    frames open, it calls
    @scheme[exit:exit].
  }
  @defmethod*[#:mode override (((on-activate (on? boolean)) void))]{

    Calls
    @method[group:% set-active-frame]
    with @scheme[this] when
    @scheme[on?] is true.

  }
}
@definterface[frame:status-line<%> (frame:basic<%>)]{
  The mixin that implements this interface provides an
  interface to a set of status lines at the bottom of this
  frame. 

  Each status line must be opened with
  @method[frame:status-line<%> open-status-line]
  before any messages are shown in the status line and
  once
  @method[frame:status-line<%> close-status-line]
  is called, no more messages may be displayed,
  unless the status line is re-opened.

  The screen space for status lines is not created
  until 
  @method[frame:status-line<%> update-status-line]
  is called with a string. Additionally,
  the screen space for one status line is re-used
  when by another status line when the first
  passes @scheme[#f] to 
  @method[frame:status-line<%> update-status-line]. In this manner, the status line frame avoids
  opening too many status lines and avoids
  flashing the status lines open and closed too
  often.
  @defmethod*[(((open-status-line (id symbol?)) void))]{
    Creates a new status line identified by the symbol
    argument. The line will not appear in the frame until a
    message is put into it, via
    @method[frame:status-line<%> update-status-line].


  }
  @defmethod*[(((close-status-line (id symbol?)) void))]{
    Closes the status line @scheme[id].

  }
  @defmethod*[(((update-status-line (id symbol?) (status (union |#f| string))) void))]{
    Updates the status line named by @scheme[id] with
    @scheme[status]. If @scheme[status] is @scheme[#f], the status
    line is becomes blank (and may be used by other ids).

  }
}
@defmixin[frame:status-line-mixin (frame:basic<%>) (frame:status-line<%>)]{

  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c panel%)) (parent (instanceof (subclass?/c panel%)))) (is-a?/c panel%)))]{

    Adds a panel at the bottom of the frame to hold the status
    lines.

  }
}
@definterface[frame:info<%> (frame:basic<%>)]{
  Frames matching this interface support a status line.

  The preference @scheme['framework:show-status-line] controls
  the visibility of the status line. If it is @scheme[#t], the
  status line is visible and if it is @scheme[#f], the
  status line is not visible (see 
  @scheme[preferences:get] for more info about preferences)
  @defmethod*[(((determine-width (str string) (canvas (instance editor-canvas%)) (text (instance text%))) integer))]{
    This method is used to calculate the size of an
    @scheme[editor-canvas%]
    with a particular set of characters in it. 
    It is used to calculate the sizes of the edits in the status line.

  }
  @defmethod*[(((lock-status-changed) void))]{
    This method is called when the lock status of the
    @scheme[editor<%>]
    changes.


    Updates the lock icon in the status line panel.
  }
  @defmethod*[(((update-info) void))]{
    This method updates all of the information in the panel.

  }
  @defmethod*[(((set-info-canvas (canvas (instance canvas:basic%))) void))]{
    Sets this canvas to be the canvas that the info frame shows info about. The
    @method[canvas:info-mixin% on-focus]
    and
    @method[canvas:info-mixin% set-editor]
    methods call this method to ensure that the info canvas is set correctly.

  }
  @defmethod*[(((get-info-canvas) (instance canvas:basic%)))]{
    Returns the canvas that the
    @scheme[frame:info<%>]
    currently shows info about. See also
    @method[frame:info<%> set-info-canvas]

  }
  @defmethod*[(((get-info-editor) (union |#f| (is-a?/c editor<%>))))]{
    Override this method to specify the editor that the status line
    contains information about.


    Returns the result of
    @method[frame:editor<%> get-editor].
  }
  @defmethod*[(((get-info-panel) (instance horizontal-panel%)))]{
    This method returns the panel where the information about this editor
    is displayed.

  }
  @defmethod*[(((show-info) void))]{
    Shows the info panel. 

    See also
    @method[frame:info<%> is-info-hidden?].

  }
  @defmethod*[(((hide-info) void))]{
    Hides the info panel.

    See also
    @method[frame:info<%> is-info-hidden?].

  }
  @defmethod*[(((is-info-hidden?) boolean))]{
    Result indicates if the show info panel has been explicitly hidden with
    @method[frame:info<%> hide-info].

    If this method returns @scheme[#t] and 
    @scheme[(preferences:get 'framework:show-status-line)] is
    @scheme[#f], then the info panel will not be visible.
    Otherwise, it is visible.
  }
}
@defmixin[frame:info-mixin (frame:basic<%>) (frame:info<%>)]{
  This mixin provides support for displaying various info in the status
  line of the frame.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.
  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c area-container<%>)) (parent (is-a?/c area-container<%>))) (instance area-container<%>)))]{

    Builds an extra panel for displaying various information.
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Removes the GC icon with
    @scheme[unregister-collecting-blit]
    and cleans up other callbacks.
  }
}
@definterface[frame:text-info<%> (frame:info<%>)]{
  Objects matching this interface receive information from editors
  constructed with
  @scheme[editor:info-mixin]
  and display it.
  @defmethod*[(((set-macro-recording (on? boolean)) void))]{
    Shows/hides the icon in the info bar that indicates if a
    macro recording is in progress.

  }
  @defmethod*[(((overwrite-status-changed) void))]{
    This method is called when the overwrite mode is turned either on or off in the 
    @scheme[editor<%>]
    in this frame.



  }
  @defmethod*[(((anchor-status-changed) void))]{
    This method is called when the anchor is turned either on or off in the 
    @scheme[editor<%>]
    in this frame.


  }
  @defmethod*[(((editor-position-changed) void))]{
    This method is called when the position in the 
    @scheme[editor<%>]
    changes.


  }
}
@defmixin[frame:text-info-mixin (frame:info<%>) (frame:text-info<%>)]{
  This mixin adds status information to the info panel relating to an
  edit.

  @defmethod*[#:mode augment (((on-close) void))]{

    removes a preferences callback for @scheme['framework:line-offsets].
    See @scheme[preferences:add-callback] for more information.
  }
  @defmethod*[#:mode override (((update-info) void))]{

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

@(include/reader "standard-menus.scrbl" s:read-syntax)

@defmixin[frame:standard-menus-mixin (frame:basic<%>) (frame:standard-menus<%>)]{
  The result of this mixin implements
  @scheme[frame:standard-menus<%>].
  @defmethod*[#:mode augment (((on-close) void))]{
    Removes the preferences callbacks for the menu items
  }
}
@definterface[frame:editor<%> (frame:standard-menus<%>)]{
  Frame classes matching this interface support embedded editors.
  @defmethod*[(((get-entire-label) string))]{
    This method returns the entire label for the frame.
    See also
    @method[window<%> set-label]
    and
    @method[frame:editor<%> set-label-prefix].

  }
  @defmethod*[(((get-label-prefix) string))]{
    This returns the prefix for the frame's label.

  }
  @defmethod*[(((set-label-prefix (prefix string)) void))]{
    Sets the prefix for the label of the frame.

  }
  @defmethod*[(((get-canvas%) (subclass?/c editor-canvas%)))]{
    The result of this method is used to create the canvas for the
    @scheme[editor<%>]
    in this frame.


    Returns 
    @scheme[editor-canvas%].
  }
  @defmethod*[(((get-canvas<%>) (instance canvas:basic%)))]{
    The result of this method is used to guard the result of the
    @method[frame:editor<%> get-canvas%]
    method. 

  }
  @defmethod*[(((get-editor%) (is-a?/c editor<%>)))]{
    The result of this class is used to create the 
    @scheme[editor<%>]
    in this frame.

    Override this method to specify a different editor class.


    Returns the value of the init-field @scheme[editor%].
  }
  @defmethod*[(((get-editor<%>) interface))]{
    The result of this method is used by 
    @method[frame:editor<%> make-editor]
    to check that 
    @method[frame:editor<%> get-editor%]
    is returning a reasonable editor.


    Returns
    @scheme[editor<%>].
  }
  @defmethod*[(((make-editor) (instance (is-a?/c editor<%>))))]{
    This method is called to create the editor in this frame.
    It calls
    @method[frame:editor<%> get-editor<%>]
    and uses that interface to make sure the result of
    @method[frame:editor<%> get-editor%]
    is reasonable.



    Calls @scheme[(make-object #, @method[frame:editor<%> get-editor%])].

  }
  @defmethod*[(((revert) void))]{

    Loads the most recently saved version of the file to the disk. If the 
    @scheme[editor<%>]
    is a
    @scheme[text%], the start and end positions are restored.
  }
  @defmethod*[(((save (format (union (quote guess) (quote standard) (quote text) (quote text-force-cr) (quote same) (quote copy)) (quote same))) boolean))]{
    Saves the file being edited, possibly calling
    @method[frame:editor<%> save-as]
    if the editor has no filename yet.


    Returns @scheme[#f] if the user cancels this operation
    (only possible when the file has not been saved before and
    the user is prompted for a new filename) and returns
    @scheme[#t] if not.
  }
  @defmethod*[(((save-as (format (union (quote guess) (quote standard) (quote text) (quote text-force-cr) (quote same) (quote copy)) (quote same))) boolean))]{
    Queries the use for a file name and saves the file with that name.


    Returns @scheme[#f] if the user cancells the file-choosing
    dialog and returns @scheme[#t] otherwise.
  }
  @defmethod*[(((get-canvas) (instance (subclass?/c canvas%))))]{
    Returns the canvas used to display the 
    @scheme[editor<%>]
    in this frame.


  }
  @defmethod*[(((get-editor) (instance (is-a?/c editor<%>))))]{
    Returns the editor in this frame.
  }
}
@defmixin[frame:editor-mixin (frame:standard-menus<%>) (frame:editor<%>)]{
  This mixin adds functionality to support an 
  @scheme[editor<%>]
  in the frame. This
  includes management of the title, implementations of some of the menu
  items, a reasonable initial size, and access to the 
  @scheme[editor<%>]
  itself.

  The size of this frame with be either 600 by 650 or 65 less than the
  width and height of the screen, whichever is smaller.

  @defconstructor[((filename string?) (editor% (is-a?/c editor<%>)) (parent (or/c (is-a?/c frame%) false/c) #f) (width (or/c (integer-in 0 10000) false/c) #f) (height (or/c (integer-in 0 10000) false/c) #f) (x (or/c (integer-in -10000 10000) false/c) #f) (y (or/c (integer-in -10000 10000) false/c) #f) (style (listof (one-of/c (quote no-resize-border) (quote no-caption) (quote no-system-menu) (quote hide-menu-bar) (quote mdi-parent) (quote mdi-child) (quote toolbar-button) (quote float) (quote metal))) null) (enabled any/c #t) (border (integer-in 0 1000) 0) (spacing (integer-in 0 1000) 0) (alignment (list/c (one-of/c (quote left) (quote center) (quote right)) (one-of/c (quote top) (quote center) (quote bottom))) (quote (center top))) (min-width (integer-in 0 10000) graphical-minimum-width) (min-height (integer-in 0 10000) graphical-minimum-height) (stretchable-width any/c #t) (stretchable-height any/c #t))]{

  }
  @defmethod*[#:mode override (((get-filename) (union |#f| path)))]{

    Returns the filename in the editor returned by
    @method[frame:editor<%> get-editor].
  }
  @defmethod*[#:mode override (((editing-this-file? (filename path)) boolean))]{

    Returns @scheme[#t] if the filename is the file that this
    frame is editing.
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Calls the 
    @scheme[editor:basic<%>]'s method
    @method[editor:basic<%> on-close].
  }
  @defmethod*[#:mode augment (((can-close?) void))]{

    Calls the 
    @scheme[editor:basic<%>]'s method
    @method[editor:basic<%> can-close?].
  }
  @defmethod*[#:mode override (((get-label) string))]{

    Returns the portion of the label after the hyphen. See also
    @method[frame:editor<%> get-entire-label].
  }
  @defmethod*[#:mode override (((set-label (label string?)) void))]{

    Sets the label, but preserves the label's prefix. See also
    @method[frame:editor<%> set-label-prefix].
  }
  @defmethod*[#:mode override (((file-menu:open-callback (item any) (evt mouse-event)) void))]{

    Calls
    @scheme[handler:open-file]
    with the directory of the saved file associated with this editor
    (if any).
  }
  @defmethod*[#:mode override (((file-menu:revert-on-demand) void))]{

    Disables the menu item when the editor is locked.
  }
  @defmethod*[#:mode override (((file-menu:revert-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Informs the user that this action is not undoable and, 
    if they still want to continue, calls
    @method[frame:editor<%> revert].
  }
  @defmethod*[#:mode override (((file-menu:create-revert?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((file-menu:save-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Saves the file in the editor.
  }
  @defmethod*[#:mode override (((file-menu:create-save?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((file-menu:save-as-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Prompts the user for a file name and uses that filename to save the buffer. 
    Calls
    @method[frame:editor<%> save-as]
    with no arguments.
  }
  @defmethod*[#:mode override (((file-menu:create-save-as?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((file-menu:print-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Calls the
    @method[editor<%> print]
    method of
    @scheme[editor<%>]
    with the default arguments, except that
    the @scheme[output-mode] argument
    is the result of calling
    @scheme[preferences:get]
    with @scheme['framework:print-output-mode].
  }
  @defmethod*[#:mode override (((file-menu:create-print?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((file-menu:between-save-as-and-print (file-menu (is-a?/c menu%))) void))]{

    Creates a Print Setup menu item.
  }
  @defmethod*[#:mode override (((edit-menu:between-select-all-and-find (edit-menu (instance menu%))) void))]{

    Adds a menu item for toggling
    @method[editor<%> auto-wrap]
    in the focused text.

  }
  @defmethod*[#:mode override (((help-menu:about-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Calls
    @scheme[message-box]
    with a message welcoming the user to the application named by
    @scheme[application:current-app-name]

  }
  @defmethod*[#:mode override (((help-menu:about-string) string))]{

    Returns the result of (@scheme[application:current-app-name])
  }
  @defmethod*[#:mode override (((help-menu:create-about?) boolean))]{

    returns #t
  }
}
@definterface[frame:open-here<%> (frame:editor<%>)]{
  Frames implementing this mixin can change the file they are
  displaying. 

  The frame is only re-used when the
  @scheme['framework:open-here?] preference is set
  (see 
  @scheme[preferences:get]
  and 
  @scheme[preferences:set]
  for details on preferences).

  The
  @scheme[frame:open-here-mixin]
  implements this interface.
  @defmethod*[(((get-open-here-editor) (is-a?/c editor<%>)))]{
    When the user switches the visible file in this frame, 
    the  of this method is the editor that gets switched.


    Defaultly returns the result of
    @method[frame:editor<%> get-editor].
  }
  @defmethod*[(((open-here (filename string)) void))]{

    Opens @scheme[filename] in the current frame, possibly
    prompting the user about saving a file (in which case the
    frame might not get switched).
  }
}
@defmixin[frame:open-here-mixin (frame:editor<%>) (frame:open-here<%>)]{
  Provides an implementation of
  @scheme[frame:open-here<%>]
  @defmethod*[#:mode override (((file-menu:new-on-demand (item (is-a?/c menu-item%))) void))]{

    Sets the label of @scheme[item] to 
    @scheme["New..."] if the preference @scheme['framework:open-here?] is set. 
  }
  @defmethod*[#:mode override (((file-menu:new-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{

    When the preference @scheme['framework:open-here?]
    preference is set, this method prompts the user, asking if
    they would like to create a new frame, or just clear out
    this one. If they clear it out and the file hasn't been
    saved, they are asked about saving.
  }
  @defmethod*[#:mode override (((file-menu:open-on-demand (item (is-a?/c menu-item%))) void))]{

    Sets the label of @scheme[item] to 
    "Open Here..." if the preference @scheme['framework:open-here?] is set. 
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Calls 
    @method[group:% set-open-here-frame]
    with @scheme[#f] if 
    the result of 
    @method[group:% get-open-here-frame]
    is @scheme[eq?] to @scheme[this].
  }
  @defmethod*[#:mode override (((on-activate (on? boolean)) void))]{

    When @scheme[on?] is @scheme[#t], calls
    @method[group:% set-open-here-frame]
    with @scheme[this].
  }
}
@definterface[frame:text<%> (frame:editor<%>)]{
  Frames matching this interface provide support for 
  @scheme[text%]s.

}
@defmixin[frame:text-mixin (frame:editor<%>) (frame:text<%>)]{
  This mixins adds support for
  @scheme[text%]s in the frame.

  @defconstructor[((editor% (extends text%)))]{

    Calls the super initialization with either the value of the
    @scheme[editor%] init or, if none was supplied, it passes @scheme[text%].
  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns @scheme[(class->interface text%)].
  }
}
@definterface[frame:pasteboard<%> (frame:editor<%>)]{
  Frames matching this interface provide support for 
  @scheme[pasteboard%]s.
}
@defmixin[frame:pasteboard-mixin (frame:editor<%>) (frame:pasteboard<%>)]{
  This mixin provides support for pasteboards in a frame.

  @defconstructor[((editor% (extends pasteboard%)))]{

    Calls the super initialization with either the value of the
    @scheme[editor%] init or, if none was supplied, it passes @scheme[pasteboard%].

  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns @scheme[(class->interface pasteboard%)].
  }
}
@definterface[frame:delegate<%> (frame:status-line<%> frame:text<%>)]{
  Frames that implement this interface provide a 20,000 feet
  overview of the text in the main editor. The term @bold{delegate} 
  in these method descriptions refers to the
  original editor and the term @bold{delegatee} refers to the
  editor showing the 20,000 feet overview.
  @defmethod*[(((get-delegated-text) (instanceof (is-a?/c text:delegate<%>))))]{
    Returns the delegate text.

  }
  @defmethod*[(((delegated-text-shown?) boolean))]{
    Returns @scheme[#t] if the delegate is visible, and
    @scheme[#f] if it isn't.

  }
  @defmethod*[(((hide-delegated-text) void))]{
    Hides the delegated text.

    When the delegated text is hidden, it is not being
    updated. This is accomplished by calling the 
    @method[text:delegate<%> set-delegate]
    method of
    @method[frame:editor<%> get-editor]with @scheme[#f].

    See also
    @method[frame:delegate<%> show-delegated-text]

  }
  @defmethod*[(((show-delegated-text) void))]{
    Makes the delegated text visible.

    When the delegated text is shown, the
    @method[text:delegate<%> set-delegate]
    method of
    @method[frame:delegate<%> get-delegated-text]is called with
    the text to delegate messages to.

    See also
    @method[frame:delegate<%> hide-delegated-text].

  }
  @defmethod*[(((delegate-moved) void))]{
    This method is called when the visible region of the
    delegate editor changes, so that the blue region in the
    delegatee is updated.

  }
}
@defmixin[frame:delegate-mixin (frame:status-line<%> frame:text<%>) (frame:delegate<%>)]{
  Adds support for a 20,000-feet view via
  @scheme[text:delegate<%>] and @scheme[text:delegate-mixin]
  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c panel%)) (parent (instanceof (subclass?/c panel%)))) (is-a?/c panel%)))]{

    adds a panel outside to hold the delegate
    @scheme[editor-canvas%] and @scheme[text%].

  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns @scheme[text:delegate].

  }
  @defmethod*[#:mode override (((get-editor%) (is-a?/c text:delegate<%>)))]{

    returns the super result, with the
    @scheme[text:delegate-mixin]
    mixed in.
  }
}
@definterface[frame:searchable<%> (frame:basic<%>)]{
  Frames that implement this interface support searching.
  @defmethod*[(((get-text-to-search) (instance (subclass?/c text%))))]{
    Override this method to specify which text to search.


    Returns the result of
    @method[frame:editor<%> get-editor].
  }
  @defmethod*[(((hide-search) void))]{
    This method hides the searching information on the bottom of the
    frame.

  }
  @defmethod*[(((unhide-search) void))]{
    When the searching sub window is hidden, makes it visible.

  }
  @defmethod*[(((set-search-direction (dir (union -1 1))) void))]{
    Sets the direction that future searches will be performed.


    If @scheme[dir] is @scheme[1] searches will be performed forwards and if 
    @scheme[dir] is @scheme[-1] searches will be performed backwards.
  }
  @defmethod*[(((replace&search) boolean))]{
    Calls
    @method[frame:searchable<%> replace]
    and if it returns @scheme[#t], calls 
    @method[frame:searchable<%> search-again].

  }
  @defmethod*[(((replace-all) void))]{
    Loops through the text from the current position to the end, replacing
    all occurrences of the search string with the contents of the replace
    edit. Only searches forward, does not loop around to the beginning of
    the text.

  }
  @defmethod*[(((replace) boolean))]{
    If the selected text matches the search string, this method replaces
    the text with the contents of the replace text. If the replace was
    successful, @scheme[#t] is returned. Otherwise, @scheme[#f] is returned.

  }
  @defmethod*[(((can-replace?) boolean))]{
    Returns @scheme[#t] if a replace command would succeed. 


    Defaultly is @scheme[#t] when the selected text in the result of
    @method[frame:searchable<%> get-text-to-search]
    is the same as the text in the find text.
  }
  @defmethod*[(((toggle-search-focus) void))]{
    Toggles the keyboard focus between the searching edit, the replacing edit and the result of
    @method[frame:searchable<%> get-text-to-search].

  }
  @defmethod*[(((move-to-search-or-search) (union boolean void)))]{
    This method moves the focus to the text that contains the search
    string, or if the focus is there already, performs a forward search.

    It returns void if the focus was not to the search text, otherwise it
    returns a boolean indicating the success of the search.


  }
  @defmethod*[(((move-to-search-or-reverse-search) (union boolean void)))]{
    This method moves the focus to the text that contains the search
    string, or if the focus is there already, performs a reverse search.

    It returns void if the focus was not to the search text, otherwise it
    returns a boolean indicating the success of the search.

  }
  @defmethod*[(((search-again (direction Symbol (rm previous searching direction)) (beep? bool |#t|)) boolean))]{
    Searches for the text in the search edit in the result of
    @method[frame:searchable<%> get-text-to-search]. 


    Returns @scheme[#t] if the text is found and sets the selection to the
    found text. If the text is not found it returns @scheme[#f].
  }
}
@defmixin[frame:searchable-mixin (frame:standard-menus<%>) (frame:searchable<%>)]{
  This mixin adds support for searching in the
  @scheme[editor<%>]
  in this frame.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.
  @defmethod*[#:mode override (((edit-menu:find-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Calls
    @method[frame:searchable<%> move-to-search-or-search].
  }
  @defmethod*[#:mode override (((edit-menu:create-find?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((edit-menu:find-again-callback) boolean))]{

    Returns @scheme[#t], and searches for the same text that was last
    searched for in the text.
  }
  @defmethod*[#:mode override (((edit-menu:create-find-again?) boolean))]{

    returns #t
  }
  @defmethod*[#:mode override (((edit-menu:replace-and-find-again-callback) boolean))]{

    Returns @scheme[#t], and if the selected text matches the current text
    in the find box, replaces it with the contents of the replace box and
    searches for the next occurrence of the text in the find box.


  }
  @defmethod*[#:mode override (((edit-menu:replace-and-find-again-on-demand (item menu-item%)) void))]{

    Disables @scheme[item] when
    @method[frame:searchable<%> can-replace?]
    returns @scheme[#f] and enables it when that method returns
    @scheme[#t].
  }
  @defmethod*[#:mode override (((edit-menu:create-replace-and-find-again?) boolean))]{

    returns @scheme[#t]
  }
  @defmethod*[#:mode override (((make-root-area-container) (is-a?/c area-container<%>)))]{

    Builds a panel for the searching information.
  }
  @defmethod*[#:mode override (((on-activate) void))]{

    When the frame is activated, searches will take place in this frame.
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Cleans up after the searching frame.
  }
}
@definterface[frame:searchable-text<%> (frame:searchable<%> frame:text<%>)]{

}
@defmixin[frame:searchable-text-mixin (frame:text<%> frame:searchable<%>) (frame:searchable-text<%>)]{

  @defmethod*[#:mode override (((get-text-to-search) (instanceof text%)))]{

    Returns the result of
    @method[frame:editor<%> get-editor].
  }
  @defmethod*[#:mode override (((get-editor<%>) (is-a?/c editor<%>)))]{

    Returns
    @scheme[text:searching<%>].
  }
  @defmethod*[#:mode override (((get-editor%) (is-a?/c editor<%>)))]{

    Returns
    @scheme[text:searching%].
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
@defclass[frame:open-here% (frame:open-here-mixin frame:editor%) ()]{}
@defclass[frame:text% (frame:text-mixin frame:open-here%) ()]{}
@defclass[frame:searchable% (frame:searchable-text-mixin (frame:searchable-mixin frame:text%)) ()]{}
@defclass[frame:delegate% (frame:delegate-mixin frame:searchable%) ()]{}
@defclass[frame:pasteboard% (frame:pasteboard-mixin frame:open-here%) ()]{}

@(include-previously-extracted "main-extracts.ss" #rx"^frame:")
