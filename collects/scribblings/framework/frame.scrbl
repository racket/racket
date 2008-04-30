#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
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

    \begin{schemedisplay}
    (class ...
      ...
     (rename [super-make-root-area-container make-root-area-container])
     (field
       [status-panel #f])
     (define/override (make-root-area-container cls parent)
       (set! status-panel
             (super-make-root-area-container vertical-panel% parent))
       (let ([root (make-object cls status-panel)])

          ; ... add other children to status-panel ...

          root))
      ...
    \end{schemedisplay}

    In this example, status-panel will contain a root panel for the other
    classes, and whatever panels are needed to display status information.

    The searching frame is implemented using this method.


    Calls \rawscm{make-object} with \var{class} and \var{parent}.
  }
  @defmethod*[(((close) void))]{
    This method closes the frame by calling the
    @method[top-level-window<%> can-close?],
    @method[top-level-window<%> on-close], and
    @method[top-level-window<%> show]
    methods. 

    It's implementation is:
    \begin{schemedisplay}
      (inherit can-close? on-close)
      (public
        [show
          (lambda ()
            (when (can-close?)
              (on-close)
              (show #f)))])
    \end{schemedisplay}

  }
  @defmethod*[(((editing-this-file? (filename path)) boolean))]{
    Indicates if this frame contains this buffer (and can edit
    that file).


    Returns \scheme|#f|.
  }
  @defmethod*[(((get-filename (temp (union |#f| (box boolean)) |#f|)) (union |#f| path)))]{
    This returns the filename that the frame is currently being saved as,
    or \rawscm{\#f} if there is no appropriate filename.


    Defaultly returns \rawscm{\#f}. 

    If \var{temp} is a box, it is filled with \rawscm{\#t} or \rawscm{\#f},
    depending if the filename is a temporary filename.
  }
  @defmethod*[(((make-visible (filename string)) void))]{
    Makes the file named by \var{filename} visible (intended for
    use with tabbed editing).

  }
}
@defmixin[frame:basic-mixin (frame%) (frame:basic<%>)]{
  This mixin provides the basic functionality that the framework
  expects. It helps manage the list of frames in the
  @scheme[group:%]
  object returned by
  @scheme[group:get-the-frame-group].

  Do not give \iscmclass{panel}s or \iscmintf{control}s this frame as
  parent. Instead, use the result of the
  @method[frame:basic<%> get-area-container]
  method.

  \index{Windows menu}
  This mixin also creates a menu bar for the frame, as the
  frame is initialized. It uses the class returned by
  @method[frame:basic<%> get-menu-bar\%]. It only passes the frame as an initialization argument.
  In addition, it creates the windows menu in the menu bar.

  See also
  @scheme[frame:reorder-menus].
  @defmethod*[#:mode override (((show (on? boolean)) void))]{

    Calls the super method.

    When \var{on?} is \scheme|#t|, inserts the frame into the
    frame group and when it is \scheme|#f|, removes the frame
    from the group.
  }
  @defmethod*[#:mode override (((can-exit?) boolean))]{

    This, together with
    @method[frame:basic-mixin on-exit]
    mimics
    @scheme[exit:exit].

    First, it calls
    @scheme[exit:set-exiting]
    with \rawscm{\#t}.
    Then, it calls 
    @scheme[exit:can-exit?]. If it returns \rawscm{\#t}, 
    so does this method. If
    it returns \rawscm{\#f}, 
    this method calls
    @scheme[exit:set-exiting]
    with \rawscm{\#f}.
  }
  @defmethod*[#:mode override (((on-exit) void))]{

    Together with
    @method[frame:basic-mixin can-exit?]
    this mimics the behavior of
    @scheme[exit:exit].

    Calls
    @scheme[exit:on-exit]
    and then queues a callback
    to call MzScheme's \rawscm{exit}
    function. If that returns, it
    calls
    @scheme[exit:set-exiting]
    to reset that flag to
    \rawscm{\#f}.
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
    with \var{pathname} as an argument.
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

    The size \var{size-preferences-key} symbol is used with 
    @scheme[preferences:get]
    and
    @scheme[preferences:set]
    to track the current size.

    Passes the \var{width} and \var{height} initialization
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
  @method[group:% insert-frame]with \scheme|this|.
  @defmethod*[#:mode augment (((can-close?) bool))]{

    Calls the inner method, with a default of \scheme|#t|.
    If that returns \scheme|#t|, 
    it checks for one of the these three conditions:
    \begin{itemize}

    \item 
    @scheme[exit:exiting?]
    returns \rawscm{\#t}
    \item there is more than one
    frame in the group returned
    by
    @scheme[group:get-the-frame-group], or
    \item the procedure
    @scheme[exit:user-oks-exit]
    returns \rawscm{\#t}.
    \end{itemize}
    If any of those conditions hold, the 
    method returns \rawscm{\#t}.
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    First calls the inner method.
    Next, calls the
    @method[group:% remove-frame]
    method of the result of
    @scheme[group:get-the-frame-group]
    with \rawscm{this} as an argument.
    Finally, unless
    @scheme[exit:exiting?]
    returns \rawscm{\#t},
    and if there are no more
    frames open, it calls
    @scheme[exit:exit].
  }
  @defmethod*[#:mode override (((on-activate (on? boolean)) void))]{

    Calls
    @method[group:% set-active-frame]
    with \rawscm{this} when
    \var{on?} is true.

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
  passes \scheme|#f| to 
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
    Closes the status line \var{id}.

  }
  @defmethod*[(((update-status-line (id symbol?) (status (union |#f| string))) void))]{
    Updates the status line named by \var{id} with
    \var{status}. If \var{status} is \scheme{#f}, the status
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

  The preference \scheme|'framework:show-status-line| controls
  the visibility of the status line. If it is \scheme|#t|, the
  status line is visible and if it is \scheme|#f|, the
  status line is not visible (see 
  \hyperref{the preferences section}{section~}{ for more info about preferences}{fw:preferences}).
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

    If this method returns \scheme|#t| and 
    \scheme|(preferences:get 'framework:show-status-line)| is
    \scheme|#f|, then the info panel will not be visible.
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

    removes a preferences callback for \rawscm{'framework:line-offsets}.
    See section~\ref{fw:preferences} for more information
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
@definterface[frame:standard-menus<%> (frame:basic<%>)]{
  \begin{itemize}
  \item
  @method[frame:standard-menus<%> file-menu:new-callback],
  @method[frame:standard-menus<%> file-menu:create-new?],
  @method[frame:standard-menus<%> file-menu:new-string],
  @method[frame:standard-menus<%> file-menu:new-help-string],
  @method[frame:standard-menus<%> file-menu:new-on-demand],
  @method[frame:standard-menus<%> file-menu:get-new-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-new-and-open]

  \item
  @method[frame:standard-menus<%> file-menu:open-callback],
  @method[frame:standard-menus<%> file-menu:create-open?],
  @method[frame:standard-menus<%> file-menu:open-string],
  @method[frame:standard-menus<%> file-menu:open-help-string],
  @method[frame:standard-menus<%> file-menu:open-on-demand],
  @method[frame:standard-menus<%> file-menu:get-open-item]

  \item
  @method[frame:standard-menus<%> file-menu:open-recent-callback],
  @method[frame:standard-menus<%> file-menu:create-open-recent?],
  @method[frame:standard-menus<%> file-menu:open-recent-string],
  @method[frame:standard-menus<%> file-menu:open-recent-help-string],
  @method[frame:standard-menus<%> file-menu:open-recent-on-demand],
  @method[frame:standard-menus<%> file-menu:get-open-recent-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-open-and-revert]

  \item
  @method[frame:standard-menus<%> file-menu:revert-callback],
  @method[frame:standard-menus<%> file-menu:create-revert?],
  @method[frame:standard-menus<%> file-menu:revert-string],
  @method[frame:standard-menus<%> file-menu:revert-help-string],
  @method[frame:standard-menus<%> file-menu:revert-on-demand],
  @method[frame:standard-menus<%> file-menu:get-revert-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-revert-and-save]

  \item
  @method[frame:standard-menus<%> file-menu:save-callback],
  @method[frame:standard-menus<%> file-menu:create-save?],
  @method[frame:standard-menus<%> file-menu:save-string],
  @method[frame:standard-menus<%> file-menu:save-help-string],
  @method[frame:standard-menus<%> file-menu:save-on-demand],
  @method[frame:standard-menus<%> file-menu:get-save-item]

  \item
  @method[frame:standard-menus<%> file-menu:save-as-callback],
  @method[frame:standard-menus<%> file-menu:create-save-as?],
  @method[frame:standard-menus<%> file-menu:save-as-string],
  @method[frame:standard-menus<%> file-menu:save-as-help-string],
  @method[frame:standard-menus<%> file-menu:save-as-on-demand],
  @method[frame:standard-menus<%> file-menu:get-save-as-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-save-as-and-print]

  \item
  @method[frame:standard-menus<%> file-menu:print-callback],
  @method[frame:standard-menus<%> file-menu:create-print?],
  @method[frame:standard-menus<%> file-menu:print-string],
  @method[frame:standard-menus<%> file-menu:print-help-string],
  @method[frame:standard-menus<%> file-menu:print-on-demand],
  @method[frame:standard-menus<%> file-menu:get-print-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-print-and-close]

  \item
  @method[frame:standard-menus<%> file-menu:close-callback],
  @method[frame:standard-menus<%> file-menu:create-close?],
  @method[frame:standard-menus<%> file-menu:close-string],
  @method[frame:standard-menus<%> file-menu:close-help-string],
  @method[frame:standard-menus<%> file-menu:close-on-demand],
  @method[frame:standard-menus<%> file-menu:get-close-item]

  \item
  @method[frame:standard-menus<%> file-menu:between-close-and-quit]

  \item
  @method[frame:standard-menus<%> file-menu:quit-callback],
  @method[frame:standard-menus<%> file-menu:create-quit?],
  @method[frame:standard-menus<%> file-menu:quit-string],
  @method[frame:standard-menus<%> file-menu:quit-help-string],
  @method[frame:standard-menus<%> file-menu:quit-on-demand],
  @method[frame:standard-menus<%> file-menu:get-quit-item]

  \item
  @method[frame:standard-menus<%> file-menu:after-quit]

  \item
  @method[frame:standard-menus<%> edit-menu:undo-callback],
  @method[frame:standard-menus<%> edit-menu:create-undo?],
  @method[frame:standard-menus<%> edit-menu:undo-string],
  @method[frame:standard-menus<%> edit-menu:undo-help-string],
  @method[frame:standard-menus<%> edit-menu:undo-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-undo-item]

  \item
  @method[frame:standard-menus<%> edit-menu:redo-callback],
  @method[frame:standard-menus<%> edit-menu:create-redo?],
  @method[frame:standard-menus<%> edit-menu:redo-string],
  @method[frame:standard-menus<%> edit-menu:redo-help-string],
  @method[frame:standard-menus<%> edit-menu:redo-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-redo-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-redo-and-cut]

  \item
  @method[frame:standard-menus<%> edit-menu:cut-callback],
  @method[frame:standard-menus<%> edit-menu:create-cut?],
  @method[frame:standard-menus<%> edit-menu:cut-string],
  @method[frame:standard-menus<%> edit-menu:cut-help-string],
  @method[frame:standard-menus<%> edit-menu:cut-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-cut-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-cut-and-copy]

  \item
  @method[frame:standard-menus<%> edit-menu:copy-callback],
  @method[frame:standard-menus<%> edit-menu:create-copy?],
  @method[frame:standard-menus<%> edit-menu:copy-string],
  @method[frame:standard-menus<%> edit-menu:copy-help-string],
  @method[frame:standard-menus<%> edit-menu:copy-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-copy-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-copy-and-paste]

  \item
  @method[frame:standard-menus<%> edit-menu:paste-callback],
  @method[frame:standard-menus<%> edit-menu:create-paste?],
  @method[frame:standard-menus<%> edit-menu:paste-string],
  @method[frame:standard-menus<%> edit-menu:paste-help-string],
  @method[frame:standard-menus<%> edit-menu:paste-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-paste-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-paste-and-clear]

  \item
  @method[frame:standard-menus<%> edit-menu:clear-callback],
  @method[frame:standard-menus<%> edit-menu:create-clear?],
  @method[frame:standard-menus<%> edit-menu:clear-string],
  @method[frame:standard-menus<%> edit-menu:clear-help-string],
  @method[frame:standard-menus<%> edit-menu:clear-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-clear-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-clear-and-select-all]

  \item
  @method[frame:standard-menus<%> edit-menu:select-all-callback],
  @method[frame:standard-menus<%> edit-menu:create-select-all?],
  @method[frame:standard-menus<%> edit-menu:select-all-string],
  @method[frame:standard-menus<%> edit-menu:select-all-help-string],
  @method[frame:standard-menus<%> edit-menu:select-all-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-select-all-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-select-all-and-find]

  \item
  @method[frame:standard-menus<%> edit-menu:find-callback],
  @method[frame:standard-menus<%> edit-menu:create-find?],
  @method[frame:standard-menus<%> edit-menu:find-string],
  @method[frame:standard-menus<%> edit-menu:find-help-string],
  @method[frame:standard-menus<%> edit-menu:find-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-find-item]

  \item
  @method[frame:standard-menus<%> edit-menu:find-again-callback],
  @method[frame:standard-menus<%> edit-menu:create-find-again?],
  @method[frame:standard-menus<%> edit-menu:find-again-string],
  @method[frame:standard-menus<%> edit-menu:find-again-help-string],
  @method[frame:standard-menus<%> edit-menu:find-again-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-find-again-item]

  \item
  @method[frame:standard-menus<%> edit-menu:replace-and-find-again-callback],
  @method[frame:standard-menus<%> edit-menu:create-replace-and-find-again?],
  @method[frame:standard-menus<%> edit-menu:replace-and-find-again-string],
  @method[frame:standard-menus<%> edit-menu:replace-and-find-again-help-string],
  @method[frame:standard-menus<%> edit-menu:replace-and-find-again-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-replace-and-find-again-item]

  \item
  @method[frame:standard-menus<%> edit-menu:between-find-and-preferences]

  \item
  @method[frame:standard-menus<%> edit-menu:preferences-callback],
  @method[frame:standard-menus<%> edit-menu:create-preferences?],
  @method[frame:standard-menus<%> edit-menu:preferences-string],
  @method[frame:standard-menus<%> edit-menu:preferences-help-string],
  @method[frame:standard-menus<%> edit-menu:preferences-on-demand],
  @method[frame:standard-menus<%> edit-menu:get-preferences-item]

  \item
  @method[frame:standard-menus<%> edit-menu:after-preferences]

  \item
  @method[frame:standard-menus<%> help-menu:before-about]

  \item
  @method[frame:standard-menus<%> help-menu:about-callback],
  @method[frame:standard-menus<%> help-menu:create-about?],
  @method[frame:standard-menus<%> help-menu:about-string],
  @method[frame:standard-menus<%> help-menu:about-help-string],
  @method[frame:standard-menus<%> help-menu:about-on-demand],
  @method[frame:standard-menus<%> help-menu:get-about-item]

  \item
  @method[frame:standard-menus<%> help-menu:after-about]

  \end{itemize}
  @defmethod*[(((get-menu%) (subclass?/c menu:can-restore-underscore-menu%)))]{
    The result of this method is used as the class
    for creating the result of these methods:
    @method[frame:standard-menus<%> get-file-menu], 
    @method[frame:standard-menus<%> get-edit-menu], 
    @method[frame:standard-menus<%> get-help-menu]. 


    defaultly returns
    @scheme[menu%]
  }
  @defmethod*[(((get-menu-item%) (subclass?/c menu-item%)))]{
    The result of this method is used as the class for creating
    the menu items in this frame (see 
    @scheme[frame:standard-menus%]
    for a list).


    defaultly returns
    @scheme[menu:can-restore-menu-item%].
  }
  @defmethod*[(((get-checkable-menu-item%) (subclass?/c checkable-menu-item%)))]{
    The result of this method is used as the class for creating
    checkable menu items in this class (see 
    @scheme[frame:standard-menus%]
    for a list).


    defaultly returns
    @scheme[menu:can-restore-checkable-menu-item%].
  }
  @defmethod*[(((get-file-menu) (instance (subclass?/c menu%))))]{
    Returns the file menu
    See also
    @method[frame:standard-menus<%> get-menu\%]

  }
  @defmethod*[(((get-edit-menu) (instance (subclass?/c menu%))))]{
    Returns the edit menu
    See also
    @method[frame:standard-menus<%> get-menu\%]

  }
  @defmethod*[(((get-help-menu) (instance (subclass?/c menu%))))]{
    Returns the help menu
    See also
    @method[frame:standard-menus<%> get-menu\%]

  }
  @defmethod*[(((file-menu:new-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the new menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (handler:edit-file #f) #t)
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-new-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:new-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant new-menu-item)"
  }
  @defmethod*[(((file-menu:new-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant new-info)}"
  }
  @defmethod*[(((file-menu:new-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-new?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((file-menu:between-new-and-open (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the new menu-item
    and before the addition of the open menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((file-menu:open-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the open menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (handler:open-file) #t)
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-open-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:open-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant open-menu-item)"
  }
  @defmethod*[(((file-menu:open-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant open-info)}"
  }
  @defmethod*[(((file-menu:open-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-open?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((file-menu:open-recent-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the open-recent menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (x y) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-open-recent-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:open-recent-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant open-recent-menu-item)"
  }
  @defmethod*[(((file-menu:open-recent-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant open-recent-info)}"
  }
  @defmethod*[(((file-menu:open-recent-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu) (handler:install-recent-items menu))}
  }
  @defmethod*[(((file-menu:create-open-recent?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((file-menu:between-open-and-revert (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the open menu-item
    and before the addition of the revert menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((file-menu:revert-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the revert menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-revert-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:revert-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant revert-menu-item)"
  }
  @defmethod*[(((file-menu:revert-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant revert-info)}"
  }
  @defmethod*[(((file-menu:revert-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-revert?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((file-menu:between-revert-and-save (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the revert menu-item
    and before the addition of the save menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((file-menu:save-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the save menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-save-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:save-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant save-menu-item)"
  }
  @defmethod*[(((file-menu:save-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant save-info)}"
  }
  @defmethod*[(((file-menu:save-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-save?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((file-menu:save-as-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the save-as menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-save-as-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:save-as-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant save-as-menu-item)"
  }
  @defmethod*[(((file-menu:save-as-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant save-as-info)}"
  }
  @defmethod*[(((file-menu:save-as-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-save-as?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((file-menu:between-save-as-and-print (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the save-as menu-item
    and before the addition of the print menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((file-menu:print-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the print menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-print-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:print-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant print-menu-item)"
  }
  @defmethod*[(((file-menu:print-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant print-info)}"
  }
  @defmethod*[(((file-menu:print-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-print?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((file-menu:between-print-and-close (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the print menu-item
    and before the addition of the close menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Adds a separator menu item.
  }
  @defmethod*[(((file-menu:close-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the close menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (when (can-close?) (on-close) (show #f)) #t)
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-close-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:close-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant close-menu-item)"
  }
  @defmethod*[(((file-menu:close-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant close-info)}"
  }
  @defmethod*[(((file-menu:close-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-close?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((file-menu:between-close-and-quit (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the close menu-item
    and before the addition of the quit menu-item to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((file-menu:quit-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the quit menu-item of the file-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (when (exit:user-oks-exit) (exit:exit)))
    \end{schemedisplay}
  }
  @defmethod*[(((file-menu:get-quit-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((file-menu:quit-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(if (eq? (system-type) (quote windows)) (string-constant quit-menu-item-windows) (string-constant quit-menu-item-others))"
  }
  @defmethod*[(((file-menu:quit-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant quit-info)}"
  }
  @defmethod*[(((file-menu:quit-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((file-menu:create-quit?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns (not (current-eventspace-has-standard-menus?))
  }
  @defmethod*[(((file-menu:after-quit (menu (instance (subclass?/c menu%)))) void))]{
    This method is called after the addition of the quit menu-item
    to the file-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((edit-menu:undo-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the undo menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'undo)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-undo-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:undo-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant undo-menu-item)"
  }
  @defmethod*[(((edit-menu:undo-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant undo-info)}"
  }
  @defmethod*[(((edit-menu:undo-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote undo))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-undo?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:redo-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the redo menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'redo)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-redo-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:redo-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant redo-menu-item)"
  }
  @defmethod*[(((edit-menu:redo-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant redo-info)}"
  }
  @defmethod*[(((edit-menu:redo-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote redo))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-redo?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-redo-and-cut (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the redo menu-item
    and before the addition of the cut menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Adds a separator menu item.
  }
  @defmethod*[(((edit-menu:cut-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the cut menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'cut)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-cut-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:cut-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant cut-menu-item)"
  }
  @defmethod*[(((edit-menu:cut-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant cut-info)}"
  }
  @defmethod*[(((edit-menu:cut-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote cut))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-cut?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-cut-and-copy (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the cut menu-item
    and before the addition of the copy menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((edit-menu:copy-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the copy menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'copy)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-copy-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:copy-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant copy-menu-item)"
  }
  @defmethod*[(((edit-menu:copy-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant copy-info)}"
  }
  @defmethod*[(((edit-menu:copy-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote copy))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-copy?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-copy-and-paste (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the copy menu-item
    and before the addition of the paste menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((edit-menu:paste-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the paste menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'paste)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-paste-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:paste-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant paste-menu-item)"
  }
  @defmethod*[(((edit-menu:paste-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant paste-info)}"
  }
  @defmethod*[(((edit-menu:paste-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote paste))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-paste?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-paste-and-clear (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the paste menu-item
    and before the addition of the clear menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((edit-menu:clear-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the clear menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'clear)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-clear-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:clear-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(if (eq? (system-type) (quote windows)) (string-constant clear-menu-item-windows) (string-constant clear-menu-item-windows))"
  }
  @defmethod*[(((edit-menu:clear-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant clear-info)}"
  }
  @defmethod*[(((edit-menu:clear-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote clear))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-clear?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-clear-and-select-all (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the clear menu-item
    and before the addition of the select-all menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((edit-menu:select-all-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the select-all menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (menu evt)
      (let ((edit (get-edit-target-object)))
        (when (and edit (is-a? edit editor<%>))
          (send edit do-edit-operation 'select-all)))
      #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-select-all-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:select-all-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant select-all-menu-item)"
  }
  @defmethod*[(((edit-menu:select-all-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant select-all-info)}"
  }
  @defmethod*[(((edit-menu:select-all-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (let* ((editor (get-edit-target-object)) (enable? (and editor (is-a? editor editor<\%>) (send editor can-do-edit-operation? (quote select-all))))) (send item enable enable?)))}
  }
  @defmethod*[(((edit-menu:create-select-all?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#t
  }
  @defmethod*[(((edit-menu:between-select-all-and-find (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the select-all menu-item
    and before the addition of the find menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Adds a separator menu item.
  }
  @defmethod*[(((edit-menu:find-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the find menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-find-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:find-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant find-menu-item)"
  }
  @defmethod*[(((edit-menu:find-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant find-info)}"
  }
  @defmethod*[(((edit-menu:find-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<\%>)))))}
  }
  @defmethod*[(((edit-menu:create-find?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((edit-menu:find-again-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the find-again menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-find-again-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:find-again-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant find-again-menu-item)"
  }
  @defmethod*[(((edit-menu:find-again-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant find-again-info)}"
  }
  @defmethod*[(((edit-menu:find-again-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<\%>)))))}
  }
  @defmethod*[(((edit-menu:create-find-again?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((edit-menu:replace-and-find-again-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the replace-and-find-again menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-replace-and-find-again-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:replace-and-find-again-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant replace-and-find-again-menu-item)"
  }
  @defmethod*[(((edit-menu:replace-and-find-again-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant replace-and-find-again-info)}"
  }
  @defmethod*[(((edit-menu:replace-and-find-again-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (item) (send item enable (let ((target (get-edit-target-object))) (and target (is-a? target editor<\%>)))))}
  }
  @defmethod*[(((edit-menu:create-replace-and-find-again?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((edit-menu:between-find-and-preferences (menu (instance (subclass?/c menu%)))) void))]{
    This method is called between the addition of the find menu-item
    and before the addition of the preferences menu-item to the edit-menu menu.
    Override it to add additional menus at that point.


    Adds a separator except when \rawscm{current-eventspace-has-standard-menus?} returns \rawscm{\#t}.
  }
  @defmethod*[(((edit-menu:preferences-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the preferences menu-item of the edit-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (preferences:show-dialog) #t)
    \end{schemedisplay}
  }
  @defmethod*[(((edit-menu:get-preferences-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((edit-menu:preferences-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant preferences-menu-item)"
  }
  @defmethod*[(((edit-menu:preferences-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant preferences-info)}"
  }
  @defmethod*[(((edit-menu:preferences-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((edit-menu:create-preferences?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns (not (current-eventspace-has-standard-menus?))
  }
  @defmethod*[(((edit-menu:after-preferences (menu (instance (subclass?/c menu%)))) void))]{
    This method is called after the addition of the preferences menu-item
    to the edit-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((help-menu:before-about (menu (instance (subclass?/c menu%)))) void))]{
    This method is called before the addition of the about menu-item
    to the help-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
  @defmethod*[(((help-menu:about-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{
    This method is called when the about menu-item of the help-menu menu is selected.


    Defaultly bound to:
    \begin{schemedisplay}
    (λ (item control) (void))
    \end{schemedisplay}
  }
  @defmethod*[(((help-menu:get-about-item) (instance menu-item%)))]{
    This method returns the \iscmclass{menu-item} that corresponds
    to this menu item.

  }
  @defmethod*[(((help-menu:about-string) string))]{
    The result of this method is the name of this menu.

    defaultly returns "(string-constant about-menu-item)"
  }
  @defmethod*[(((help-menu:about-help-string) string))]{
    This result of this method is used as the help string when the
    @scheme[menu-item%]
    object is created.


    Defaultly returns "\scheme{(string-constant about-info)}"
  }
  @defmethod*[(((help-menu:about-on-demand (item menu-item%)) void))]{
    The menu item's on-demand method calls this method


    Defaultly is this:
    \scheme{(λ (menu-item) (void))}
  }
  @defmethod*[(((help-menu:create-about?) boolean))]{
    The result of this method determines if the
    corresponding menu-item is created. Override this
    to control the creation of the menu-item.


    defaultly returns \#f
  }
  @defmethod*[(((help-menu:after-about (menu (instance (subclass?/c menu%)))) void))]{
    This method is called after the addition of the about menu-item
    to the help-menu menu.
    Override it to add additional menus at that point.


    Does nothing.
  }
}
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
    @method[frame:editor<%> get-canvas\%]
    method. 

  }
  @defmethod*[(((get-editor%) (is-a?/c editor<%>)))]{
    The result of this class is used to create the 
    @scheme[editor<%>]
    in this frame.

    Override this method to specify a different editor class.


    Returns the value of the init-field \scheme|editor%|.
  }
  @defmethod*[(((get-editor<%>) interface))]{
    The result of this method is used by 
    @method[frame:editor<%> make-editor]
    to check that 
    @method[frame:editor<%> get-editor\%]
    is returning a reasonable editor.


    Returns
    @scheme[editor<%>].
  }
  @defmethod*[(((make-editor) (instance (is-a?/c editor<%>))))]{
    This method is called to create the editor in this frame.
    It calls
    @method[frame:editor<%> get-editor<\%>]
    and uses that interface to make sure the result of
    @method[frame:editor<%> get-editor\%]
    is reasonable.



    Calls \rawscm{(make-object @method[frame:editor<%> get-editor\%])}.

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


    Returns \rawscm{\#f} if the user cancels this operation
    (only possible when the file has not been saved before and
    the user is prompted for a new filename) and returns
    \rawscm{\#t} if not.
  }
  @defmethod*[(((save-as (format (union (quote guess) (quote standard) (quote text) (quote text-force-cr) (quote same) (quote copy)) (quote same))) boolean))]{
    Queries the use for a file name and saves the file with that name.


    Returns \rawscm{\#f} if the user cancells the file-choosing
    dialog and returns \rawscm{\#t} otherwise.
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

    Returns \scheme|#t| if the filename is the file that this
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

    returns \#t
  }
  @defmethod*[#:mode override (((file-menu:save-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Saves the file in the editor.
  }
  @defmethod*[#:mode override (((file-menu:create-save?) boolean))]{

    returns \#t
  }
  @defmethod*[#:mode override (((file-menu:save-as-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Prompts the user for a file name and uses that filename to save the buffer. 
    Calls
    @method[frame:editor<%> save-as]
    with no arguments.
  }
  @defmethod*[#:mode override (((file-menu:create-save-as?) boolean))]{

    returns \#t
  }
  @defmethod*[#:mode override (((file-menu:print-callback (item (is-a?/c menu-item%)) (evt (is-a?/c control-event%))) void))]{

    Calls the
    @method[editor<%> print]
    method of
    @scheme[editor<%>]
    with the default arguments, except that
    the \var{output-mode} argument
    is the result of calling
    @scheme[preferences:get]
    with \rawscm{'framework:print-output-mode}.
  }
  @defmethod*[#:mode override (((file-menu:create-print?) boolean))]{

    returns \#t
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

    returns \#t
  }
}
@definterface[frame:open-here<%> (frame:editor<%>)]{
  Frames implementing this mixin can change the file they are
  displaying. 

  The frame is only re-used when the
  \scheme|'framework:open-here?| preference is set
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

    Opens \var{filename} in the current frame, possibly
    prompting the user about saving a file (in which case the
    frame might not get switched).
  }
}
@defmixin[frame:open-here-mixin (frame:editor<%>) (frame:open-here<%>)]{
  Provides an implementation of
  @scheme[frame:open-here<%>]
  @defmethod*[#:mode override (((file-menu:new-on-demand (item (is-a?/c menu-item%))) void))]{

    Sets the label of \var{item} to 
    "New..." if the preference \scheme|'framework:open-here?| is set. 
  }
  @defmethod*[#:mode override (((file-menu:new-callback (item (instance (subclass?/c menu-item%))) (evt (instance control-event%))) void))]{

    When the preference \scheme|'framework:open-here?|
    preference is set, this method prompts the user, asking if
    they would like to create a new frame, or just clear out
    this one. If they clear it out and the file hasn't been
    saved, they are asked about saving.
  }
  @defmethod*[#:mode override (((file-menu:open-on-demand (item (is-a?/c menu-item%))) void))]{

    Sets the label of \var{item} to 
    "Open Here..." if the preference \scheme|'framework:open-here?| is set. 
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Calls 
    @method[group:% set-open-here-frame]
    with \scheme|#f| if 
    the result of 
    @method[group:% get-open-here-frame]
    is \scheme|eq?| to \scheme|this|.
  }
  @defmethod*[#:mode override (((on-activate (on? boolean)) void))]{

    When \var{on?} is \scheme|#t|, calls
    @method[group:% set-open-here-frame]
    with \scheme|this|.
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
    \scheme|editor%| init or, if none was supplied, it passes \scheme|text%|.
  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns \rawscm{(class->interface \iscmclass{text})}.
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
    \scheme|editor%| init or, if none was supplied, it passes \scheme|pasteboard%|.

  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns \rawscm{(class->interface \iscmclass{pasteboard})}.
  }
}
@definterface[frame:delegate<%> (frame:status-line<%> frame:text<%>)]{
  Frames that implement this interface provide a 20,000 feet
  overview of the text in the main editor. The term {\bf
  delegate} in these method descriptions refers to the
  original editor and the term {\bf delegatee} refers to the
  editor showing the 20,000 feet overview.
  @defmethod*[(((get-delegated-text) (instanceof (is-a?/c text:delegate<%>))))]{
    Returns the delegate text.

  }
  @defmethod*[(((delegated-text-shown?) boolean))]{
    Returns \rawscm{\#t} if the delegate is visible, and
    \rawscm{\#f} if it isn't.

  }
  @defmethod*[(((hide-delegated-text) void))]{
    Hides the delegated text.

    When the delegated text is hidden, it is not being
    updated. This is accomplished by calling the 
    @method[text:delegate<%> set-delegate]
    method of
    @method[frame:editor<%> get-editor]with \rawscm{\#f}.

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
  \iscmintf{text:delegate} and \iscmmixin{text:delegate-mixin}
  @defmethod*[#:mode override (((make-root-area-container (class (subclass?/c panel%)) (parent (instanceof (subclass?/c panel%)))) (is-a?/c panel%)))]{

    adds a panel outside to hold the delegate
    \iscmclass{editor-canvas} and \iscmclass{text}.

  }
  @defmethod*[#:mode override (((get-editor<%>) interface))]{

    Returns \iscmintf{text:delegate}.

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


    If \var{dir} is \rawscm{1} searches will be performed forwards and if 
    \var{dir} is \rawscm{-1} searches will be performed backwards.
  }
  @defmethod*[(((replace&search) boolean))]{
    Calls
    @method[frame:searchable<%> replace]
    and if it returns \rawscm{\#t}, calls 
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
    successful, \rawscm{\#t} is returned. Otherwise, \rawscm{\#f} is returned.

  }
  @defmethod*[(((can-replace?) boolean))]{
    Returns \rawscm{\#t} if a replace command would succeed. 


    Defaultly is \rawscm{\#t} when the selected text in the result of
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


    Returns \rawscm{\#t} if the text is found and sets the selection to the
    found text. If the text is not found it returns \rawscm{\#f}.
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

    returns \#t
  }
  @defmethod*[#:mode override (((edit-menu:find-again-callback) boolean))]{

    Returns \rawscm{\#t}, and searches for the same text that was last
    searched for in the text.
  }
  @defmethod*[#:mode override (((edit-menu:create-find-again?) boolean))]{

    returns \#t
  }
  @defmethod*[#:mode override (((edit-menu:replace-and-find-again-callback) boolean))]{

    Returns \rawscm{\#t}, and if the selected text matches the current text
    in the find box, replaces it with the contents of the replace box and
    searches for the next occurrence of the text in the find box.


  }
  @defmethod*[#:mode override (((edit-menu:replace-and-find-again-on-demand (item menu-item%)) void))]{

    Disables \var{item} when
    @method[frame:searchable<%> can-replace?]
    returns \rawscm{\#f} and enables it when that method returns
    \rawscm{\#t}.
  }
  @defmethod*[#:mode override (((edit-menu:create-replace-and-find-again?) boolean))]{

    returns \#t
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

@(include-extracted (lib "main.ss" "framework") #rx"^frame:")
