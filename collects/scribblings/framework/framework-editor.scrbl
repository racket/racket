#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework/framework))
@(require (for-label scheme/gui))
@title{Editor}

@definterface[editor:basic<%> (editor<%>)]{
  Classes matching this interface support the basic 
  @scheme[editor<%>]
  functionality required by the framework.
  @defmethod*[(((has-focus?) boolean))]{
    This function returns \rawscm{\#t} when the editor has the keyboard
    focus. It is implemented using:
    @method[editor<%> on-focus]

  }
  @defmethod*[(((local-edit-sequence?) boolean))]{
    Indicates if this editor is in an edit sequence. Enclosing buffer's
    edit-sequence status is not considered by this method.

    See
    @method[editor<%> begin-edit-sequence]
    and
    @method[editor<%> end-edit-sequence]
    for more info about edit sequences.

  }
  @defmethod*[(((run-after-edit-sequence (thunk (-> void)) (tag (union symbol? |#f|) |#f|)) void))]{
    This method is used to install callbacks that will be run after any
    edit-sequence completes.


    The procedure \var{thunk} will be called immediately if the edit is
    not in an edit-sequence. If the edit is in an edit-sequence, it will
    be called when the edit-sequence completes.

    If \var{tag} is a symbol, the \var{thunk} is keyed on that symbol, and
    only one thunk per symbol will be called after the
    edit-sequence. Specifically, the last call to 
    @method[editor:basic<%> run-after-edit-sequence]'s argument will be called.

  }
  @defmethod*[(((get-top-level-window) (union |#f| (is-a?/c top-level-window<%>))))]{
    Returns the 
    @scheme[top-level-window<%>]
    currently associated with this buffer.

    This does not work for embedded editors.


  }
  @defmethod*[(((save-file-out-of-date?) boolean))]{
    Returns \rawscm{\#t} if the file on disk has been modified, by some other program.

  }
  @defmethod*[(((save-file/gui-error (filename (union path |#f|) |#f|) (format (union (quote guess) (quote standard) (quote text) (quote text-force-cr) same copy) (quote same)) (show-errors? boolean |#t|)) boolean))]{
    This method is an alternative to 
    @method[editor<%> save-file]. Rather than showing errors via the original stdout, it
    opens a dialog with an error message showing the error.

    The result indicates if an error happened (the error has
    already been shown to the user). It returns \rawscm{\#t} if
    no error occurred and \rawscm{\#f} if an error occurred.

  }
  @defmethod*[(((load-file/gui-error (filename (union string |#f|) |#f|) (format (union (quote guess) (quote standard) (quote text) (quote text-force-cr) (quote same) (quote copy)) (quote guess)) (show-errors? boolean |#t|)) boolean))]{
    This method is an alternative to 
    @method[editor<%> load-file]. Rather than showing errors via the original stdout, it
    opens a dialog with an error message showing the error.

    The result indicates if an error happened (the error has
    already been shown to the user). It returns \rawscm{\#t} if
    no error occurred and \rawscm{\#f} if an error occurred.

  }
  @defmethod*[(((on-close) void))]{
    This method is called when an editor is closed. See also
    @method[editor:basic<%> can-close?]
    and
    @method[editor:basic<%> close].


    Does nothing.
  }
  @defmethod*[(((can-close?) boolean))]{
    This method is called to query the editor if is okay to
    close the editor. Although there is no visible effect
    associated with closing an editor, there may be some cleanup
    actions that need to be run when the user is finished with
    the editor (asking if it should be saved, for example).

    See also
    @method[editor:basic<%> on-close]and
    @method[editor:basic<%> close].


    Returns \scheme|#t|.
  }
  @defmethod*[(((close) boolean))]{
    This method is merely
    \begin{schemedisplay}
    (if (can-close?)
        (begin (on-close) #t)
        #f)
    \end{schemedisplay}
    It is intended as a shorthand, helper method for closing
    an editor. See also
    @method[editor:basic<%> can-close?]
    and 
    @method[editor:basic<%> on-close].

  }
  @defmethod*[(((get-filename/untitled-name) string))]{
    Returns the printed version of the filename for this
    editor. If the editor doesn't yet have a filename, it
    returns a symbolic name (something like "Untitled").

  }
}
@defmixin[editor:basic-mixin (editor<%>) (editor:basic<%>)]{
  This provides the basic editor services required by the rest of the
  framework.

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.

  Each instance of a class created with this mixin contains a private
  \iscmclass{keymap} that is chained to the global keymap via:
  \rawscm{(send \var{keymap} chain-to-keymap (keymap:get-global) \#f)}.

  This installs the global keymap \iscmprocedure{keymap:get-global} to
  handle keyboard and mouse mappings not handled by \var{keymap}. The
  global keymap is created when the framework is invoked.
  @defmethod*[#:mode augment (((can-save-file? (filename string) (format symbol?)) boolean))]{

    Checks to see if the file on the disk has been modified out
    side of this editor, using
    @method[editor:basic<%> save-file-out-of-date?]. 
    If it has, this method prompts the user to be sure they want to save.

  }
  @defmethod*[#:mode augment (((after-save-file (success? boolean)) void))]{

    If the current filename is not a temporary filename, this method calls
    @scheme[handler:add-to-recent]with the current filename.

    to add the new filename to the list of recently opened files.

    Additionally, updates a private instance variable with the
    modification time of the file, for using in implementing
    @method[editor:basic<%> save-file-out-of-date?].

  }
  @defmethod*[#:mode augment (((after-load-file (success? boolean)) void))]{

    Updates a private instance variable with the modification
    time of the file, for using in implementing
    @method[editor:basic<%> save-file-out-of-date?]

  }
  @defmethod*[#:mode override (((on-focus (on? boolean)) void))]{
    Manages the state to implement
    @method[editor:basic<%> has-focus?]

  }
  @defmethod*[#:mode augment (((on-edit-sequence) boolean))]{

    Always returns \rawscm{\#t}. Updates a flag for
    @method[editor:basic<%> local-edit-sequence?]
  }
  @defmethod*[#:mode augment (((after-edit-sequence) void))]{

    Helps to implement
    @method[editor:basic<%> run-after-edit-sequence].
  }
  @defmethod*[#:mode override (((on-new-box (type (union (quote pasteboard) (quote text)))) (instance editor-snip%)))]{

    Creates instances of 
    @scheme[pasteboard:basic%]
    or
    @scheme[text:basic%]
    instead of the built in
    @scheme[pasteboard%]
    and
    @scheme[text%]
    classes. 
  }
  @defmethod*[#:mode override (((get-file (directory (or/c path-string? false/c))) string))]{

    Uses
    @scheme[finder:get-file]
    to find a filename. Also, sets the parameter
    @scheme[finder:dialog-parent-parameter]
    to the result of
    @method[editor:basic<%> get-top-level-window].
  }
  @defmethod*[#:mode override (((put-file (directory (or/c path? false/c)) (default-name (or/c path? false/c))) string))]{

    Uses
    @scheme[finder:put-file]
    to find a filename. Also, sets the parameter
    @scheme[finder:dialog-parent-parameter]
    to the result of
    @method[editor:basic<%> get-top-level-window].
  }
}
@definterface[editor:standard-style-list<%> (editor<%>)]{
  This interface is implemented by the results of
  @scheme[editor:standard-style-list-mixin].
}
@defmixin[editor:standard-style-list-mixin (editor<%>) (editor:standard-style-list<%>)]{
  The mixin adds code to the initialization
  of the class that sets the editor's style
  list (via
  @method[editor<%> set-style-list])
  to the result of
  @scheme[editor:get-standard-style-list].

  In addition, it calls
  @method[editor<%> set-load-overwrites-styles]
  with \scheme|#f|.
  This ensures that saved files with different
  settings for the style list do not clobber
  the shared style list.
}
@definterface[editor:keymap<%> (editor:basic<%>)]{
  Classes matching this interface add support for mixing in multiple
  keymaps. They provides an extensible interface to chained keymaps,
  through the
  @method[editor:keymap<%> get-keymaps]
  method.

  This editor is initialized by calling
  @scheme[add-editor-keymap-functions],
  @scheme[add-text-keymap-functions], and
  @scheme[add-pasteboard-keymap-functions].
  @defmethod*[(((get-keymaps) (list-of (instance keymap%))))]{
    The keymaps returned from this method are chained to this
    \iscmintf{editor}'s keymap.

    The result of this method should not change -- that is, it
    should return the same list of keymaps each time it is
    called.


    Defaultly returns \rawscm{(list
    @scheme[keymap:get-global])}
  }
}
@defmixin[editor:keymap-mixin (editor:basic<%>) (editor:keymap<%>)]{
  This provides a mixin that implements the
  @scheme[editor:keymap<%>]
  interface.
}
@definterface[editor:autowrap<%> (editor:basic<%>)]{
  Classes implementing this interface keep the
  @method[editor<%> auto-wrap]
  state set based on the
  \rawscm{'framework:auto-set-wrap?} preference
  (see 
  \hyperref{the preferences section}{section~}{ for more info about preferences}{fw:preferences}).

  They install a preferences callback with
  @scheme[preferences:add-callback]
  that sets the state when the preference changes and 
  initialize the value of
  @method[editor<%> auto-wrap]
  to the current value of \rawscm{'framework:auto-set-wrap?}
  via
  @scheme[preferences:get].
}
@defmixin[editor:autowrap-mixin (editor:basic<%>) (editor:autowrap<%>)]{
  See
  @scheme[editor:autowrap<%>]
}
@definterface[editor:file<%> (editor:keymap<%>)]{
  Objects supporting this interface are expected to support files.
  @defmethod*[(((get-can-close-parent) (union false (is-a/c? frame%) (is-a/c? dialog%))))]{
    The result of this method is used as the parent for the
    dialog that asks about closing.


    Defaultly returns \scheme|#f|.
  }
  @defmethod*[(((update-frame-filename) void))]{
    Attempts to find a frame that displays this editor. If it
    does, it updates the frame's title based on a new filename
    in the editor.

  }
  @defmethod*[(((allow-close-with-no-filename?) boolean))]{
    This method indicates if closing the file when it hasn't
    been saved is a reason to alert the user. See also
    @method[editor:file-mixin can-close?].


    Defaultly returns \scheme|#f|.
  }
}
@defmixin[editor:file-mixin (editor:keymap<%>) (editor:file<%>)]{
  This editor locks itself when the file that is opened is read-only in
  the filesystem.

  The class that this mixin produces uses the same initialization
  arguments as it's input.
  @defmethod*[#:mode override (((set-filename (name string) (temp? boolean |#f|)) void))]{

    Updates the filename on each frame displaying this editor, for each
    frame that matches
    @scheme[frame:editor<%>].
  }
  @defmethod*[#:mode augment (((can-close?) boolean))]{

    If the 
    @method[editor:file<%> allow-close-with-no-filename?]
    method returns \scheme|#f|, this method checks to see if the file
    has been saved at all yet. If not, it asks the user
    about saving (and saves if they ask).

    If the 
    @method[editor:file<%> allow-close-with-no-filename?]
    method returns \scheme|#t|, this method does as before,
    except only asks if the editor's 
    @method[editor<%> get-filename]method returns a path.

    Also calls inner.
  }
  @defmethod*[#:mode override (((get-keymaps) (list-of (instance keymap%))))]{

    This returns a list containing the super-class's keymaps, plus the
    result of
    @scheme[keymap:get-file]
  }
}
@definterface[editor:backup-autosave<%> (editor:basic<%>)]{
  Classes matching this interface support backup files and autosaving.
  @defmethod*[(((backup?) boolean))]{
    Indicates weather this 
    @scheme[editor<%>]
    should be backed up.


    Returns the value of the 
    @scheme[preferences:get]
    applied to
    \rawscm{'framework:backup-files?}.
    \index{'framework:backup-files?}

  }
  @defmethod*[(((autosave?) boolean))]{
    Indicates weather this 
    @scheme[editor<%>]
    should be autosaved.


    Returns \rawscm{\#t}.
  }
  @defmethod*[(((do-autosave) (union |#f| string)))]{
    This method is called to perform the autosaving.
    See also
    @scheme[autosave:register]


    When the file has been modified since it was last saved and autosaving
    it turned on (via the
    @method[editor:backup-autosave<%> autosave?]
    method) an autosave file is created for this 
    @scheme[editor<%>].

    Returns the filename where the autosave took place, or
    \rawscm{\#f} if none did.
  }
  @defmethod*[(((remove-autosave) void))]{
    This method removes the autosave file associated with this
    @scheme[editor<%>].


  }
}
@defmixin[editor:backup-autosave-mixin (editor:basic<%>) (editor:backup-autosave<%> autosave:autosavable<%>)]{
  This mixin adds backup and autosave functionality to an editor.

  During initialization, this object is registered
  with 
  @scheme[autosave:register].

  The result of this mixin uses the same initialization arguments as the
  mixin's argument.

  @defmethod*[#:mode augment (((on-save-file (filename path?) (format (one-of/c (quote guess) (quote standard) (quote text) (quote text-force-cr) (quote same) (quote copy)))) bool))]{

    If a backup file has not been created this session for this file,
    deletes any existing backup file and copies the old save file into the
    backup file. For the backup file's name, see
    @scheme[path-utils:generate-backup-name]
  }
  @defmethod*[#:mode augment (((on-close) void))]{

    Deletes the autosave file and turns off autosaving.
  }
  @defmethod*[#:mode augment (((on-change) void))]{

    Sets a flag indicating that this \iscmintf{editor} needs to be autosaved.
  }
  @defmethod*[#:mode override (((set-modified (modified? any/c)) void))]{

    If the file is no longer modified, this method deletes the autosave
    file. If it is, it updates a flag to indicate that the autosave file
    is out of date.
  }
}
@definterface[editor:info<%> (editor:basic<%>)]{
  An
  @scheme[editor<%>]
  matching this interface provides information about its lock state to its
  @scheme[top-level-window<%>].
}
@defmixin[editor:info-mixin (editor:basic<%>) (editor:info<%>)]{
  This editor tells the frame when it is locked and unlocked.
  See also
  @scheme[frame:text-info<%>].

  @defmethod*[#:mode override (((lock (lock? boolean)) void))]{

    Uses 
    @method[editor:basic<%> run-after-edit-sequence]
    to call
    @method[frame:info<%> lock-status-changed].
  }
}
@(require framework/framework-docs)
@(def-fw-procs editor)
