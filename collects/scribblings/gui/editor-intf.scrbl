#lang scribble/doc
@(require "common.ss")

@definterface/title[editor<%> ()]{

The @scheme[editor<%>] interface is implemented by @scheme[text%] and
 @scheme[pasteboard%].

@defmethod[(add-canvas [canvas (is-a?/c editor-canvas%)])
           void?]{

Adds a canvas to this editor's list of displaying canvases. (See
@method[editor<%> get-canvases].)

Normally, this method is called only by @xmethod[editor-canvas%
 set-editor].

}

@defmethod[(add-undo [undoer (-> any)])
           void?]{

Adds an undoer procedure to the editor's undo stack. If an undo is
 currently being performed, the undoer is added to the editor's redo
 stack. The undoer is called by the system when it is undoing (or
 redoing) changes to an editor, and when this undoer is the first item
 on the undo (or redo) stack.

The system automatically installs undo records to undo built-in editor
 operations, such as inserts, deletes, and font changes.  Install an
 undoer only when it is necessary to maintain state or handle
 operations that are not built-in. For example, in a program where the
 user can assign labels to snips in a pasteboard, the program should
 install an undoer to revert a label change. Thus, when a user changes
 a snip's label and then selects @onscreen{Undo} (from a standard menu
 bar), the snip's label will revert as expected. In contrast, there is
 no need to install an undoer when the user moves a snip by dragging
 it, because the system installs an appropriate undoer automatically.

After an undoer returns, the undoer is popped off the editor's undo
 (or redo) stack; if the return value is true, then the next undoer is
 also executed as part of the same undo (or redo) step.  The undoer
 should return true if the action being undone was originally
 performed as part of a @method[editor<%> begin-edit-sequence] and
 @method[editor<%> end-edit-sequence] sequence. The return value
 should also be true if the undone action was implicitly part of a
 sequence. To extend the previous example, if a label change is paired
 with a move to realign the snip, then the label-change undoer should
 be added to the editor @italic{after} the call to @method[pasteboard%
 move], and it should return @scheme[#t] when it is called. As a
 result, the move will be undone immediately after the label change is
 undone. (If the opposite order is needed, use @method[editor<%>
 begin-edit-sequence] and @method[editor<%> end-edit-sequence] to
 create an explicit sequence.)

The system adds undoers to an editor (in response to other method
 calls) without calling this method.

}

@defmethod[(adjust-cursor [event (is-a?/c mouse-event%)])
           (or/c (is-a?/c cursor%) false/c)]{

@methspec{

Gets a cursor to be used in the editor's @techlink{display}.  If the
 return value is @scheme[#f], a default cursor is used.

See also @method[editor<%> set-cursor].

}
@methimpl{

If an overriding cursor has been installed with
@method[editor<%> set-cursor], then the installed cursor is returned.

Otherwise, if the event is a dragging event, a snip in the editor has
the focus, and the snip's
@method[snip% adjust-cursor] method returns a cursor, that cursor is returned.

Otherwise, if the cursor is over a snip and the snip's
@method[snip% adjust-cursor] method returns a cursor, that cursor is returned.

Otherwise, if a cursor has been installed with
@method[editor<%> set-cursor], then the installed cursor is returned.

Otherwise, if the cursor is over a clickback region in an editor, an
arrow cursor is returned.

Finally, if none of the above cases apply, a default cursor is
returned. For a text editor, the default cursor is an I-beam. For a
pasteboard editor, the default cursor is an arrow.

}}

@defmethod[#:mode pubment 
           (after-edit-sequence)
           void?]{

@methspec{

Called after a top-level edit sequence completes (involving unnested
@method[editor<%> begin-edit-sequence] and @method[editor<%>
end-edit-sequence]).

See also @method[editor<%> on-edit-sequence].

}

@methimpl{

Does nothing.

}

}

@defmethod[#:mode pubment 
           (after-load-file [success? any/c])
           void?]{

@methspec{

Called just after the editor is loaded from a file. 

The argument to the method originally specified whether the save was
successful, but failures now trigger exceptions such that the method is
not even called. Consequently, the argument is always @scheme[#t].

See also
@method[editor<%> can-load-file?] and
@method[editor<%> on-load-file].

}

@methimpl{

Does nothing.

}
}

@defmethod[#:mode pubment 
           (after-save-file [success? any/c])
           void?]{

@methspec{

Called just after the editor is saved to a file.

The argument to the method originally specified whether the save was
successful, but failures now trigger exceptions such that the method is
not even called. Consequently, the argument is always @scheme[#t].

See also
@method[editor<%> can-save-file?] and
@method[editor<%> on-save-file].

}
@methimpl{

Does nothing.

}
}

@defmethod*[([(auto-wrap)
              boolean?]
             [(auto-wrap [auto-wrap? any/c])
              void?])]{

Enables or disables automatically calling @method[editor<%>
set-max-width] in response to @method[editor<%> on-display-size], or
gets the state of auto-wrapping. For text editors, this has the effect
of wrapping the editor's contents to fit in a canvas displaying the
editor (the widest one if multiple canvases display the editor). For
pasteboard editors, ``auto-wrapping'' merely truncates the area of the
pasteboard to match its canvas @techlink{display}.

When the wrapping mode is changed, the @method[editor<%>
on-display-size] method is called immediately to update the editor's
maximum width.

Auto-wrapping is initially disabled. 

}

@defmethod[(begin-edit-sequence [undoable? any/c #t]
                                [interrupt-streak? any/c #t])
           void?]{

@methspec{

The @method[editor<%> begin-edit-sequence] and @method[editor<%>
 end-edit-sequence] methods are used to bracket a set of editor
 modifications so that the results are all displayed at once. The
 commands may be nested arbitrarily deep. Using these functions can
 greatly speed up displaying the changes.

When an editor contains other editors, using @method[editor<%>
 begin-edit-sequence] and @method[editor<%> end-edit-sequence] on the
 main editor brackets some changes to the sub-editors as well, but it
 is not as effective when a sub-editor changes as calling
 @method[editor<%> begin-edit-sequence] and @method[editor<%>
 end-edit-sequence] for the sub-editor.

See also @method[editor<%> refresh-delayed?] and @method[editor<%>
 in-edit-sequence?], and see @secref["editorthreads"] for
 information about edit sequences and refresh requests.

If the @scheme[undoable?] flag is @scheme[#f], then the changes made
 in the sequence cannot be reversed through the @method[editor<%>
 undo] method. This flag is only effective for the outermost
 @method[editor<%> begin-edit-sequence] when nested sequences are
 used. Note that, for a @scheme[text%] object, the character-inserting
 version of @method[text% insert] interferes with sequence-based undo
 groupings.

If the @scheme[interrupt-streak?] flag is @scheme[#f] and the sequence is
 outermost, then special actions before and after the sequence count
 as consecutive actions. For example, kills just before and after the
 sequence are appended in the copy buffer.

}
@methimpl{

Starts a sequence.

}}

@defmethod[(begin-write-header-footer-to-file [f (is-a?/c editor-stream-out%)]
                                              [name string?]
                                              [buffer (box/c (and/c exact? integer?))])
           void?]{

This method must be called before writing any special header data to a
stream. See @|filediscuss| and @method[editor<%>
write-headers-to-file] for more information.

The @scheme[name] string must be a unique name that can be used by a
 header reader to recognize the data. This method will store a value
 in @scheme[buffer] that should be passed on to @method[editor<%>
 end-write-header-footer-to-file].

}

@defmethod[(blink-caret)
           void?]{

@methspec{

Tells the editor to blink the selection caret. This method is 
called periodically when the editor's @techlink{display} has the keyboard
focus.

}
@methimpl{

Propagates the request to any snip with the editor-local focus.

}}

@defmethod[(can-do-edit-operation? [op (one-of/c 'undo 'redo 'clear 'cut 'copy 'paste 
                                                 'kill 'select-all 'insert-text-box 
                                                 'insert-pasteboard-box 'insert-image)]
                                   [recursive? any/c #t])
           boolean?]{
@methspec{

Checks whether a generic edit command would succeed for the editor.
 This check is especially useful for enabling and disabling menus on
 demand. See @method[editor<%> do-edit-operation] for information
 about the @scheme[op] and @scheme[recursive?] arguments.

}
@methimpl{

Allows the operation depending on the selection, whether the editor is
locked, etc.

}}

@defmethod[#:mode pubment 
           (can-load-file? [filename path?]
                           [format (one-of/c 'guess 'same 'copy 'standard
                                             'text 'text-force-cr)])
           boolean?]{
@methspec{

Called just before the editor is loaded from a file. If the return
value is @scheme[#f], the file is not loaded. See also
@method[editor<%> on-load-file] and @method[editor<%>
after-load-file].

The @scheme[filename] argument is the name the file will be loaded
 from. See @method[editor<%> load-file] for information about
 @scheme[format].

Note that the @scheme[filename] argument cannot be a string; it must
be a path value.
 
}
@methimpl{

Returns @scheme[#t].

}}

@defmethod[#:mode pubment 
           (can-save-file? [filename path?]
                           [format (one-of/c 'guess 'same 'copy 'standard
                                             'text 'text-force-cr)])
           boolean?]{
@methspec{

Called just before the editor is saved to a file. If the return value
is @scheme[#f], the file is not saved. See also @method[editor<%>
on-save-file] and @method[editor<%> after-save-file].

The @scheme[filename] argument is the name the file will be saved
 to. See @method[editor<%> load-file] for information about
 @scheme[format].

Note that the @scheme[filename] argument cannot be a string; it must
 be a path value.

}

@methimpl{

Returns @scheme[#t].

}}

@defmethod*[([(change-style [delta (or/c (is-a?/c style-delta%) false/c)])
              void?]
             [(change-style [style (or/c (is-a?/c style<%>) false/c)])
              void?])]{

Changes the style for @techlink{items} in the editor, either by
 applying a style delta or using a specific style.

To change a large collection of snips from one style to another style,
 consider providing a @scheme[style<%>] instance rather than a
 @scheme[style-delta%] instance. Otherwise, @method[editor<%>
 change-style] must convert the @scheme[style-delta%] instance to the
 @scheme[style<%>] instance for every snip; this conversion consumes
 both time and (temporary) memory.

@MonitorMethod[@elem{The style within an editor} @elem{the
 system (in response to other method calls)} @elem{@xmethod[text% on-change-style]} @elem{style}]

}

@defmethod[(clear)
           void?]{

Deletes the currently selected @techlink{item}s.

@|OnDeleteNote|

}

@defmethod[(clear-undos)
           void?]{

Destroys the undo history of the editor.

}

@defmethod[(copy [extend? any/c #f]
                 [time (and/c exact? integer?) 0])
           void?]{

Copies @techlink{item}s into the clipboard. If @scheme[extend?] is not
 @scheme[#f], the old clipboard contents are appended.

The system may execute a copy (in response to other method calls)
 without calling this method. To extend or re-implement copying,
 override the @xmethod[text% do-copy] or @xmethod[pasteboard% do-copy]
 method of an editor.

See @|timediscuss| for a discussion of the @scheme[time] argument.  If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.


}

@defmethod[(copy-self)
           (or/c (is-a?/c text%) (is-a?/c pasteboard%))]{

Creates a new editor with the same properties as this one.  After an
 editor is created (either a @scheme[text%] or @scheme[pasteboard%]
 instance, as appropriate), the new editor is passed to
 @method[editor<%> copy-self-to].

}


@defmethod[(copy-self-to [dest (or/c (is-a?/c text%) (is-a?/c pasteboard%))])
           void?]{

Copies the properties of @this-obj[] to @scheme[dest].

Each snip in @this-obj[] is copied and inserted into @scheme[dest].
In addition, @this-obj[]'s filename, maximum undo history setting,
keymap, interactive caret threshold, and overwrite-styles-on-load
settings are installed into @scheme[dest]. Finally, @this-obj[]'s
style list is copied and the copy is installed as the style list for
@scheme[dest].

}

@defmethod[(cut [extend? any/c #f]
                [time (and/c exact? integer?) 0])
           void?]{

Copies and then deletes the currently selected @techlink{item}s. If
 @scheme[extend?]  is not @scheme[#f], the old clipboard contents are
 appended.

The system may execute a cut (in response to other method calls)
 without calling this method. To extend or re-implement the copying
 portion of the cut, override the @xmethod[text% do-copy] or
 @xmethod[pasteboard% do-copy] method of an editor. To monitor
 deletions in an editor, override @xmethod[text% on-delete] or
 @xmethod[pasteboard% on-delete].

See @|timediscuss| for a discussion of the @scheme[time] argument.  If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(dc-location-to-editor-location [x real?]
                                           [y real?])
           (values real? real?)]{

Converts the given coordinates from top-level @techlink{display}
 coordinates (usually canvas coordinates) to editor
 @techlink{location} coordinates.  The same calculation is performed
 by @method[editor<%> global-to-local].

@|OVD|

See also @method[editor<%> editor-location-to-dc-location].

}

@defmethod[(default-style-name)
           string?]{

Returns the name of a style to be used for newly inserted text,
 etc. The default is @scheme["Standard"].

}

@defmethod[(do-edit-operation [op (one-of/c 'undo 'redo 'clear 'cut 'copy 'paste 
                                            'kill 'select-all 'insert-text-box 
                                            'insert-pasteboard-box 'insert-image)]
                              [recursive? any/c #t]
                              [time (and/c exact? integer?) 0])
           void?]{

Performs a generic edit command. The @scheme[op] argument must be a
valid edit command, one of:

@itemize{
@item{@scheme['undo] --- undoes the last operation}
@item{@scheme['redo] --- undoes the last undo}
@item{@scheme['clear] --- deletes the current selection}
@item{@scheme['cut] --- cuts}
@item{@scheme['copy] --- copies}
@item{@scheme['paste] --- pastes}
@item{@scheme['kill] --- cuts to the end of the current line, or cuts a newline if there is only whitespace between the selection and end of line}
@item{@scheme['select-all] --- selects everything in the editor}
@item{@scheme['insert-text-box] --- inserts a text editor as an @techlink{item} in this editor; see also
@method[editor<%> on-new-box] .}
@item{@scheme['insert-pasteboard-box] --- inserts a pasteboard editor as an @techlink{item} in this editor; see also
@method[editor<%> on-new-box] .}
@item{@scheme['insert-image] --- gets a filename from the user and inserts the image as an @techlink{item} in this editor; see also
@method[editor<%> on-new-image-snip] .}
}

If @scheme[recursive?] is not @scheme[#f], then the command is passed on to
 any active snips of this editor (i.e., snips which own the caret).

See @|timediscuss| for a discussion of the @scheme[time] argument. If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

}

@defmethod[(editor-location-to-dc-location [x real?]
                                           [y real?])
           (values real? real?)]{

Converts the given coordinates from editor @techlink{location}
 coordinates to top-level @techlink{display} coordinates (usually
 canvas coordinates).  The same calculation is performed by
 @method[editor<%> local-to-global].

@|OVD|

See also @method[editor<%> dc-location-to-editor-location].

}

@defmethod[(end-edit-sequence)
           void?]{

See @method[editor<%> begin-edit-sequence].

}

@defmethod[(end-write-header-footer-to-file [f (is-a?/c editor-stream-out%)]
                                            [buffer-value (and/c exact? integer?)])
           void?]{

This method must be called after writing any special header data to a
stream. The @scheme[buffer-value] argument must be the value put in
the @scheme[buffer] argument box by @method[editor<%>
begin-write-header-footer-to-file].

See @|filediscuss| and @method[editor<%> write-headers-to-file] for
more information.

}


@defmethod[(find-first-snip)
           (or/c (is-a?/c snip%) false/c)]{

Returns the first snip in the editor, or @scheme[#f] if the editor is
 empty. To get all of the snips in the editor, use the @xmethod[snip%
 next] on the resulting snip.

The first snip in a text editor is the one at @techlink{position}
 0. The first snip in a pasteboard is the frontmost
 snip. (@|seesniporderdiscuss|)

}

@defmethod[(find-scroll-line [location real?])
           exact-nonnegative-integer?]{

Maps a vertical @techlink{location} within the editor to a vertical
 scroll position.

For @scheme[text%] objects: @|FCA| @|OVD|

}

@defmethod[(get-active-canvas)
           (or/c (is-a?/c editor-canvas%) false/c)]{

If the editor is displayed in a canvas, this method returns the canvas
 that most recently had the keyboard focus (while the editor was
 displayed). If no such canvas exists, @scheme[#f] is returned.

}

@defmethod[(get-admin)
           (or/c (is-a?/c editor-admin%) false/c)]{

Returns the @scheme[editor-admin%] object currently managing this
 editor or @scheme[#f] if the editor is not displayed.

}

@defmethod[(get-canvas)
           (or/c (is-a?/c editor-canvas%) false/c)]{

If @method[editor<%> get-active-canvas] returns a canvas, that canvas
 is also returned by this method. Otherwise, if @method[editor<%>
 get-canvases] returns a non-empty list, the first canvas in the list
 is returned, otherwise @scheme[#f] is returned.

}

@defmethod[(get-canvases)
           (listof (is-a?/c editor-canvas%))]{

Returns a list of canvases displaying the editor. An editor may be
 displayed in multiple canvases and no other kind of @techlink{display}, or one
 instance of another kind of @techlink{display} and no canvases. If the editor is
 not displayed or the editor's current @techlink{display} is not a canvas,
 @scheme[null] is returned.

}

@defmethod[(get-dc)
           (or/c (is-a?/c dc<%>) false/c)]{

Typically used (indirectly) by snip objects belonging to the
 editor. Returns a destination drawing context which is suitable for
 determining display sizing information, or @scheme[#f] if the editor
 is not displayed.

}

@defmethod[(get-descent)
           (and/c real? (not/c negative?))]{

Returns the font descent for the editor. This method is primarily used
 when an editor is an @techlink{item} within another editor.

@|OVD| @FCAME[]

}

@defmethod[(get-extent [w (or/c (box/c (and/c real? (not/c negative?))) false/c)]
                       [h (or/c (box/c (and/c real? (not/c negative?))) false/c)])
           void?]{

Gets the current extent of the editor's graphical representation.
@boxisfillnull[(scheme w) @elem{the editor's width}]
@boxisfillnull[(scheme h) @elem{the editor's height}]

@|OVD|  @FCAME[]

}

@defmethod[(get-file [directory (or/c path? false/c)])
           (or/c path-string? false/c)]{
@methspec{

Called when the user must be queried for a filename to load an
 editor. A starting-directory path is passed in, but is may be
 @scheme[#f] to indicate that any directory is fine.

Note that the @scheme[directory] argument cannot be a string; it must
 be a path value or @scheme[#f].

}
@methimpl{

Calls the global @scheme[get-file] procedure. 

If the editor is displayed in a single canvas, then the canvas's
 top-level frame is used as the parent for the file dialog. Otherwise,
 the file dialog will have no parent.

}}

@defmethod[(get-filename [temp (box/c (or/c any/c false/c)) #f])
           (or/c path-string? false/c)]{

Returns the path name of the last file saved from or loaded into this
 editor, @scheme[#f] if the editor has no filename.

@boxisfill[(scheme temp) @elem{@scheme[#t] if the filename is temporary or @scheme[#f]
otherwise}]

}

@defmethod[(get-flattened-text)
           string?]{

Returns the contents of the editor in text form. See @|textdiscuss| for
a discussion of flattened vs. non-flattened text.

}


@defmethod[(get-focus-snip)
           (or/c (is-a?/c snip%) false/c)]{

@index['("keyboard focus" "snips")]{Returns} the snip within the
 editor that gets the keyboard focus when the editor has the focus, or
 @scheme[#f] if the editor does not delegate the focus.

The returned snip might be an @scheme[editor-snip%] object. In that
 case, the embedded editor might delegate the focus to one of its own
 snips. However, the @method[editor<%> get-focus-snip] method returns
 only the @scheme[editor-snip%] object, because it is the focus-owning
 snip within the immediate editor.

See also @method[editor<%> set-caret-owner].

}


@defmethod[(get-inactive-caret-threshold)
           (one-of/c 'no-caret 'show-inactive-caret 'show-caret)]{

Returns the threshold for painting an inactive selection. This
 threshold is compared with the @scheme[draw-caret] argument to
 @method[editor<%> refresh] and if the argument is as least as large
 as the threshold (but larger than @indexed-scheme['show-caret]), the
 selection is drawn as inactive.

See also @method[editor<%> set-inactive-caret-threshold] and
 @|drawcaretdiscuss|.

}


@defmethod[(get-keymap)
           (or/c (is-a?/c keymap%) false/c)]{

Returns the main keymap currently used by the editor.

}


@defmethod[(get-load-overwrites-styles)
           boolean?]{

Reports whether named styles in the current style list are replaced by
 @method[editor<%> load-file] when the loaded file contains style
 specifications.

See also  @method[editor<%> set-load-overwrites-styles].

}

@defmethod[(get-max-height)
           (or/c (and/c real? (not/c negative?)) (one/of 'none))]{

Gets the maximum display height for the contents of the editor; zero or
 @scheme['none] indicates that there is no maximum.

}

@defmethod[(get-max-undo-history)
           (integer-in 0 100000)]{

Returns the maximum number of undoables that will be remembered by the
 editor. Note that undoables are counted by insertion, deletion,
 etc. events, not by the number of times that @method[editor<%> undo]
 can be called; a single @method[editor<%> undo] call often reverses
 multiple events at a time (such as when the user types a stream of
 characters at once).

}

@defmethod[(get-max-view-size)
           (values real? real?)]{

Returns the maximum visible area into which the editor is currently
 being displayed, according to the editor's administrators. If the
 editor has only one @techlink{display}, the result is the same as for
 @method[editor<%> get-view-size]. Otherwise, the maximum width and
 height of all the editor's displaying canvases is returned.

@|OVD|

If the @techlink{display} is an editor canvas, see also
 @method[area-container<%> reflow-container].

}

@defmethod[(get-max-width)
           (or/c (and/c real? (not/c negative?)) (one/of 'none))]{

Gets the maximum display width for the contents of the editor; zero or
 @scheme['none] indicates that there is no maximum. In a text editor,
 zero of @scheme['none] disables automatic line breaking.

}

@defmethod[(get-min-height)
           (or/c (and/c real? (not/c negative?)) (one/of 'none))]{

Gets the minimum display height for the contents of the editor; zero
 or @scheme['none] indicates that there is no minimum.

}


@defmethod[(get-min-width)
           (or/c (and/c real? (not/c negative?)) (one/of 'none))]{

Gets the minimum display width for the contents of the editor; zero or
 @scheme['none] indicates that there is no minimum.

}

@defmethod[(get-paste-text-only)
           boolean?]{

If the result is @scheme[#t], then the editor accepts only plain-text
 data from the clipboard. If the result is @scheme[#f], the editor
 accepts both text and snip data from the clipboard.

}

@defmethod[(get-snip-data [thesnip (is-a?/c snip%)])
           (or/c (is-a?/c editor-data%) false/c)]{

@methspec{

Gets extra data associated with a snip (e.g., @techlink{location}
 information in a pasteboard) or returns @scheme[#f] is there is no
 information. See @|editordatadiscuss| for more information.

}
@methimpl{

Returns @scheme[#f].

}}


@defmethod[(get-snip-location [thesnip (is-a?/c snip%)]
                              [x (or/c (box/c real?) false/c) #f]
                              [y (or/c (box/c real?) false/c) #f]
                              [bottom-right? any/c #f])
           boolean?]{

Gets the @techlink{location} of the given snip. If the snip is found in
 the editor, @scheme[#t] is returned; otherwise, @scheme[#f] is returned.

@boxisfillnull[(scheme x) @elem{the x-coordinate of the snip's @techlink{location}}]
@boxisfillnull[(scheme y) @elem{the y-coordinate of the snip's @techlink{location}}]

If @scheme[bottom-right?] is not @scheme[#f], the values in the
 @scheme[x] and @scheme[y] boxes are for the snip's bottom right
 corner instead of its top-left corner.

Obtaining the @techlink{location} if the bottom-right corner may
 trigger delayed size calculations (including snips other than
 the one whose @techlink{location} was requested).

@|OVD| As a special case, however, a @scheme[pasteboard%] object
 always reports valid answers when @scheme[bottom-right?] is @scheme[#f].
 @FCAME[]

}


@defmethod[(get-space)
           (and/c real? (not/c negative?))]{

Returns the maximum font space for the editor. This method is
 primarily used when an editor is an @techlink{item} within another
 editor.

@|OVD| @FCAME[]

}

@defmethod[(get-style-list)
           (is-a?/c style-list%)]{

Returns the style list currently in use by the editor.

}


@defmethod[(get-view-size [w (or/c (box/c (and/c real? (not/c negative?))) false/c)]
                          [h (or/c (box/c (and/c real? (not/c negative?))) false/c)])
           void?]{

Returns the visible area into which the editor is currently being
 displayed (according to the editor's administrator). See also
 @method[editor-admin% get-view] .

@boxisfillnull[(scheme w) @elem{the visible area width}]
@boxisfillnull[(scheme h) @elem{the visible area height}]

@|OVD|

If the @techlink{display} is an editor canvas, see also
@method[area-container<%> reflow-container].

}

@defmethod[(global-to-local [x (or/c (box/c real?) false/c)]
                            [y (or/c (box/c real?) false/c)])
           void?]{

Converts the given coordinates from top-level @techlink{display} coordinates
 (usually canvas coordinates) to editor @techlink{location} coordinates.  The
 same calculation is performed by
@method[editor<%> dc-location-to-editor-location].

@boxisfillnull[(scheme x) @elem{the translated x-coordinate of the value initially
in @scheme[x]}] 
@boxisfillnull[(scheme y) @elem{the translated x-coordinate of the value initially
in @scheme[y]}]

@|OVD|

See also @method[editor<%> local-to-global].

}

@defmethod[(in-edit-sequence?)
           boolean?]{

Returns @scheme[#t] if updating on this editor is currently delayed
 because @method[editor<%> begin-edit-sequence] has been called for
 this editor.

See also @method[editor<%> refresh-delayed?].

}


@defmethod[(insert [snip (is-a?/c snip%)])
           void?]{

Inserts data into the editor. A snip cannot be inserted into multiple
 editors or multiple times within a single editor.

@|OnInsertNote|

}


@defmethod[(insert-box [type (one-of/c 'text 'pasteboard) 'text])
           void?]{

Inserts a box (a sub-editor) into the editor by calling
@method[editor<%> on-new-box], then passing along @scheme[type] and
inserts the resulting snip into the editor.

@|OnInsertNote|

}


@defmethod*[([(insert-file [filename path-string?]
                           [format (one-of/c 'guess 'same 'copy 'standard
                                             'text 'text-force-cr) 'guess]
                           [show-errors? any/c #t])
              boolean?]
             [(insert-file [port input-port]
                           [format (one-of/c 'guess 'same 'copy 'standard
                                             'text 'text-force-cr) 'guess]
                           [show-errors? any/c #t])
              boolean?])]{

Inserts the content of a file or port into the editor (at the current
 selection @techlink{position} in @scheme[text%] editors).  The result
 is @scheme[#t]; if an error occurs, an exception is raised.

If @scheme[port] is supplied, it must support position setting with
@scheme[file-position].

For information on @scheme[format], see @method[editor<%> load-file].
The @scheme[show-errors?] argument is no longer used.

@|OnInsertNote|

}


@defmethod[(insert-image [filename (or/c path-string? false/c) #f]
                         [type (one-of/c 'unknown 'gif 'jpeg 'xbm 'xpm 'bmp 'pict) 'unknown]
                         [relative-path? any/c #f]
                         [inline? any/c #t])
           void?]{

Inserts an image into the editor. 

If @scheme[filename] is @scheme[#f], then the
user is queried for a filename. The @scheme[kind] must one of
the symbols that can be passed to 
@method[bitmap% load-file].

After the filename has been determined, an image is created by
calling
@method[editor<%> on-new-image-snip]. See also
@scheme[image-snip%].

@|OnInsertNote|

}

@defmethod[(insert-port [port input-port]
                        [format (one-of/c 'guess 'same 'copy 'standard
                                          'text 'text-force-cr) 'guess]
                        [show-errors? any/c #t])
           (one-of/c 'standard 'text 'text-force-cr)]{

Use @method[editor<%> insert-file], instead.

Inserts the content of a port into the editor (at the current
 selection @techlink{position} in @scheme[text%] editors) without wrapping
 the insert operations as an edit sequence. The result is the actual
 format of the loaded content (which is different from the given
 format type if the given format is @scheme['guess], @scheme['same], or
 @scheme['copy]).

The @scheme[port] must support position setting with @scheme[file-position].

For information on @scheme[format], see
@method[editor<%> load-file]. 

The @scheme[show-errors?] argument is no longer used.

}

@defmethod[(invalidate-bitmap-cache [x real? 0.0]
                                    [y real? 0.0]
                                    [width (or/c (and/c real? (not/c negative?)) (one/of 'end)) 'end]
                                    [height (or/c (and/c real? (not/c negative?)) (one/of 'end)) 'end])
           void?]{

When @method[editor<%> on-paint] is overridden, call this method when
 the state of @method[editor<%> on-paint]'s drawing changes.

The @scheme[x], @scheme[y], @scheme[width], and @scheme[height]
 arguments specify the area that needs repainting in editor
 coordinates. If @scheme[width]/@scheme[height] is @scheme['end], then
 the total height/width of the editor (as reported by
 @method[editor<%> get-extent]) is used. Note that the editor's size
 can be smaller than the visible region of its @techlink{display}.

The default implementation triggers a redraw of the editor, either
 immediately or at the end of the current edit sequence (if any)
 started by @method[editor<%> begin-edit-sequence].

}


@defmethod[(is-locked?)
           boolean?]{

Returns @scheme[#t] if the editor is currently locked, @scheme[#f]
 otherwise. See @method[editor<%> lock] for more information.

}


@defmethod[(is-modified?)
           boolean?]{

Returns @scheme[#t] is the editor has been modified since the last
 save or load (or the last call to @method[editor<%> set-modified]
 with @scheme[#f]), @scheme[#f] otherwise.

}


@defmethod[(kill [time (and/c exact? integer?) 0])
           void?]{

In a text editor, cuts to the end of the current line, or cuts a
 newline if there is only whitespace between the selection and end of
 line.  Multiple consecutive kills are appended.  In a pasteboard
 editor, cuts the current selection.

See @|timediscuss| for a discussion of the @scheme[time] argument. If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

See also @method[editor<%> cut].

@|OnDeleteNote|

}


@defmethod[(load-file [filename (or/c path-string? false/c) #f]
                      [format (one-of/c 'guess 'same 'copy 'standard
                                        'text 'text-force-cr) 'guess]
                      [show-errors? any/c #t])
           boolean?]{


Loads a file into the editor and returns @scheme[#t]. If an error
 occurs, an exception is raised.

If @scheme[filename] is @scheme[#f], then the
internally stored filename will be used; if @scheme[filename] is @scheme[""] or
if the internal name is unset or temporary, then the user will be
prompted for a name.  

The possible values for @scheme[format] are listed below. A single set of
@scheme[format] values are used for loading and saving files:

@itemize{

@item{@scheme['guess] --- guess the format based on
extension and/or contents; when saving a file, this is the same as
@scheme['standard]}

@item{@scheme['same] --- read in whatever format was last loaded or saved}

@item{@scheme['standard] --- read/write a standard file (binary format)}

@item{@scheme['copy] --- write using whatever format was last loaded
 or saved, but do not change the modification flag or remember
 @scheme[filename] (saving only)}

@item{@scheme['text] --- read/write a text file (@scheme[text%] only);
 file writing uses the platform's text-mode conventions
 (e.g., newlines as return--linefeed combinations under Windows) when
 not specifically disabled via @method[editor<%> use-file-text-mode]}

@item{@scheme['text-force-cr] --- read/write a text file
(@scheme[text%] only); when writing, change automatic newlines (from
word-wrapping) into real carriage returns}

}

In a @scheme[text%] instance, the format returned from @method[text%
 get-file-format] is always one of @scheme['standard], @scheme['text],
 or @scheme['text-force-cr].

The @scheme[show-errors?] argument is no longer used.

The filename used to load the file can be retrieved with
 @method[editor<%> get-filename]. For a @scheme[text%] instance, the
 format can be retrieved with @method[text% get-file-format]. However,
 if an error occurs while loading the file, the filename is set to
 @scheme[#f].

See also @method[editor<%> on-load-file], @method[editor<%>
 after-load-file], @method[editor<%> can-load-file?], and
 @method[editor<%> set-load-overwrites-styles].

}

@defmethod[(local-to-global [x (box/c real?)]
                            [y (box/c real?)])
           void?]{

Converts the given coordinates from editor @techlink{location}
 coordinates to top-level @techlink{display} coordinates (usually
 canvas coordinates).  The same calculation is performed by
 @method[editor<%> editor-location-to-dc-location].

@boxisfillnull[(scheme x) @elem{the translated x-coordinate of the value initially
in @scheme[x]}] 
@boxisfillnull[(scheme y) @elem{the translated x-coordinate of the value initially
in @scheme[y]}]

@|OVD|

See also @method[editor<%> global-to-local].

}


@defmethod[(locations-computed?)
           boolean?]{

Returns @scheme[#t] if all @techlink{location} information has been
 computed after recent changes to the editor's content or to its
 snips, @scheme[#f] otherwise.

Location information is often computed on demand, and
 @method[editor<%> begin-edit-sequence] tends to delay the
 computation.

When the editor is locked for reflowing, location information cannot
 be recomputed. See also @|lockdiscuss|.

}


@defmethod[(lock [lock? any/c])
           void?]{

Locks or unlocks the editor for modifications. If an editor is locked,
 @italic{all} modifications are blocked, not just user modifications.

See also @method[editor<%> is-locked?].

This method does not affect internal locks, as discussed in
 @|lockdiscuss|.

}

@defmethod[(locked-for-flow?)
           boolean?]{

Reports whether the editor is internally locked for flowing. See
 @|lockdiscuss| for more information.

}


@defmethod[(locked-for-read?)
           boolean?]{

Reports whether the editor is internally locked for reading. See
 @|lockdiscuss| for more information.

}


@defmethod[(locked-for-write?)
           boolean?]{

Reports whether the editor is internally locked for writing. See
 @|lockdiscuss| for more information.

}


@defmethod[(needs-update [snip (is-a?/c snip%)]
                         [localx real?]
                         [localy real?]
                         [w (and/c real? (not/c negative?))]
                         [h (and/c real? (not/c negative?))])
           void?]{

Typically called (indirectly) by a snip within the editor to force the
editor to be redrawn.

The @scheme[localx], @scheme[localy], @scheme[width], and @scheme[height]
 arguments specify the area that needs repainting in the coordinate
 system of @scheme[snip].

@FCAME[]

}


@defmethod[(num-scroll-lines)
           exact-nonnegative-integer?]{

Reports the number of scroll positions available within the editor.

For @scheme[text%] objects: @|FCA| @|EVD|

}


@defmethod[#:mode pubment 
           (on-change)
           void?]{

@methspec{

Called whenever any change is made to the editor that affects the way
 the editor is drawn or the values reported for the
 @techlink{location}/size of some snip in the editor. The
 @method[editor<%> on-change] method is called just before the editor
 calls its administrator's @method[editor-admin% needs-update] method
 to refresh the editor's @techlink{display}, and it is also called
 just before and after printing an editor.

The editor is locked for writing and reflowing during the call to
@method[editor<%> on-change].

}
@methimpl{

Does nothing.

}}


@defmethod[(on-char [event (is-a?/c key-event%)])
           void?]{
@methspec{

Handles keyboard input to the editor.

Consider overriding @method[editor<%> on-local-char] or
@method[editor<%> on-default-char] instead of this method.

}
@methimpl{

Either passes this event on to a caret-owning snip or calls
 @method[editor<%> on-local-char]. In the latter case, @scheme[text%]
 first calls @scheme[hide-cursor-until-moved].

}}

@defmethod[(on-default-char [event (is-a?/c key-event%)])
           void?]{
@methspec{

Called by @method[editor<%> on-local-char] when the event is
 @italic{not} handled by a caret-owning snip or by the keymap.

}
@methimpl{

Does nothing.

}}

@defmethod[(on-default-event [event (is-a?/c mouse-event%)])
           void?]{

@methspec{

Called by @method[editor<%> on-local-event] when the event is
 @italic{not} handled by a caret-owning snip or by the keymap.

}
@methimpl{

Does nothing. See also @xmethod[text% on-default-event] and
 @xmethod[pasteboard% on-default-event].

}}


@defmethod[#:mode pubment 
           (on-display-size)
           void?]{

@methspec{

This method is called by the editor's @techlink{display} whenever the
 display's size (as reported by @method[editor<%> get-view-size])
 changes, but it is called indirectly through @method[editor<%>
 on-display-size-when-ready].

}
@methimpl{

If automatic wrapping is enabled (see @method[editor<%> auto-wrap] )
 then @method[editor<%> set-max-width] is called with the maximum
 width of all of the editor's canvases (according to the
 administrators; @xmethod[editor-canvas% call-as-primary-owner] is
 used with each canvas to set the administrator and get the view
 size). If the editor is displayed but not in a canvas, the unique
 width is obtained from the editor's administrator (there is only
 one). If the editor is not displayed, the editor's maximum width is
 not changed.

}}


@defmethod[(on-display-size-when-ready)
           void?]{

Calls @method[editor<%> on-display-size] unless the editor is
 currently in an edit sequence or currently being refreshed. In the
 latter cases, the call to @method[editor<%> on-display-size] is
 delegated to another thread; see @secref["editorthreads"] for more
 information.

}


@defmethod[#:mode pubment 
           (on-edit-sequence)
           void?]{

@methspec{

Called just after a top-level (i.e., unnested) edit sequence starts.

During an edit sequence, all callbacks methods are invoked normally,
 but it may be appropriate for these callbacks to delay computation
 during an edit sequence. The callbacks must manage this delay
 manually. Thus, when overriding other callback methods, such as
 @xmethod[text% on-insert], @xmethod[pasteboard% on-insert],
 @xmethod[text% after-insert], or @xmethod[pasteboard% after-insert],
 consider overriding @method[editor<%> on-edit-sequence] and
 @method[editor<%> after-edit-sequence] as well.

``Top-level edit sequence'' refers to an outermost pair of
 @method[editor<%> begin-edit-sequence] and @method[editor<%>
 end-edit-sequence] calls. The embedding of an editor within another
 editor does not affect the timing of calls to @method[editor<%>
 on-edit-sequence], even if the embedding editor is in an edit
 sequence.

Pairings of @method[editor<%> on-edit-sequence] and @method[editor<%>
 after-edit-sequence] can be nested if an @method[editor<%>
 after-edit-sequence] starts a new edit sequence, since
 @method[editor<%> after-edit-sequence] is called after an edit
 sequence ends. However, @method[editor<%> on-edit-sequence] can never
 start a new top-level edit sequence (except through an unpaired
 @method[editor<%> end-edit-sequence]), because it is called after a
 top-level edit sequence starts.

}
@methimpl{

Does nothing.

}}


@defmethod[(on-event [event (is-a?/c mouse-event%)])
           void?]{

@methspec{

Handles mouse input to the editor.  The event's x and y coordinates
 are in the @techlink{display}'s co-ordinate system; use the
 administrator's @method[editor-admin% get-dc] method to obtain
 translation arguments (or use @method[editor<%>
 dc-location-to-editor-location]).

Consider overriding @method[editor<%> on-local-event] or
 @method[editor<%> on-default-event] instead of this method.

}
@methimpl{

Either passes this event on to a caret-owning snip, selects a new
 caret-owning snip (@scheme[text%] only) and passes the event on to
 the selected snip, or calls @method[editor<%> on-local-event]. A new
 caret-owning snip is selected in a @scheme[text%] object when the
 click is on an event-handling snip, and not too close to the space
 between snips (see @method[text% get-between-threshold] ).

}}


@defmethod[(on-focus [on? any/c])
           void?]{

@index['("keyboard focus" "notification")]{Called} when the keyboard
 focus changes into or out of this editor (and not to/from a snip
 within the editor) with @scheme[#t] if the focus is being turned on,
 @scheme[#f] otherwise.

}


@defmethod[#:mode pubment 
           (on-load-file [filename path?]
                         [format (one-of/c 'guess 'same 'copy 'standard
                                           'text 'text-force-cr)])
           void?]{
@methspec{

Called just before the editor is loaded from a file, after calling
 @method[editor<%> can-load-file?] to verify that the load is
 allowed. See also @method[editor<%> after-load-file].

The @scheme[filename] argument is the name the file will be loaded
 from. See @method[editor<%> load-file] for information about
 @scheme[format].

Note that the @scheme[filename] argument cannot be a string; it must
 be a path value.

}
@methimpl{

Does nothing.

}}

@defmethod[(on-local-char [event (is-a?/c key-event%)])
           void?]{
@methspec{

Called by @method[editor<%> on-char] when the event is @italic{not}
 handled by a caret-owning snip.

Consider overriding @method[editor<%> on-default-char] instead of this
 method.

}
@methimpl{

Either lets the keymap handle the event or calls @method[editor<%>
 on-default-char].

}}


@defmethod[(on-local-event [event (is-a?/c mouse-event%)])
           void?]{
@methspec{

Called by @method[editor<%> on-event] when the event is @italic{not}
 handled by a caret-owning snip.

Consider overriding @method[editor<%> on-default-event] instead of
 this method.

}
@methimpl{

Either lets the keymap handle the event or calls 
 @method[editor<%> on-default-event].

}}


@defmethod[(on-new-box [type (one-of/c 'text 'pasteboard)])
           (is-a?/c snip%)]{
@methspec{

Creates and returns a new snip for an embedded editor. This method is
 called by @method[editor<%> insert-box].

}
@methimpl{

Creates a @scheme[editor-snip%] with either a sub-editor from
 @scheme[text%] or sub-pasteboard from @scheme[pasteboard%], depending
 on whether @scheme[type] is @scheme['text] or
 @scheme['pasteboard]. The keymap (see @scheme[keymap%]) and style
 list (see @scheme[style-list%]) for of the new sub-editor are set to
 the keymap and style list of this editor.

}}


@defmethod[(on-new-image-snip [filename (or/c path? false/c)]
                              [kind (one-of/c 'unknown 'gif 'jpeg 'xbm 'xpm 'bmp 'pict)]
                              [relative-path? any/c]
                              [inline? any/c])
           (is-a?/c image-snip%)]{
@methspec{

Creates and returns a new instance of @scheme[image-snip%] for
 @method[editor<%> insert-image].

Note that the @scheme[filename] argument cannot be a string; it must be a 
 path value.

}
@methimpl{

Returns @scheme[(make-object image-snip% filename kind relative-path? inline?)].

}}


@defmethod[(on-paint [before? any/c]
                     [dc (is-a?/c dc<%>)]
                     [left real?]
                     [top real?]
                     [right real?]
                     [bottom real?]
                     [dx real?]
                     [dy real?]
                     [draw-caret (one-of/c 'no-caret 'show-inactive-caret 'show-caret)])
           void?]{
@methspec{

Provides a way to add arbitrary graphics to an editor's @techlink{display}.  This
 method is called just before and just after every painting of the
 editor.

The @scheme[before?] argument is @scheme[#t] when the method is called just
 before a painting the contents of the editor or @scheme[#f] when it is
 called after painting. The @scheme[left], @scheme[top], @scheme[right], and
 @scheme[bottom] arguments specify which region of the editor is being
 repainted, in editor coordinates. To get the coordinates for
 @scheme[dc], offset editor coordinates by adding (@scheme[dx], @scheme[dy]).
 See @|drawcaretdiscuss| for information about @scheme[draw-caret].

The @method[editor<%> on-paint] method, together with the snips'
 @method[snip% draw] methods, must be able to draw the entire state of
 an editor.  Never paint directly into an editor's @techlink{display}
 canvas except from within @method[editor<%> on-paint] or
 @method[snip% draw]. Instead, put all extra drawing code within
 @method[editor<%> on-paint] and call @method[editor<%>
 invalidate-bitmap-cache] when part of the @techlink{display} needs to
 be repainted.

If an @method[editor<%> on-paint] method uses cached
 @techlink{location} information, then the cached information should
 be recomputed in response to a call of @method[editor<%>
 invalidate-bitmap-cache].

The @method[editor<%> on-paint] method must not make any assumptions
 about the state of the drawing context (e.g., the current pen),
 except that the clipping region is already set to something
 appropriate. Before @method[editor<%> on-paint] returns, it must
 restore any drawing context settings that it changes.

The editor is internally locked for writing and reflowing during a
 call to this method (see also @|lockdiscuss|).

See also @method[editor<%> invalidate-bitmap-cache].

}
@methimpl{

Does nothing.

}}

@defmethod[#:mode pubment 
           (on-save-file [filename path?]
                         [format (one-of/c 'guess 'same 'copy 'standard
                                           'text 'text-force-cr)])
           void?]{
@methspec{

Called just before the editor is saved to a file, after calling
@method[editor<%> can-save-file?] to verify that the save is
allowed. See also @method[editor<%> after-save-file].

The @scheme[filename] argument is the name the file will be saved
to. See @method[editor<%> load-file] for information about
@scheme[format].

Note that the @scheme[filename] argument cannot be a string; it must
 be a path value.


}
@methimpl{

Does nothing.

}}


@defmethod[#:mode pubment 
           (on-snip-modified [snip (is-a?/c snip%)]
                             [modified? any/c])
           void?]{
@methspec{

This method is called whenever a snip within the editor reports that
 it has been modified (by calling its adminstrator's
 @method[snip-admin% modified] method). The method arguments are the
 snip that reported a modification-state change, and the snip's new
 modification state.

See also @method[editor<%> set-modified].

}
@methimpl{

If @scheme[modified?] is true and the editor was not already modified
 (i.e., its @method[editor<%> is-modified?]  method reports
 @scheme[#f]), then the @method[editor<%> set-modified] method is
 called with @scheme[#t]. If the editor was already modified, then the
 internal modify-counter is incremented.

If @scheme[modified?] is @scheme[#f], and if the modify-counter is
 @scheme[1], then the @method[editor<%> set-modified] method is called
 with @scheme[#f] (on the assumption that the modify-counter was set
 to @scheme[1] by an earlier call to this method for the same snip).

}}

@defmethod[(own-caret [own? any/c])
           void?]{
@methspec{

Tells the editor to display or not display the caret or selection.

@MonitorMethod[@elem{The focus state of an editor} @elem{by the system} @elem{@method[editor<%> on-focus]} @elem{focus}]

}
@methimpl{

Propagates the flag to any snip with the editor-local focus. If no
 sub-editors are active, the editor assumes the caret ownership.

}}


@defmethod[(paste [time (and/c exact? integer?) 0])
           void?]{

Pastes the current contents of the clipboard into the editor.

See @|timediscuss| for a discussion of the @scheme[time] argument. If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

The system may execute a paste (in response to other method calls)
 without calling this method. To extend or re-implement copying,
 override the @xmethod[text% do-paste] or @xmethod[pasteboard%
 do-paste] method.

See also @method[editor<%> get-paste-text-only].

}


@defmethod[(paste-x-selection [time (and/c exact? integer?) 0])
           void?]{

Like @method[editor<%> paste], but under X, uses the X selection
instead of the X clipboard.

See @|timediscuss| for a discussion of the @scheme[time] argument. If
 @scheme[time] is outside the platform-specific range of times,
 @|MismatchExn|.

To extend or re-implement copying, override the @xmethod[text%
 do-paste-x-selection] or @xmethod[pasteboard% do-paste-x-selection]
 method.

}


@defmethod[(print [interactive? any/c #t]
                  [fit-on-page? any/c #t]
                  [output-mode (one-of/c 'standard 'postscript) 'standard]
                  [parent (or/c (or/c @scheme[frame%] (is-a?/c dialog%)) false/c) #f]
                  [force-ps-page-bbox? any/c #t]
                  [as-eps? any/c #f])
           void?]{

Prints the editor. 

If @scheme[interactive?] is true and a PostScript file is created, the
 is given a dialog for adjusting printing parameters; see also
 @scheme[get-ps-setup-from-user]. Otherwise, if a PostScript file is
 created, the settings returned by @scheme[current-ps-setup] are
 used. (The user may still get a dialog to select an output file name;
 see @scheme[post-script-dc%] for more details.)

If @scheme[fit-on-page?] is a true value, then during printing for a
 @scheme[text%] editor, the editor's maximum width is set to the width
 of the page (less margins) and the autowrapping bitmap is removed.

The @scheme[output-mode] setting is used for Windows and Mac OS X. It
 determines whether the output is generated directly as a PostScript
 file (using PLT Scheme's built-in PostScript system) or generated
 using the platform-specific standard printing mechanism. The possible
 values are

@itemize{

 @item{@scheme['standard] --- print using the platform-standard
 mechanism (via a @scheme[printer-dc%]) under Windows and
 Mac OS X, PostScript for Unix (via a @scheme[post-script-dc%])}

 @item{@scheme['postscript] --- print to a PostScript file (via a
 @scheme[post-script-dc%])}

}

If @scheme[parent] is not @scheme[#f], it is used as the parent window
 for configuration dialogs (for either PostScript or platform-standard
 printing). If @scheme[parent] is @scheme[#f] and if the editor is
 displayed in a single canvas, then the canvas's top-level frame is
 used as the parent for configuration dialogs. Otherwise,
 configuration dialogs will have no parent.

The @scheme[force-ps-page-bbox?] argument is used for PostScript
 printing, and is used as the third initialization argument when
 creating the @scheme[post-script-dc%] instance. Unless it is
 @scheme[#f], the bounding-box of the resulting PostScript file is set
 to the current paper size.

The @scheme[as-eps?] argument is used for PostScript printing, and is
 used as the fourth initialization argument when creating the
 @scheme[post-script-dc%] instance. Unless it is @scheme[#f], the
 resulting PostScript file is identified as Encapsulated PostScript
 (EPS).

The printing margins are determined by @method[ps-setup%
 get-editor-margin] in the current @scheme[ps-setup%] object (as
 determined by @scheme[current-ps-setup]).

}


@defmethod[(print-to-dc [dc (is-a?/c dc<%>)])
           void?]{

Prints the editor into the given drawing context. See also
 @method[editor<%> print].

}


@defmethod[(put-file [directory (or/c path? false/c)]
                     [default-name (or/c path? false/c)])
           (or/c path-string? false/c)]{
@methspec{

Called when the user must be queried for a filename to save an
 editor. Starting-directory and default-name paths are passed in,
 but either may be @scheme[#f] to indicate that any directory is fine or
 there is no default name.

Note that the @scheme[directory] and @scheme[filename] arguments
 cannot be strings; each must be a path value.

}
@methimpl{

Calls the global @scheme[put-file] procedure.

If the editor is displayed in a single canvas, then the canvas's
 top-level frame is used as the parent for the file dialog. Otherwise,
 the file dialog will have no parent.

}}


@defmethod[(read-footer-from-file [stream (is-a?/c editor-stream-in%)]
                                  [name string?])
           boolean?]{

See @method[editor<%> read-header-from-file].

}


@defmethod[(read-from-file [stream (is-a?/c editor-stream-in%)]
                           [overwrite-styles? any/c #t])
           boolean?]{

Reads new contents for the editor from a stream. The return value is
 @scheme[#t] if there are no errors, @scheme[#f] otherwise. See also
 @|filediscuss|.

The stream provides either new mappings for names in the editor's
 style list, or it indicates that the editor should share a
 previously-read style list (depending on how style lists were shared
 when the editor was written to the stream; see also @method[editor<%>
 write-to-file]).

@itemize{

 @item{In the former case, if the @scheme[overwrite-styles?] argument
 is @scheme[#f], then each style name in the loaded file that is already
 in the current style list keeps its current style. Otherwise,
 existing named styles are overwritten with specifications from the
 loaded file.}

 @item{In the latter case, the editor's style list will be changed to
 the previously-read list.}

}
}


@defmethod[(read-header-from-file [stream (is-a?/c editor-stream-in%)]
                                  [name string?])
           boolean?]{

Called to handle a named header that is found when reading editor data
 from a stream. The return value is @scheme[#t] if there are no errors,
 @scheme[#f] otherwise.

Override this method only to embellish the file format with new header
 information. Always call the inherited method if the derived reader
 does not recognize the header.

See also @|filediscuss|.
}


@defmethod[(redo)
           void?]{

Undoes the last undo, if no other changes have been made since. See
 @method[editor<%> undo] for information about Emacs-style undo.  If
 the editor is currently performing an undo or redo, the method call
 is ignored.

The system may perform a redo without calling this method in response
 to other method calls. Use methods such as
 @method[editor<%> on-change] to monitor editor content changes.

See also @method[editor<%> add-undo].

}


@defmethod[(refresh [x real?]
                    [y real?]
                    [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))]
                    [draw-caret (one-of/c 'no-caret 'show-inactive-caret 'show-caret)]
                    [background (or/c (is-a?/c color%) false/c)])
           void?]{

Repaints a region of the editor, generally called by an editor
 administrator. The @scheme[x], @scheme[y], @scheme[width], and
 @scheme[height] arguments specify the area that needs repainting in
 editor coordinates. The @method[editor-admin% get-dc] method of the
 editor's administrator (as returned by @method[editor<%> get-admin])
 supplies the target @scheme[dc<%>] object and offset for drawing.

See @|drawcaretdiscuss| for information about @scheme[draw-caret].

The @scheme[background] color corresponds to the background of the
 @techlink{display}; if it is @scheme[#f], then the display is transparent.
 An editor should use the given background color as its own
 background (or not paint the background of @scheme[background] is
 @scheme[#f]).

See @secref["editorthreads"] for information about edit sequences and
 refresh requests.

}


@defmethod[(refresh-delayed?)
           boolean?]{

Returns @scheme[#t] if updating on this editor is currently
 delayed. Updating may be delayed because @method[editor<%>
 begin-edit-sequence] has been called for this editor, or because the
 editor has no administrator, or because the editor's administrator
 returns @scheme[#t] from its @method[editor-admin% refresh-delayed?]
 method. (The administrator might return @scheme[#t] because an
 enclosing editor's refresh is delayed.)

See also @method[editor<%> in-edit-sequence?].

}


@defmethod[(release-snip [snip (is-a?/c snip%)])
           void?]{

Requests that the specified snip be deleted and released from the
 editor. If this editor is not the snip's owner or if the snip cannot
 be released, then @scheme[#f] is returned. Otherwise, @scheme[#t] is
 returned and the snip is no longer owned.

See also @xmethod[snip-admin% release-snip] .

}


@defmethod[(remove-canvas [canvas (is-a?/c editor-canvas%)])
           void?]{

Removes a canvas from this editor's list of displaying canvases. (See
 @method[editor<%> get-canvases].)

Normally, this method is called only by @xmethod[editor-canvas%
 set-editor].

}


@defmethod[(resized [snip (is-a?/c snip%)]
                    [redraw-now? any/c])
           void?]{

Called (indirectly) by snips within the editor: it forces a
 recalculation of the display information in which the specified snip
 has changed its size.

If @scheme[redraw-now?] is @scheme[#f], the editor will require
 another message to repaint itself. (See also @method[editor<%>
 needs-update].)

}


@defmethod[(save-file [filename (or/c path-string? false/c) #f]
                      [format (one-of/c 'guess 'same 'copy 'standard
                                        'text 'text-force-cr) 'same]
                      [show-errors? any/c #t])
           boolean?]{

Saves the editor into a file and returns @scheme[#t].  If an error
 occurs, an exception is raised.

If @scheme[filename] is @scheme[#f], then the internally stored filename
 will be used; if @scheme[filename] is @scheme[""] or if the internal name
 is unset or temporary, then the user will be prompted for a name.
 The possible values for @scheme[format] are described at
@method[editor<%> load-file]. 

The filename and format used to save the file can be retrieved with
 @method[editor<%> get-filename]. In a @scheme[text%] instance, the
 format can be retrieved with @method[text% get-file-format].

See also @method[editor<%> on-save-file], @method[editor<%>
 after-save-file], and @method[editor<%> can-save-file?].

Under Mac OS X, the file's type signature is set to @scheme["TEXT"]
 for a text-format file or @scheme["WXME"] for a standard-format
 (binary) file.

The @scheme[show-errors?] argument is no longer used.

}


@defmethod[(save-port [port output-port]
                      [format (one-of/c 'guess 'same 'copy 'standard
                                        'text 'text-force-cr) 'same]
                      [show-errors? any/c #t])
           boolean?]{

Saves the editor into a port and returns @scheme[#t].  If an error
 occurs, an exception is raised.

The possible values for @scheme[format] are described at
@method[editor<%> load-file]. 

The @scheme[show-errors?] argument is no longer used.

}


@defmethod[(scroll-editor-to [localx real?]
                             [localy real?]
                             [width (and/c real? (not/c negative?))]
                             [height (and/c real? (not/c negative?))]
                             [refresh? any/c]
                             [bias (one-of/c 'start 'end 'none)])
           boolean?]{

Causes the editor to be scrolled so that a given @techlink{location}
 is visible. If the editor is scrolled, @scheme[#t] is returned,
 otherwise @scheme[#f] is returned.

This method is normally called indirectly by @method[editor<%>
 scroll-to] or @xmethod[text% scroll-to-position] to implement
 scrolling.

The default implementation forwards the request to the
@method[editor-admin% scroll-to] method of the current administrator,
if any (see @method[editor<%> get-admin]). If the editor has no
administrator, @scheme[#f] is returned.

}


@defmethod[(scroll-line-location [pos (and/c exact? integer?)])
           (and/c real? (not/c negative?))]{

Maps a vertical scroll position to a vertical @techlink{location}
 within the editor.

For @scheme[text%] objects: @|FCA| @|EVD|

}


@defmethod[(scroll-to [snip (is-a?/c snip%)]
                      [localx real?]
                      [localy real?]
                      [width (and/c real? (not/c negative?))]
                      [height (and/c real? (not/c negative?))]
                      [refresh? any/c]
                      [bias (one-of/c 'start 'end 'none) 'none])
           boolean?]{

Called (indirectly) by snips within the editor: it causes the editor
 to be scrolled so that a given @techlink{location} range within a
 given snip is visible. If the editor is scrolled immediately,
 @scheme[#t] is returned, otherwise @scheme[#f] is returned.

If refreshing is delayed (see @method[editor<%> refresh-delayed?]),
 then the scroll request is saved until the delay has ended. The
 scroll is performed (immediately or later) by calling
 @method[editor<%> scroll-editor-to].

The @scheme[localx], @scheme[localy], @scheme[width], and @scheme[height]
 arguments specify the area that needs to be visible in @scheme[snip]'s
 coordinate system.

When the specified region cannot fit in the visible area, @scheme[bias]
 indicates which end of the region to display. When @scheme[bias] is
 @scheme['start], then the top-left of the region is
 displayed. When @scheme[bias] is @scheme['end], then the
 bottom-right of the region is displayed. Otherwise, @scheme[bias] must
 be @scheme['none].

}


@defmethod[(select-all)
           void?]{

Selects all data in the editor

}

@defmethod[(set-active-canvas [canvas (is-a?/c editor-canvas%)])
           void?]{

Sets the active canvas for this editor. (See @method[editor<%>
 get-active-canvas].)

Normally, this method is called only by @xmethod[editor-canvas%
 on-focus] in an editor canvas that is displaying an editor.

}

@defmethod[(set-admin [admin (or/c (is-a?/c editor-admin%) false/c)])
           void?]{

Sets the editor's administrator. This method is only called by an
 administrator.

@Unmonitored[@elem{The administrator of an editor} @elem{by the
system} @elem{the administrator changes} @elem{@method[editor<%>
get-admin]}]

}


@defmethod[(set-caret-owner [snip (or/c (is-a?/c snip%) false/c)]
                            [domain (one-of/c 'immediate 'display 'global) 'immediate])
           void?]{

Attempts to give the keyboard focus to @scheme[snip].  If @scheme[snip] is
 @scheme[#f], then the caret is taken away from any snip in the editor
 that currently has the caret and restored to this editor.

If the keyboard focus is moved to @scheme[snip] and the editor has the
 real keyboard focus, the @method[snip% own-caret] method of the snip
 will be called.

If @scheme[#f] is provided as the new owner, then the local focus is
 moved to the editor itself. Otherwise, the local focus is moved to
 the specified snip.

The domain of focus-setting is one of:

@itemize{

 @item{@scheme['immediate] --- only set the focus owner within the
 editor}

 @item{@scheme['display] --- make this editor or the new focus
 owner get the keyboard focus among the editors in this editor's
 @techlink{display} (if this is an embedded editor)}

 @item{@scheme['global] --- make this editor or the new focus
 owner get the keyboard focus among all elements in the editor's frame}

}

@MonitorMethod[@elem{The focus state of an editor} @elem{by the
system} @elem{@method[editor<%> on-focus]} @elem{focus}]

See also @method[editor<%> get-focus-snip].

}


@defmethod[(set-cursor [cursor (or/c (is-a?/c cursor%) false/c)]
                       [override? any/c @scheme[#t]])
           void?]{

Sets the custom cursor for the editor to @scheme[cursor]. If
 @scheme[override?] is a true value and @scheme[cursor] is not
 @scheme[#f], then this cursor overrides cursor settings in embedded
 editors.

If the custom cursor is @scheme[#f], the current cursor is removed,
 and a cursor is selected automatically by the editor (depending on
 whether the cursor is pointing at a clickback). See @method[editor<%>
 adjust-cursor] for more information about the default selection.

An embedding editor's custom cursor can override the cursor of an
 embedded editor---even if the embedded editor has the caret---if
 the cursor is specified as an overriding cursor.

}


@defmethod[(set-filename [filename (or/c path-string? false/c)]
                         [temporary? any/c #f])
           void?]{

Sets the filename to @scheme[filename]. If @scheme[filename] is
 @scheme[#f] or @scheme[temporary?] is a true value, then the user
 will still be prompted for a name on future calls to
 @method[editor<%> save-file] and @method[editor<%> load-file].

This method is also called when the filename changes through any
 method (such as @method[editor<%> load-file]).

}


@defmethod[(set-inactive-caret-threshold [threshold (one-of/c 'no-caret 'show-inactive-caret 'show-caret)])
           void?]{

Sets the threshold for painting an inactive selection.  See
 @method[editor<%> get-inactive-caret-threshold] for more information.

}


@defmethod[(set-keymap [keymap (or/c (is-a?/c keymap%) false/c) #f])
           void?]{

Sets the current keymap for the editor. A @scheme[#f] argument removes
 all key mapping.

}


@defmethod[(set-load-overwrites-styles [overwrite? any/c])
           void?]{

Determines whether named styles in the current style list are replaced
 by @method[editor<%> load-file] when the loaded file contains style
 specifications.

See also @method[editor<%> get-load-overwrites-styles] and
 @method[editor<%> read-from-file].

}


@defmethod[(set-max-height [width (or/c (and/c real? (not/c negative?)) (one/of 'none))])
           void?]{

Sets the maximum display height for the contents of the editor.  A
 value less or equal to @scheme[0] indicates that there is no maximum.

Setting the height is disallowed when the editor is internally locked
 for reflowing (see also @|lockdiscuss|).

}


@defmethod[(set-max-undo-history [count (or/c exact-nonnegative-integer? (one/of 'forever))])
           void?]{

Sets the maximum number of undoables that will be remembered by the
 editor. The default is @scheme[0], which disables undo.  The symbol
 @indexed-scheme['forever] is accepted as a synonym for a very large
 number.

}


@defmethod[(set-max-width [width (or/c (and/c real? (not/c negative?)) (one/of 'none))])
           void?]{

Sets the maximum display width for the contents of the editor;
 zero or @scheme['none] indicates that there is no maximum.  In a
 text editor, having no maximum disables automatic line breaking,
 and the minimum (positive) maximum width depends on the width of the
 autowrap bitmap.

Setting the width is disallowed when the editor is internally locked
 for reflowing (see also @|lockdiscuss|).

See also @method[text% set-autowrap-bitmap].

}

@defmethod[(set-min-height [width (or/c (and/c real? (not/c negative?)) (one/of 'none))])
           void?]{

Sets the minimum display height for the contents of the editor; zero
 or @scheme['none] indicates that there is no minimum.

Setting the height is disallowed when the editor is internally locked
 for reflowing (see also @|lockdiscuss|).

}

@defmethod[(set-min-width [width (or/c (and/c real? (not/c negative?)) (one/of 'none))])
           void?]{

Sets the minimum display width for the contents of the editor; zero or
 @scheme['none] indicates that there is no minimum.

Setting the width is disallowed when the editor is internally locked
for reflowing (see also @|lockdiscuss|).

}

@defmethod[(set-modified [modified? any/c])
           void?]{

Sets the modified state of the editor. Usually, the state is changed
 automatically after an insertion, deletion, or style change by
 calling this method. (This method is also called when the
 modification state changes through @italic{any} method.) This method
 is usually not called when the state of the flag is not changing.

See also @method[editor<%> is-modified?] and @method[editor<%>
on-snip-modified].

When @scheme[modified?] is true, then an internal modify-counter is
 set to @scheme[1].

When @scheme[modified?] is @scheme[#f] and the editor's undo or redo
 stack contains a system-created undoer that resets the modified state
 (because the preceding undo or redo action puts the editor back to a
 state where the modification state was @scheme[#f]), the undoer is
 disabled.

Regardless of the value of @scheme[modified?], the editor's
 adminstrator's @method[editor-admin% modified] method is called.

Finally, if @scheme[modified?] is @scheme[#f] and the internal
 modify-counter is set to @scheme[0], then the @method[snip%
 set-unmodified] method is called on every snip within the editor.

}


@defmethod[(set-paste-text-only [text-only? any/c])
           void?]{

Sets whether the editor accepts only text from the clipboard, or both
 text and snips. By default, an editor accepts both text and
 snips.

See also @method[editor<%> get-paste-text-only].

}

@defmethod[(set-snip-data [thesnip (is-a?/c snip%)]
                          [data (is-a?/c editor-data%)])
           void?]{

Sets extra data associated with the snip (e.g., @techlink{location}
 information in a pasteboard). See @|editordatadiscuss| for more
 information.

}


@defmethod[(set-style-list [style-list (is-a?/c style-list%)])
           void?]{

Sets the editor's style list. Styles currently in use with the old
 style list will be ``moved'' to the new style list. In this ``move,''
 if a named style already exists in the new style list, then the new
 style with the same name will be used in place of the old style.

Setting the style list is disallowed when the editor is internally
 locked for reflowing (see also @|lockdiscuss|).

}


@defmethod[(style-has-changed [style (or/c (is-a?/c style<%>) false/c)])
           void?]{

Notifies the editor that a style in its style list has changed. This
 method is automatically registered with the editor's style list using
 @xmethod[style-list% notify-on-change] and automatically deregistered
 when the style list is removed from the editor.

See @xmethod[style-list% notify-on-change] for more information.

}


@defmethod[(undo)
           void?]{

Undoes the last editor change, if undos have been enabled by calling
 @method[editor<%> set-max-undo-history] with a non-zero integer.

If the editor is currently performing an undo or redo, the method call
 is ignored.

The user may enable Emacs-style undo for editors; see
 @|mrprefsdiscuss|. Normally, undo operations add to the redo stack
 (see @method[editor<%> redo]), and any undoable (non-undo) operation
 clears the redo stack. With Emacs-style undo, the redo stack is added
 back to the undo stack, along with the original undos, so that a
 complete history is kept in the undo stack.

The system may perform an undo without calling this method in response
 to other method calls. Use methods such as
 @method[editor<%> on-change] to monitor editor content changes.

See also @method[editor<%> add-undo] .

}

@defmethod*[([(use-file-text-mode) boolean?]
             [(use-file-text-mode [on? any/c]) void?])]{

Gets or sets whether the current platform's text mode is used for
writing files in @scheme['text] or @scheme['text-force-cr] mode, which
affects the way that newlines are written. The setting is consulted by
@method[editor<%> save-file] after @method[editor<%> on-save-file] is
called. See also @method[editor<%> load-file] for information on file
modes.

}


@defmethod[(write-footers-to-file [stream (is-a?/c editor-stream-out%)])
           boolean?]{

See @method[editor<%> write-headers-to-file].

}


@defmethod[(write-headers-to-file [stream (is-a?/c editor-stream-out%)])
           boolean?]{

@methspec{

Called when the editor is being saved to a file.  The return value is
 @scheme[#t] if there are no errors, @scheme[#f] otherwise. Override
 this method to add custom header data to a file, but always call the
 inherited method so that it can write its own extra headers.

To write a header item, call @method[editor<%>
 begin-write-header-footer-to-file], passing a box for an
 integer. Then write the header data and end by calling
 @method[editor<%> end-write-header-footer-to-file], passing back the
 integer that was put into the box.  Follow this procedure correctly
 or the file will be corrupted.

}
@methimpl{

Does nothing.

}}

@defmethod[(write-to-file [stream (is-a?/c editor-stream-out%)])
           boolean?]{

Writes the current editor contents to the given stream.  The return
 value is @scheme[#t] if there are no errors, @scheme[#f] otherwise. See
 also @|filediscuss|.

If the editor's style list has already been written to the stream, it
 is not re-written. Instead, the editor content indicates that the
 editor shares a previously-written style list. This sharing will be
 recreated when the stream is later read.
}}
