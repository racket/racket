#lang scribble/doc
@(require "common.ss")

@defclass/title[snip% object% ()]{

A direct instance of @scheme[snip%] is uninteresting. Useful snips are
 defined by instantiating derived subclasses, but this class defines
 the basic functionality.

In deriving a new snip class, these methods must be overridden to
create a useful snip:

@itemize[

 @item{@method[snip% get-extent]} 

 @item{@method[snip% draw]} 

 @item{@method[snip% resize] if the snip can be resized by the user}

 @item{@method[snip% partial-offset] if the snip can contain more than
       one @techlink{item}}

 @item{@method[snip% split] if the snip can contain more than one @techlink{item}}

 @item{@method[snip% size-cache-invalid] if the snip caches the result to@method[snip% get-extent]} 

 @item{@method[snip% get-text] (not required)}

 @item{@method[snip% find-scroll-step], @method[snip%
       get-num-scroll-steps], and @method[snip%
       get-scroll-step-offset] if the snip can contain more than one
       scroll position}

 @item{@method[snip% set-unmodified] if the snip's internal state can
       be modified by the user, and call @method[snip-admin% modified]
       in the snip's administrator when the state changes the first
       time}

]

If a snip can contain more than one @techlink{item}, then the snip's @techlink{count}
 must be maintained as well.

To define a class of snips that can be saved or cut-and-pasted:

@itemize[

 @item{Create an instance of @scheme[snip-class%], implementing the
       @method[snip-class% read] method. Export the
       @scheme[snip-class%] instance as @scheme[snip-class] from a
       module, and use a classname of the form @scheme["(lib ...)"] as
       described in @|snipclassdiscuss|.}

 @item{For each instance of the snip class, set the snip's class object 
       with @method[snip% set-snipclass].}

 @item{Override the @method[snip% copy] method.}

 @item{Override the @method[snip% write] method.}

]

To define a class of snips that read specially with
@scheme[open-input-text-editor]:

@itemize[

 @item{Make your @scheme[snip%] class implement @scheme[readable-snip<%>].}

 @item{Implement the @method[readable-snip<%> read-special] method.}

]



@defconstructor[()]{

Creates a plain snip of length 1 with the @scheme["Basic"] style of
 @scheme[the-style-list].

}


@defmethod[(adjust-cursor [dc (is-a?/c dc<%>)]
                          [x real?]
                          [y real?]
                          [editorx real?]
                          [editory real?]
                          [event (is-a?/c mouse-event%)])
           (or/c (is-a?/c cursor%) false/c)]{

@methspec{

Called to determine the cursor image used when the cursor is moved
 over the snip in an editor. If @scheme[#f] is returned, a default
 cursor is selected by the editor. (See @xmethod[editor<%>
 adjust-cursor] for more information.)

}
@methimpl{

Returns @scheme[#f].

}}


@defmethod[(blink-caret [dc (is-a?/c dc<%>)]
                        [x real?]
                        [y real?])
           void?]{

Tells the snip to blink the selection caret. This method is called
 periodically when the snip's editor's @techlink{display} has the
 keyboard focus, and the snip has the editor-local focus.

The drawing context and snip's @techlink{location}s in drawing context
 coordinates are provided.

}


@defmethod[#:mode pubment 
           (can-do-edit-operation? [op (one-of/c 'undo 'redo 'clear 'cut 'copy 
                                                 'paste 'kill 'select-all 
                                                 'insert-text-box 'insert-pasteboard-box 
                                                 'insert-image)]
                                   [recursive? any/c #t])
           boolean?]{

See @xmethod[editor<%> can-do-edit-operation?].

Called when the snip's editor's method is called, @scheme[recursive?]
 is not @scheme[#f], and this snip owns the caret.

}


@defmethod[(copy)
           (is-a?/c snip%)]{

Creates and returns a copy of this snip. The @method[snip% copy]
 method is responsible for copying this snip's style (as returned by
 @method[snip% get-style]) to the new snip.

}


@defmethod[(do-edit-operation [op (one-of/c 'undo 'redo 'clear 'cut 'copy 
                                            'paste 'kill 'select-all 
                                            'insert-text-box 'insert-pasteboard-box 
                                            'insert-image)]
                              [recursive? any/c #t]
                              [time exact-integer? 0])
           void?]{

See @xmethod[editor<%> do-edit-operation].

Called when the snip's editor's method is called,
 @scheme[recursive?] is not @scheme[#f], and this snip owns the caret.

}


@defmethod[(draw [dc (is-a?/c dc<%>)]
                 [x real?]
                 [y real?]
                 [left real?]
                 [top real?]
                 [right real?]
                 [bottom real?]
                 [dx real?]
                 [dy real?]
                 [draw-caret (one-of/c 'no-caret 'show-inactive-caret 'show-caret)])
           void?]{
@methspec{

Called (by an editor) to draw the snip into the given drawing context
 with the snip's top left corner at @techlink{location} (@scheme[x],
 @scheme[y]) in DC coordinates.

The arguments @scheme[left], @scheme[top], @scheme[right], and @scheme[bottom]
 define a clipping region (in DC coordinates) that the snip can use to
 optimize drawing, but it can also ignore these arguments.

The @scheme[dx] and @scheme[dy] argument provide numbers that can be
 subtracted from @scheme[x] and @scheme[y] to obtain the snip's @techlink{location} in
 editor coordinates (as opposed to DC coordinates, which are used for
 drawing).

See @|drawcaretdiscuss| for information about @scheme[draw-caret].

Before this method is called, the correct font, text color, and pen
 color for the snip's style will have been set in the drawing context
 already.  (This is @italic{not} true for @method[snip% get-extent] or
 @method[snip% partial-offset].)  The @method[snip% draw] method must
 not make any other assumptions about the state of the drawing
 context, except that the clipping region is already set to something
 appropriate. Before @method[snip% draw] returns, it must restore any
 drawing context settings that it changes.

See also @xmethod[editor<%> on-paint].

The snip's editor is usually internally locked for
 writing and reflowing when this method is called
 (see also @|lockdiscuss|).

}
@methimpl{

Draws nothing.

}}


@defmethod[(find-scroll-step [y real?])
           exact-nonnegative-integer?]{

@methspec{

If a snip contains more than one vertical scroll step (see
 @method[snip% get-num-scroll-steps]) then this method is called to
 find a scroll step offset for a given y-offset into the snip.

}
@methimpl{

Returns @scheme[0].

}}


@defmethod[(get-admin)
           (or/c (is-a?/c snip-admin%) false/c)]{

Returns the administrator for this snip. (The administrator can be
 @scheme[#f] even if the snip is owned but not visible in the editor.)

}

@defmethod[(get-count)
           (integer-in 0 100000)]{

Returns the snip's @techlink{count} (i.e., number of @techlink{item}s
 within the snip).

}

@defmethod[(get-extent [dc (is-a?/c dc<%>)]
                       [x real?]
                       [y real?]
                       [w (or/c (box/c (and/c real? (not/c negative?))) false/c) #f]
                       [h (or/c (box/c (and/c real? (not/c negative?))) false/c) #f]
                       [descent (or/c (box/c (and/c real? (not/c negative?))) false/c) #f]
                       [space (or/c (box/c (and/c real? (not/c negative?))) false/c) #f]
                       [lspace (or/c (box/c (and/c real? (not/c negative?))) false/c) #f]
                       [rspace (or/c (box/c (and/c real? (not/c negative?))) false/c) #f])
           void?]{
@methspec{

Calculates the snip's width, height, descent (amount of height which
 is drawn below the baseline), space (amount of height which is
 ``filler'' space at the top), and horizontal spaces (amount of width
 which is ``filler'' space at the left and right). Those values are
 returned by filling the @scheme[w], @scheme[h], @scheme[descent],
 @scheme[space], @scheme[lspace], and @scheme[rspace] boxes.

This method is called by the snip's administrator; it is not normally
 called directly by others. To get the extent of a snip, use
 @xmethod[editor<%> get-snip-location] .

A drawing context is provided for the purpose of finding font sizes,
 but no drawing should occur. The @method[snip% get-extent] and
 @method[snip% partial-offset] methods must not make any assumptions
 about the state of the drawing context, except that it is scaled
 properly. In particular, the font for the snip's style is not
 automatically set in the drawing context before the method is
 called. (Many snips cache their size information, so
 automatically setting the font would be wasteful.)  If @method[snip%
 get-extent] or @method[snip% partial-offset] changes the drawing
 context's setting, it must restore them before returning. However,
 the methods should not need to change the drawing context; only font
 settings can affect measurement results from a device context, and
 @xmethod[dc<%> get-text-extent] accepts a @scheme[font%] argument for
 sizing that overrides that device context's current font.

The snip's left and top @techlink{location}s are provided as @scheme[x]
 and @scheme[y] in editor coordinates, in case the snip's size depends
 on its location; the @scheme[x] and @scheme[y] arguments are usually
 ignored. In a text editor, the @scheme[y]-coordinate is the @italic{line's}
 top @techlink{location}; the snip's actual top @techlink{location} is
 potentially undetermined until its height is known.

If a snip caches the result size for future replies, it should
 invalidate its cached size when @method[snip% size-cache-invalid] is
 called (especially if the snip's size depends on any device context
 properties).

If a snip's size changes after receiving a call to
@method[snip% get-extent] and before receiving a call to
@method[snip% size-cache-invalid], then the snip must notify its administrator of the size change, so
 that the administrator can recompute its derived size information.
 Notify the administrator of a size change by call its
@method[snip-admin% resized] method.

The snip's editor is usually internally locked for writing and
 reflowing when this method is called (see also @|lockdiscuss|).

}
@methimpl{

Fills in all boxes with @scheme[0.0].

}}


@defmethod[(get-flags)
           (listof symbol?)]{

Returns flags defining the behavior of the snip, a list of the
following symbols:

@itemize[

 @item{@indexed-scheme['is-text] --- this is a text snip derived from
       @scheme[string-snip%]; do not set this flag}

 @item{@indexed-scheme['can-append] --- this snip can be merged with
       another snip of the same type}

 @item{@indexed-scheme['invisible] --- the user doesn't ``see'' this snip; 
       e.g.: a carriage return}

 @item{@indexed-scheme['hard-newline] --- a newline must follow the snip}

 @item{@indexed-scheme['newline] --- a newline currently follows the
       snip; only an owning editor should set this flag}

 @item{@indexed-scheme['handles-events] --- this snip can handle
       keyboard and mouse events}

 @item{@indexed-scheme['width-depends-on-x] --- this snip's display
       width depends on the snip's x-@techlink{location} within the
       editor; e.g.: tab}

 @item{@indexed-scheme['height-depends-on-y] --- this snip's display
       height depends on the snip's y-@techlink{location} within the editor}

 @item{@indexed-scheme['width-depends-on-y] --- this snip's display
       width depends on the snip's y-@techlink{location} within the editor}

 @item{@indexed-scheme['height-depends-on-x] --- this snip's display
       height depends on the snip's x-@techlink{location} within the editor}

 @item{@indexed-scheme['uses-editor-path] --- this snip uses its
       editor's pathname and should be notified when the name changes;
       notification is given as a redundant call to @method[snip%
       set-admin]}

]}


@defmethod[(get-num-scroll-steps)
           exact-nonnegative-integer?]{

@methspec{

Returns the number of horizontal scroll steps within the snip.  For
 most snips, this is @scheme[1]. Embedded editor snips use this method so that
 scrolling in the owning editor will step through the lines in the
 embedded editor.

}
@methimpl{

Returns @scheme[1].

}}


@defmethod[(get-scroll-step-offset [offset exact-nonnegative-integer?])
           (and/c real? (not/c negative?))]{

@methspec{
If a snip contains more than one vertical scroll step (see
@method[snip% get-num-scroll-steps]) then this method is called to
find the y-offset into the snip for a given scroll offset.

}
@methimpl{

Returns @scheme[0.0].

}}


@defmethod[(get-snipclass)
           (is-a?/c snip-class%)]{

Returns the snip's class, which is used for file saving and
 cut-and-paste.

Since this method returns the snip class stored by @method[snip%
 set-snipclass], it is not meant to be overridden.

}


@defmethod[(get-style)
           (is-a?/c style<%>)]{

Returns the snip's style. See also @method[snip% set-style].

}


@defmethod[(get-text [offset exact-nonnegative-integer?]
                     [num exact-nonnegative-integer?]
                     [flattened? any/c #f])
           string?]{
@methspec{

Returns the text for this snip starting with the @techlink{position}
 @scheme[offset] within the snip, and continuing for a total length of
 @scheme[num] @techlink{item}s. If @scheme[offset] is greater than the snip's
 @techlink{count}, then @scheme[""] is returned. If @scheme[num] is greater than the
 snip's @techlink{count} minus the offset, then text from the offset to the end
 of the snip is returned.

If @scheme[flattened?] is not @scheme[#f], then flattened text is returned.
 See @|textdiscuss| for a discussion of flattened vs. non-flattened
 text.

}
@methimpl{

Returns @scheme[""].

}}


@defmethod[(get-text! [buffer (and/c string? (not/c immutable?))]
                      [offset exact-nonnegative-integer?]
                      [num exact-nonnegative-integer?]
                      [buffer-offset exact-nonnegative-integer?])
           void?]{
@methspec{

Like @method[snip% get-text] in non-flattened mode, except that the
 characters are put into the given mutable string, instead of returned
 in a newly allocated string.

The @scheme[buffer] string is filled starting at position
 @scheme[buffer-offset]. The @scheme[buffer] string must be at least
 @math{@scheme[num]+@scheme[buffer-offset]} characters long.

}
@methimpl{

Calls @method[snip% get-text], except in the case of a
 @scheme[string-snip%], in which case @scheme[buffer] is filled
 directly.

}}


@defmethod[(is-owned?)
           boolean?]{

Returns @scheme[#t] if this snip has an owner, @scheme[#f] otherwise.
 Note that a snip may be owned by an editor if it was inserted and
 then deleted from the editor, if it's still in the editor's undo
 history.

}


@defmethod[(match? [snip (is-a?/c snip%)])
           boolean?]{

@methspec{

Return @scheme[#t] if @this-obj[] ``matches'' @scheme[snip],
 @scheme[#f] otherwise.

}
@methimpl{

Returns @scheme[#t] if the @scheme[snip] and @this-obj[] are from the
 same class and have the same length.

}}


@defmethod[(merge-with [prev (is-a?/c snip%)])
           (or/c (is-a?/c snip%) false/c)]{

@methspec{

Merges @this-obj[] with @scheme[prev], returning @scheme[#f] if the
 snips cannot be merged or a new merged snip otherwise. This method
 will only be called if both snips are from the same class and both
 have the @indexed-scheme['can-append] flag.

If the returned snip does not have the expected @techlink{count}, its
 @techlink{count} is forcibly modified. If the returned snip is
 already owned by another administrator, a surrogate snip is created.

The snip's editor is usually internally locked for reading when this
 method is called (see also @|lockdiscuss|).

}
@methimpl{

Returns @scheme[#f].

}}


@defmethod[(next)
           (or/c (is-a?/c snip%) false/c)]{

Returns the next snip in the editor owning this snip, or @scheme[#f]
 if this is the last snip.

In a text editor, the next snip is the snip at the @techlink{position}
 following this snip's (last) @techlink{position}. In a pasteboard,
 the next snip is the one immediately behind this
 snip. (@|seesniporderdiscuss|)

}


@defmethod[(on-char [dc (is-a?/c dc<%>)]
                    [x real?]
                    [y real?]
                    [editorx real?]
                    [editory real?]
                    [event (is-a?/c key-event%)])
           void?]{
@methspec{

Called to handle keyboard events when this snip has the keyboard focus
 and can handle events. The drawing context is provided, as well as
 the snip's @techlink{location} in @techlink{display} coordinates (the
 event uses @techlink{display} coordinates), and the snip's
 @techlink{location} in editor coordinates.

The @scheme[x] and @scheme[y] arguments are the snip's
 @techlink{location} in @techlink{display} coordinates. The
 @scheme[editorx] and @scheme[editory] arguments are the snip's
 @techlink{location} in editor coordinates.  To get @scheme[event]'s x
 @techlink{location} in snip coordinates, subtract @scheme[x] from
 @scheme[(send event get-x)].

See also @indexed-scheme['handles-events] in @method[snip% get-flags].

}
@methimpl{

Does nothing.

}}


@defmethod[(on-event [dc (is-a?/c dc<%>)]
                     [x real?]
                     [y real?]
                     [editorx real?]
                     [editory real?]
                     [event (is-a?/c mouse-event%)])
           void?]{
@methspec{

Called to handle mouse events on the snip when this snip can handle
 events and when the snip has the keyboard focus. See @method[snip%
 on-char] for information about the arguments. 

The @scheme[x] and @scheme[y] arguments are the snip's
 @techlink{location} in @techlink{display} coordinates. The
 @scheme[editorx] and @scheme[editory] arguments are the snip's
 @techlink{location} in editor coordinates.  To get @scheme[event]'s x
 @techlink{location} in snip coordinates, subtract @scheme[x] from
 @scheme[(send event get-x)].

See also @indexed-scheme['handles-events] in @method[snip% get-flags].

}
@methimpl{

Does nothing.

}}


@defmethod[(own-caret [own-it? any/c])
           void?]{
@methspec{

Notifies the snip that it is or is not allowed to display the caret
 (indicating ownership of keyboard focus) in some
 @techlink{display}. This method is @italic{not} called to request
 that the caret is actually shown or hidden; the @method[snip% draw]
 method is called for all display requests.

The @scheme[own-it?] argument is @scheme[#t] if the snip owns the
 keyboard focus or @scheme[#f] otherwise.

}
@methimpl{

Does nothing.

}}


@defmethod[(partial-offset [dc (is-a?/c dc<%>)]
                           [x real?]
                           [y real?]
                           [len exact-nonnegative-integer?])
           real?]{
@methspec{

Calculates a partial width for the snip, starting from the first snip
 @techlink{item} and continuing for @scheme[len] @techlink{item}s. The
 drawing context and snip's @techlink{location}s in editor coordinates
 are provided. See also @method[snip% get-extent].

The snip's editor is usually internally locked for writing and
 reflowing when this method is called (see also @|lockdiscuss|).

}
@methimpl{

Returns @scheme[0.0].

}}


@defmethod[(previous)
           (or/c (is-a?/c snip%) false/c)]{

Returns the previous snip in the editor owning this snip, or @scheme[#f] if this
 is the first snip.

}


@defmethod[(release-from-owner)
           boolean?]{
@methspec{

Asks the snip to try to release itself from its owner. If the snip is
 not owned or the release is successful, then @scheme[#t] is
 returned. Otherwise, @scheme[#f] is returned and the snip remains
 owned.  See also @method[snip% is-owned?].

Use this method for moving a snip from one editor to another. This
 method notifies the snip's owning editor that someone else really
 wants control of the snip. It is not necessary to use this method for
 "cleaning up" a snip when it is deleted from an editor.

}
@methimpl{

Requests a low-level release from the snip's owning administrator.

}}


@defmethod[(resize [w (and/c real? (not/c negative?))]
                   [h (and/c real? (not/c negative?))])
           boolean?]{
@methspec{

Resizes the snip. The snip can refuse to be resized by returning
 @scheme[#f]. Otherwise, the snip will resize (it must call its
 administrator's @method[snip-admin% resized] method) and return
 @scheme[#t].

See also @xmethod[pasteboard% on-interactive-resize].

}
@methimpl{

Returns @scheme[#f].

}}


@defmethod[(set-admin [admin (or/c (is-a?/c snip-admin%) false/c)])
           void?]{

Sets the snip's administrator. Only an administrator should call this
 method.

The default method sets the internal state of a snip to record its
 administrator. It will not modify this state if the snip is already
 owned by an administrator and the administrator has not blessed the
 transition. If the administrator state of a snip is not modified as
 expected during a sensitive call to this method by an instance of
 @scheme[text%] or @scheme[pasteboard%], the
 internal state may be forcibly modified (if the new administrator was
 @scheme[#f]) or a surrogate snip may be created (if the snip was
 expected to receive a new administrator).

The snip's (new) editor is usually internally locked for reading when
 this method is called (see also @|lockdiscuss|).

}


@defmethod[(set-count [c (integer-in 1 100000)])
           void?]{
@methspec{

Sets the snip's @techlink{count} (i.e., the number of @techlink{item}s
 within the snip).

The snip's @techlink{count} may be changed by the system (in extreme cases to
 maintain consistency) without calling this method.

}
@methimpl{

Sets the snip's @techlink{count} and notifies the snip's administrator
 that the snip's size has changed.

}}


@defmethod[(set-flags [flags (listof symbol?)])
           void?]{
@methspec{

Sets the snip's flags. See @method[snip% get-flags].

}
@methimpl{

Sets the snip flags and notifies the snip's editor that its flags have
changed. 

}}


@defmethod[(set-snipclass [class (is-a?/c snip-class%)])
           void?]{

Sets the snip's class, used for file saving and cut-and-paste.

This method stores the snip class internally; other editor objects may
 access the snip class directly, instead of through the @method[snip%
 get-snipclass] method.

}


@defmethod[(set-style [style (is-a?/c style<%>)])
           void?]{

Sets the snip's style if it is not owned by any editor.  See also
 @method[snip% get-style] and @method[snip% is-owned?].

The snip's style may be changed by the system without calling this method.

}


@defmethod[(set-unmodified)
           void?]{
@methspec{

Called by the snip's administrator to notify the snip that its changed
 have been saved. The next time snip's internal state is modified by
 the user, it should call @method[snip-admin% modified] to report the
 state change (but only on the first change after this method is
 called, or the first change after the snip acquires a new
 administrator).

}
@methimpl{

Does nothing.

}}


@defmethod[(size-cache-invalid)
           void?]{

@methspec{

Called to notify the snip that it may need to recalculate its display
 arguments (width, height, etc.) when it is next asked, because the
 style or @techlink{location} of the snip has changed.

The snip's (new) editor is usually internally locked for reflowing
 when this method is called (see also @|lockdiscuss|).

}

@methimpl{

Does nothing.

}}


@defmethod[(split [position exact-nonnegative-integer?]
                  [first (box/c (is-a?/c snip%))]
                  [second (box/c (is-a?/c snip%))])
           void?]{

@methspec{
Splits the snip into two snips. This is called when a snip has more
 than one @techlink{item} and something is inserted between two
 @techlink{item}s.

The arguments are a relative @techlink{position} integer and two
 boxes. The @techlink{position} integer specifies how many
 @techlink{item}s should be given to the new first snip; the rest go
 to the new second snip. The two boxes must be filled with two new
 snips. (The old snip is no longer used, so it can be recycled as a
 new snip.)

If the returned snips do not have the expected @techlink{count}s, their
 @techlink{count}s are forcibly modified. If either returned snip is already
 owned by another administrator, a surrogate snip is created.

The snip's editor is usually internally locked for reading when this
 method is called (see also @|lockdiscuss|).
}

@methimpl{

Creates a new @scheme[snip%] instance with @scheme[position]
elements, and modifies @this-obj[] to decrement its count by
@scheme[position]. The nest snip is installed into @scheme[first] and
@this-obj[] is installed into @scheme[second].

}}


@defmethod[(write [f (is-a?/c editor-stream-out%)])
           void?]{

Writes the snip to the given stream. (Snip reading is handled by the
 snip class.) Style information about the snip (i.e., the content of
 @method[snip% get-style]) will be saved and restored automatically.

}}
