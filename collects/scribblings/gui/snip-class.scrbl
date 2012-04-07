#lang scribble/doc
@(require "common.rkt")

@defclass/title[snip% object% (equal<%>)]{

A direct instance of @racket[snip%] is uninteresting. Useful snips are
 defined by instantiating derived subclasses, but this class defines
 the basic functionality.

In deriving a new snip class, these methods must be overridden to
create a useful snip:

@itemize[

 @item{@method[snip% get-extent]} 

 @item{@method[snip% draw]} 

 @item{@method[snip% copy]} 

 @item{@method[snip% resize] if the snip can be resized by the user}

 @item{@method[snip% partial-offset] if the snip can contain more than
       one @techlink{item}}

 @item{@method[snip% split] if the snip can contain more than one @techlink{item}}

 @item{@method[snip% size-cache-invalid] if the snip caches the result to @method[snip% get-extent]} 

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

 @item{Create an instance of @racket[snip-class%], implementing the
       @method[snip-class% read] method. Export the
       @racket[snip-class%] instance as @racket[snip-class] from a
       module, and use a classname of the form @racket["(lib ...)"] as
       described in @|snipclassdiscuss|.}

 @item{For each instance of the snip class, set the snip's class object 
       with @method[snip% set-snipclass].}

 @item{Override the @method[snip% copy] method.}

 @item{Override the @method[snip% write] method.}

]

To define a class of snips that read specially with
@racket[open-input-text-editor]:

@itemize[

 @item{Make your @racket[snip%] class implement @racket[readable-snip<%>].}

 @item{Implement the @method[readable-snip<%> read-special] method.}

]



@defconstructor[()]{

Creates a plain snip of length 1 with the @racket["Basic"] style of
 @racket[the-style-list].

}


@defmethod[(adjust-cursor [dc (is-a?/c dc<%>)]
                          [x real?]
                          [y real?]
                          [editorx real?]
                          [editory real?]
                          [event (is-a?/c mouse-event%)])
           (or/c (is-a?/c cursor%) #f)]{

@methspec{

Called to determine the cursor image used when the cursor is moved
 over the snip in an editor. If @racket[#f] is returned, a default
 cursor is selected by the editor. (See @xmethod[editor<%>
 adjust-cursor] for more information.)

}
@methimpl{

Returns @racket[#f].

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


@defmethod[(can-do-edit-operation? [op (or/c 'undo 'redo 'clear 'cut 'copy 
                                             'paste 'kill 'select-all 
                                             'insert-text-box 'insert-pasteboard-box 
                                             'insert-image)]
                                   [recursive? any/c #t])
           boolean?]{

See @xmethod[editor<%> can-do-edit-operation?].

Called when the snip's editor's method is called, @racket[recursive?]
 is not @racket[#f], and this snip owns the caret.

}


@defmethod[(copy)
           (is-a?/c snip%)]{

Creates and returns a copy of this snip. The @method[snip% copy]
 method is responsible for copying this snip's style (as returned by
 @method[snip% get-style]) to the new snip.

}


@defmethod[(do-edit-operation [op (or/c 'undo 'redo 'clear 'cut 'copy 
                                        'paste 'kill 'select-all 
                                        'insert-text-box 'insert-pasteboard-box 
                                        'insert-image)]
                              [recursive? any/c #t]
                              [time exact-integer? 0])
           void?]{

See @xmethod[editor<%> do-edit-operation].

Called when the snip's editor's method is called,
 @racket[recursive?] is not @racket[#f], and this snip owns the caret.

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
                 [draw-caret (or/c 'no-caret 'show-inactive-caret 'show-caret
                                   (cons/c exact-nonnegative-integer?
                                           exact-nonnegative-integer?))])
           void?]{
@methspec{

Called (by an editor) to draw the snip into the given drawing context
 with the snip's top left corner at @techlink{location} (@racket[x],
 @racket[y]) in DC coordinates.

The arguments @racket[left], @racket[top], @racket[right], and @racket[bottom]
 define a clipping region (in DC coordinates) that the snip can use to
 optimize drawing, but it can also ignore these arguments.

The @racket[dx] and @racket[dy] argument provide numbers that can be
 subtracted from @racket[x] and @racket[y] to obtain the snip's @techlink{location} in
 editor coordinates (as opposed to DC coordinates, which are used for
 drawing).

See @|drawcaretdiscuss| for information about
@racket[draw-caret]. When @racket[draw-caret] is a pair, refrain from
drawing a background for the selected region, and if
@racket[(get-highlight-text-color)] returns a color (instead of @racket[#f]),
use that color for drawing selected text and other selected foreground elements.

Before this method is called, the font, text color, and pen color for
 the snip's style will have been set in the drawing context.  (The
 drawing context is @italic{not} so configured for @method[snip%
 get-extent] or @method[snip% partial-offset].)  The @method[snip% draw] method must
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

@defmethod[(equal-to? [snip (is-a?/c snip%)]
                      [equal? (-> any/c any/c boolean?)])
           boolean?]{
@methspec{See @racket[equal<%>].}

@methimpl{Calls the @method[snip% other-equal-to?] method of @racket[snip]
(to simulate multi-method dispatch) in case @racket[snip] provides a
more specific equivalence comparison.}}

@defmethod[(other-equal-to? [that (is-a?/c snip%)]
                            [equal? (-> any/c any/c boolean?)])
           boolean?]{
@methimpl{Returns @racket[(eq? @#,(this-obj) that)].}
}

@defmethod[(equal-hash-code-of [hash-code (any/c . -> . exact-integer?)])
           exact-integer?]{

@methspec{See @racket[equal<%>].}
 
@methimpl{Returns @racket[(eq-hash-code @#,(this-obj))].}}

@defmethod[(equal-secondary-hash-code-of [hash-code (any/c . -> . exact-integer?)])
           exact-integer?]{

@methspec{See @racket[equal<%>].}
 
@methimpl{Returns @racket[1].}}

@defmethod[(find-scroll-step [y real?])
           exact-nonnegative-integer?]{

@methspec{

If a snip contains more than one vertical scroll step (see
 @method[snip% get-num-scroll-steps]) then this method is called to
 find a scroll step offset for a given y-offset into the snip.

}
@methimpl{

Returns @racket[0].

}}

@defmethod[(get-admin)
           (or/c (is-a?/c snip-admin%) #f)]{

Returns the administrator for this snip. (The administrator can be
 @racket[#f] even if the snip is owned but not visible in the editor.)

}

@defmethod[(get-count)
           (integer-in 0 100000)]{

Returns the snip's @techlink{count} (i.e., number of @techlink{item}s
 within the snip).

}

@defmethod[(get-extent [dc (is-a?/c dc<%>)]
                       [x real?]
                       [y real?]
                       [w (or/c (box/c (and/c real? (not/c negative?))) #f) #f]
                       [h (or/c (box/c (and/c real? (not/c negative?))) #f) #f]
                       [descent (or/c (box/c (and/c real? (not/c negative?))) #f) #f]
                       [space (or/c (box/c (and/c real? (not/c negative?))) #f) #f]
                       [lspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f]
                       [rspace (or/c (box/c (and/c real? (not/c negative?))) #f) #f])
           void?]{
@methspec{

Calculates the snip's width, height, descent (amount of height which
 is drawn below the baseline), space (amount of height which is
 ``filler'' space at the top), and horizontal spaces (amount of width
 which is ``filler'' space at the left and right). Those values are
 returned by filling the @racket[w], @racket[h], @racket[descent],
 @racket[space], @racket[lspace], and @racket[rspace] boxes.

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
 @xmethod[dc<%> get-text-extent] accepts a @racket[font%] argument for
 sizing that overrides that device context's current font.

The snip's left and top @techlink{location}s are provided as @racket[x]
 and @racket[y] in editor coordinates, in case the snip's size depends
 on its location; the @racket[x] and @racket[y] arguments are usually
 ignored. In a text editor, the @racket[y]-coordinate is the @italic{line's}
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

Fills in all boxes with @racket[0.0].

}}


@defmethod[(get-flags)
           (listof symbol?)]{

Returns flags defining the behavior of the snip, a list of the
following symbols:

@itemize[

 @item{@indexed-racket['is-text] --- this is a text snip derived from
       @racket[string-snip%]; do not set this flag}

 @item{@indexed-racket['can-append] --- this snip can be merged with
       another snip of the same type}

 @item{@indexed-racket['invisible] --- an @deftech{invisible} snip
       that the user doesn't see, such as a newline}

 @item{@indexed-racket['hard-newline] --- a newline must follow the snip}

 @item{@indexed-racket['newline] --- a newline currently follows the
       snip; only an owning editor should set this flag}

 @item{@indexed-racket['handles-events] --- this snip can handle
       keyboard and mouse events when it has the keyboard focus}

 @item{@indexed-racket['handles-all-mouse-events] --- this snip can
       handle mouse events that touch the snip or that immediately
       follow an event that touches the snip, even if the snip does
       not have the keyboard focus}

 @item{@indexed-racket['width-depends-on-x] --- this snip's display
       width depends on the snip's x-@techlink{location} within the
       editor; e.g.: tab}

 @item{@indexed-racket['height-depends-on-y] --- this snip's display
       height depends on the snip's y-@techlink{location} within the editor}

 @item{@indexed-racket['width-depends-on-y] --- this snip's display
       width depends on the snip's y-@techlink{location} within the editor}

 @item{@indexed-racket['height-depends-on-x] --- this snip's display
       height depends on the snip's x-@techlink{location} within the editor}

 @item{@indexed-racket['uses-editor-path] --- this snip uses its
       editor's pathname and should be notified when the name changes;
       notification is given as a redundant call to @method[snip%
       set-admin]}

]}


@defmethod[(get-num-scroll-steps)
           exact-nonnegative-integer?]{

@methspec{

Returns the number of horizontal scroll steps within the snip.  For
 most snips, this is @racket[1]. Embedded editor snips use this method so that
 scrolling in the owning editor will step through the lines in the
 embedded editor.

}
@methimpl{

Returns @racket[1].

}}


@defmethod[(get-scroll-step-offset [offset exact-nonnegative-integer?])
           (and/c real? (not/c negative?))]{

@methspec{
If a snip contains more than one vertical scroll step (see
@method[snip% get-num-scroll-steps]) then this method is called to
find the y-offset into the snip for a given scroll offset.

}
@methimpl{

Returns @racket[0.0].

}}


@defmethod[(get-snipclass)
           (or/c #f (is-a?/c snip-class%))]{

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
 @racket[offset] within the snip, and continuing for a total length of
 @racket[num] @techlink{item}s. If @racket[offset] is greater than the snip's
 @techlink{count}, then @racket[""] is returned. If @racket[num] is greater than the
 snip's @techlink{count} minus the offset, then text from the offset to the end
 of the snip is returned.

If @racket[flattened?] is not @racket[#f], then flattened text is returned.
 See @|textdiscuss| for a discussion of flattened vs. non-flattened
 text.

}
@methimpl{

Returns @racket[""].

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

The @racket[buffer] string is filled starting at position
 @racket[buffer-offset]. The @racket[buffer] string must be at least
 @math{@racket[num]+@racket[buffer-offset]} characters long.

}
@methimpl{

Calls @method[snip% get-text], except in the case of a
 @racket[string-snip%], in which case @racket[buffer] is filled
 directly.

}}


@defmethod[(is-owned?)
           boolean?]{

Returns @racket[#t] if this snip has an owner, @racket[#f] otherwise.
 Note that a snip may be owned by an editor if it was inserted and
 then deleted from the editor, if it's still in the editor's undo
 history.

}


@defmethod[(match? [snip (is-a?/c snip%)])
           boolean?]{

@methspec{

Return @racket[#t] if @this-obj[] ``matches'' @racket[snip],
 @racket[#f] otherwise.

}
@methimpl{

Returns @racket[#t] if the @racket[snip] and @this-obj[] are from the
 same class and have the same length.

}}


@defmethod[(merge-with [prev (is-a?/c snip%)])
           (or/c (is-a?/c snip%) #f)]{

@methspec{

Merges @this-obj[] with @racket[prev], returning @racket[#f] if the
 snips cannot be merged or a new merged snip otherwise. This method
 will only be called if both snips are from the same class and both
 have the @indexed-racket['can-append] flag.

If the returned snip does not have the expected @techlink{count}, its
 @techlink{count} is forcibly modified. If the returned snip is
 already owned by another administrator, a surrogate snip is created.

The snip's editor is usually internally locked for reading when this
 method is called (see also @|lockdiscuss|).

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(next)
           (or/c (is-a?/c snip%) #f)]{

Returns the next snip in the editor owning this snip, or @racket[#f]
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

The @racket[x] and @racket[y] arguments are the snip's
 @techlink{location} in @techlink{display} coordinates. The
 @racket[editorx] and @racket[editory] arguments are the snip's
 @techlink{location} in editor coordinates.  To get @racket[event]'s x
 @techlink{location} in snip coordinates, subtract @racket[x] from
 @racket[(send event get-x)].

See also @indexed-racket['handles-events] in @method[snip% get-flags].

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

The @racket[x] and @racket[y] arguments are the snip's
 @techlink{location} in @techlink{display} coordinates. The
 @racket[editorx] and @racket[editory] arguments are the snip's
 @techlink{location} in editor coordinates.  To get @racket[event]'s x
 @techlink{location} in snip coordinates, subtract @racket[x] from
 @racket[(send event get-x)].

See also @indexed-racket['handles-events] in @method[snip% get-flags].

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

The @racket[own-it?] argument is @racket[#t] if the snip owns the
 keyboard focus or @racket[#f] otherwise.

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
 @techlink{item} and continuing for @racket[len] @techlink{item}s. The
 drawing context and snip's @techlink{location}s in editor coordinates
 are provided. See also @method[snip% get-extent].

The snip's editor is usually internally locked for writing and
 reflowing when this method is called (see also @|lockdiscuss|).

}
@methimpl{

Returns @racket[0.0].

}}


@defmethod[(previous)
           (or/c (is-a?/c snip%) #f)]{

Returns the previous snip in the editor owning this snip, or @racket[#f] if this
 is the first snip.

}


@defmethod[(release-from-owner)
           boolean?]{
@methspec{

Asks the snip to try to release itself from its owner. If the snip is
 not owned or the release is successful, then @racket[#t] is
 returned. Otherwise, @racket[#f] is returned and the snip remains
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
 @racket[#f]. Otherwise, the snip will resize (it must call its
 administrator's @method[snip-admin% resized] method) and return
 @racket[#t].

See also @xmethod[pasteboard% on-interactive-resize].

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(set-admin [admin (or/c (is-a?/c snip-admin%) #f)])
           void?]{

Sets the snip's administrator. Only an administrator should call this
 method.

The default method sets the internal state of a snip to record its
 administrator. It will not modify this state if the snip is already
 owned by an administrator and the administrator has not blessed the
 transition. If the administrator state of a snip is not modified as
 expected during a sensitive call to this method by an instance of
 @racket[text%] or @racket[pasteboard%], the
 internal state may be forcibly modified (if the new administrator was
 @racket[#f]) or a surrogate snip may be created (if the snip was
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

Creates a new @racket[snip%] instance with @racket[position]
elements, and modifies @this-obj[] to decrement its count by
@racket[position]. The nest snip is installed into @racket[first] and
@this-obj[] is installed into @racket[second].

}}


@defmethod[(write [f (is-a?/c editor-stream-out%)])
           void?]{

Writes the snip to the given stream. (Snip reading is handled by the
 snip class.) Style information about the snip (i.e., the content of
 @method[snip% get-style]) will be saved and restored automatically.

}}
