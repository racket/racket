#lang scribble/doc
@(require "common.rkt")

@defclass/title[snip-admin% object% ()]{

See @|admindiscuss| for information about the role of administrators.
 The @racket[snip-admin%] class is never instantiated directly. It
 is not even instantiated through derived classes by most programmers;
 each @racket[text%] or @racket[pasteboard%] object
 creates its own administrator. However, it may be useful to derive a
 new instance of this class to display snips in a new context. Also,
 it may be useful to call the methods of an existing administrator
 from an owned snip.

To create a new @racket[snip-admin%] class, all methods described here
 must be overridden. They are all invoked by the administrator's snip.

Because a @racket[snip-admin%] object typically owns more than one
 snip, many methods require a @racket[snip%] object as an argument.



@defconstructor[()]{

Creates a (useless) editor administrator.

}

@defmethod[(get-dc)
           (or/c (is-a?/c dc<%>) #f)]{

Gets a drawing context suitable for determining display size
 information. If the snip is not displayed, @racket[#f] is returned.

}

@defmethod[(get-editor)
           (or/c (is-a?/c text%) (is-a?/c pasteboard%))]{

Returns the editor that this administrator reports to (directly or
 indirectly).

}

@defmethod[(get-view [x (or/c (box/c real?) #f)]
                     [y (or/c (box/c real?) #f)]
                     [w (or/c (box/c (and/c real? (not/c negative?))) #f)]
                     [h (or/c (box/c (and/c real? (not/c negative?))) #f)]
                     [snip (or/c (is-a?/c snip%) #f) #f])
           void?]{
@methspec{

Gets the @techlink{location} and size of the visible region of a snip in snip
 coordinates. The result is undefined if the given snip is not managed
 by this administrator.

If @racket[snip] is not @racket[#f], the current visible region of the
 snip is installed in the boxes @racket[x], @racket[y], @racket[w],
 and @racket[h].  The @racket[x] and @racket[y] values are relative to
 the snip's top-left corner. The @racket[w] and @racket[h] values may
 be larger than the snip itself.

If @racket[snip] is @racket[#f], the total visible region of the
 snip's top-level @techlink{display} is returned in editor
 coordinates. Using @racket[#f] for @racket[snip] is analogous to
 using @racket[#t] for @racket[full?] in @xmethod[editor-admin%
 get-view].

If no snip is specified, then the @techlink{location} and size of the snip's
 editor are returned, instead, in editor coordinates.

See also @xmethod[editor-admin% get-view].

}
@methimpl{

Fills all boxes with @racket[0.0].

}}

@defmethod[(get-view-size [w (or/c (box/c (and/c real? (not/c negative?))) #f)]
                          [h (or/c (box/c (and/c real? (not/c negative?))) #f)])
           void?]{

@methspec{

Gets the visible size of the administrator's @techlink{display} region.

If the @techlink{display} is an editor canvas, see also
 @method[area-container<%> reflow-container].

}
@methimpl{

Fills all boxes with @racket[0.0].

}
}

@defmethod[(modified [snip (is-a?/c snip%)]
                     [modified? any/c])
           void?]{
@methspec{

Called by a snip to report that its modification state has changed to
 either modified or unmodified.

}
@methimpl{

Does nothing.

}}

@defmethod[(needs-update [snip (is-a?/c snip%)]
                         [localx real?]
                         [localy real?]
                         [w (and/c real? (not/c negative?))]
                         [h (and/c real? (not/c negative?))])
           void?]{
@methspec{

Called by the snip to request that the snip's display needs to be
 updated. The administrator determines when to actually update the
 snip; the snip's @method[snip% draw] method is eventually called.

The @racket[localx], @racket[localy], @racket[w], and @racket[h]
 arguments specify a region of the snip to be refreshed (in snip
 coordinates).

No update occurs if the given snip is not managed by this
 administrator.

}
@methimpl{

Does nothing.

}}

@defmethod[(popup-menu [menu (is-a?/c popup-menu%)]
                       [snip (is-a?/c snip%)]
                       [x real?]
                       [y real?])
           boolean?]{
@methspec{

Opens a popup menu in the @techlink{display} for this snip's editor.  The result
 is @racket[#t] if the popup succeeds, @racket[#f] otherwise (independent
 of whether the user selects an item in the popup menu).

The menu is placed at @racket[x] and @racket[y] in @racket[snip]
 coordinates.

While the menu is popped up, its target is set to the top-level editor
 in the @techlink{display} for this snip's editor. See
@method[popup-menu% get-popup-target] for more information.

}
@methimpl{

Returns @racket[#f].

}}

@defmethod[(recounted [snip (is-a?/c snip%)]
                      [refresh? any/c])
           void?]{
@methspec{

Called by a snip to notify the administrator that the specified snip
 has changed its @techlink{count}. The snip generally needs to be updated after
 changing its @techlink{count}, but the snip decides whether the update should
 occur immediately.

If @racket[refresh?] is not @racket[#f], then the snip is requesting
 to be updated immediately. Otherwise, @method[snip-admin%
 needs-update] must eventually be called as well.

The method call is ignored if the given snip is not managed by this
 administrator.

}
@methimpl{

Does nothing.

}}

@defmethod[(release-snip [snip (is-a?/c snip%)])
           boolean?]{

@methspec{

Requests that the specified snip be released. If this administrator is
 not the snip's owner or if the snip cannot be released, then
 @racket[#f] is returned. Otherwise, @racket[#t] is returned and the
 snip is no longer owned.

See also @xmethod[editor<%> release-snip] .

The result is @racket[#f] if the given snip is not managed by this
 administrator.

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(resized [snip (is-a?/c snip%)]
                    [refresh? any/c])
           void?]{
@methspec{

Called by a snip to notify the administrator that the specified snip
 has changed its display size. The snip generally needs to be updated
 after a resize, but the snip decides whether the update should occur
 immediately.

If @racket[refresh?] is not @racket[#f], then the snip is requesting
 to be updated immediately, as if calling @method[snip-admin%
 needs-update].  Otherwise, @method[snip-admin% needs-update] must
 eventually be called as well.


The method call is ignored if the given snip is not managed by this
 administrator.

}
@methimpl{

Does nothing.

}}


@defmethod[(scroll-to [snip (is-a?/c snip%)]
                      [localx real?]
                      [localy real?]
                      [w (and/c real? (not/c negative?))]
                      [h (and/c real? (not/c negative?))]
                      [refresh? any/c]
                      [bias (or/c 'start 'end 'none) 'none])
           boolean?]{
@methspec{

Called by the snip to request scrolling so that the given region is
 visible. The snip generally needs to be updated after a scroll, but
 the snip decides whether the update should occur immediately.

The @racket[localx], @racket[localy], @racket[w], and @racket[h] arguments specify
 a region of the snip to be made visible by the scroll (in snip
 coordinates).

If @racket[refresh?] is not @racket[#f], then the editor is requesting to
 be updated immediately.

The @racket[bias] argument is one of:
@itemize[

 @item{@racket['start] --- if the range doesn't fit in the visible area, show the top-left region}

 @item{@racket['none] --- no special scrolling instructions}

 @item{@racket['end] --- if the range doesn't fit in the visible area, show the bottom-right region}

]

The result is @racket[#t] if the editor is scrolled, @racket[#f]
 otherwise.

The method call is ignored (and the result is @racket[#f]) if the given
 snip is not managed by this administrator.

}
@methimpl{

Returns @racket[#f].

}}

@defmethod[(set-caret-owner [snip (is-a?/c snip%)]
                            [domain (or/c 'immediate 'display 'global)])
           void?]{
@methspec{

Requests that the keyboard focus is assigned to the specified snip.
 If the request is granted, the @method[snip% own-caret] method of the
 snip is called.

See @method[editor<%> set-caret-owner] for information about the
 possible values of @racket[domain].


The method call is ignored if the given snip is not managed by this
 administrator.

}
@methimpl{

Does nothing.

}}


@defmethod[(update-cursor)
           void?]{

@methspec{

Queues an update for the cursor in the @techlink{display} for this
 snip's editor.  The actual cursor used will be determined by calling
 the snip's @method[snip% adjust-cursor] method as appropriate.

}
@methimpl{

Does nothing.

}}


@defmethod[(get-line-spacing)
           (and/c real? (not/c negative?))]{

@methspec{

Returns the spacing inserted by the snip's editor between each
line. 
}
@methimpl{

Returns @racket[0.0]

}}

@defmethod[(get-selected-text-color)
           (or/c (is-a?/c color%) #f)]{

@methspec{

Returns the color that is used to draw selected text or @racket[#f] if
selected text is drawn with its usual color.
}
@methimpl{

Returns @racket[#f].
}}


@defmethod[(call-with-busy-cursor [thunk (-> any)])
           any]{

@methspec{

Calls @racket[thunk] while changing the cursor to a watch cursor for
all windows in the current eventspace.

}

@methimpl{

Does nothing.
}}

@defmethod[(get-tabs [length (or/c (box/c exact-nonnegative-integer?) #f) #f]
                     [tab-width (or/c (box/c real?) #f) #f]
                     [in-units (or/c (box/c any/c) #f) #f])
           (listof real?)]{

@methspec{
Returns the current tab-position array as a list.

@boxisfillnull[@racket[length] @elem{the length of the tab array (and therefore the returned 
list)}]
@boxisfillnull[@racket[tab-width] @elem{the width used for tabs past the 
end of the tab array}]
@boxisfillnull[@racket[in-units] @elem{@racket[#t] if the tabs are specified in
canvas units or @racket[#f] if they are specified in space-widths}]
}

@methimpl{
Returns @racket[null].
}
}
}
