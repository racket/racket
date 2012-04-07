#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-admin% object% ()]{

See @|admindiscuss| for information about the role of administrators.
 The @racket[editor-admin%] class is never instantiated directly. It
 is not even instantiated through derived classes by most programmers;
 each @racket[editor-canvas%] and @racket[editor-snip%] object
 creates its own administrator. However, it may be useful to derive a
 new instance of this class to display editors in a new context. Also,
 it may be useful to call the methods of an existing administrator
 from an owned editor.

To create a new @racket[editor-admin%] class, all methods described
 here must be overridden. They are all invoked by the administrator's
 editor.

@defconstructor[()]{

Creates a (useless) editor administrator.


}

@defmethod[(get-dc [x (or/c (box/c real?) #f) #f]
                   [y (or/c (box/c real?) #f) #f])
           (or/c (is-a?/c dc<%>) #f)]{
@methspec{

Returns either the drawing context into which the editor is displayed,
 or the context into which it is currently being drawn. When the
 editor is not embedded, the returned context is always the drawing
 content into which the editor is displayed. If the editor is not
 displayed, @racket[#f] is returned.

The origin of the drawing context is also returned, translated into
 the local coordinates of the editor. For an embedded editor, the
 returned origin is reliable only while the editor is being drawn, or
 while it receives a mouse or keyboard event.

@boxisfillnull[@racket[x] @elem{the x-origin of the DC in editor coordinates}]
@boxisfillnull[@racket[y] @elem{the y-origin of the DC in editor coordinates}]

See also @xmethod[editor<%> editor-location-to-dc-location] and
 @xmethod[editor<%> dc-location-to-editor-location].

}
@methimpl{

Fills all boxes with @racket[0.0] and returns @racket[#f].

}}

@defmethod[(get-max-view [x (or/c (box/c real?) #f)]
                         [y (or/c (box/c real?) #f)]
                         [w (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [h (or/c (box/c (and/c real? (not/c negative?))) #f)]
                         [full? any/c #f])
           void?]{
@methspec{

Same as @method[editor-admin% get-view] unless the editor is visible
 in multiple standard @techlink{display}s. If the editor has multiple
 @techlink{display}s, a region is computed that includes the visible
 region in all @techlink{display}s.

See @method[editor-admin% get-view].

}
@methimpl{

Fills all boxes with @racket[0.0].

}}


@defmethod[(get-view [x (or/c (box/c real?) #f)]
                     [y (or/c (box/c real?) #f)]
                     [w (or/c (box/c (and/c real? (not/c negative?))) #f)]
                     [h (or/c (box/c (and/c real? (not/c negative?))) #f)]
                     [full? any/c #f])
           void?]{
@methspec{

Gets the visible region of the editor within its @techlink{display} (in
 editor coordinates), or the overall size of the viewing region in the
 editor's top-level @techlink{display} (for an embedded editor).

If the @techlink{display} is an editor canvas, see also
 @method[area-container<%> reflow-container]. The viewing area within
 an editor canvas is not the full client area of the canvas, because
 an editor canvas installs a whitespace border around a displayed
 editor within the client area.

The calculation of the editor's visible region is based on the current
 size and scrollbar values of the top-level @techlink{display}. For an
 editor canvas @techlink{display}, the region reported by
 @method[editor-admin% get-view] does not depend on whether the canvas
 is hidden, obscured by other windows, or moved off the edge of the
 screen.

@boxisfillnull[@racket[x] @elem{the left edge of the visible region in editor coordinates}]
@boxisfillnull[@racket[y] @elem{the top edge of the visible region in editor coordinates}]
@boxisfillnull[@racket[w] @elem{the width of the visible region, which may be larger than the editor itself}]
@boxisfillnull[@racket[h] @elem{the height of the visible region, which may be larger than the editor itself}]

If an editor is fully visible and @racket[full?] is @racket[#f], then
 @racket[x] and @racket[y] will both be filled with @racket[0].

If @racket[full?] is a true value, then the returned area is the view
 area of the top-level @techlink{display} for the editor. This result
 is different only when the editor is embedded in another editor; in
 that case, the @racket[x] and @racket[y] values may be meaningless,
 because they are in the coordinate system of the immediate editor
 within the top-level @techlink{display}.

}
@methimpl{

Fills all boxes with @racket[0.0].

}}

@defmethod[(grab-caret [domain (or/c 'immediate 'display 'global) 'global])
           void?]{
@methspec{

Called by the editor to request the keyboard focus. If the request is
 granted, then the administered editor's @method[editor<%> own-caret]
 method will be called.

See @method[editor<%> set-caret-owner] for information about the
 possible values of @racket[domain].

}
@methimpl{

Does nothing.

}}


@defmethod[(modified [modified? any/c])
           void?]{
@methspec{

Called by the editor to report that its modification state has
 changed to either modified or unmodified.

See also @xmethod[editor<%> set-modified].

}
@methimpl{

Does nothing.

}}

@defmethod[(needs-update [localx real?]
                         [localy real?]
                         [w (and/c real? (not/c negative?))]
                         [h (and/c real? (not/c negative?))])
           void?]{
@methspec{

Called by the editor to request a refresh to its displayed
 representation. When the administrator decides that the displayed
 should be refreshed, it calls the editor's @method[editor<%> refresh]
 method.

The @racket[localx], @racket[localy], @racket[w], and @racket[h]
 arguments specify a region of the editor to be updated (in editor
 coordinates).

}
@methimpl{

Does nothing.

}}


@defmethod[(popup-menu [menu (is-a?/c popup-menu%)]
                       [x real?]
                       [y real?])
           boolean?]{
@methspec{

@popupmenuinfo[@elem{administrator's @techlink{display}}
               @elem{top-level editor in this administrator's @techlink{display}}
               @elem{The result is @racket[#t] if the popup succeeds,
                  @racket[#f] otherwise (independent of whether the
                  user selects an item in the popup menu).}]

The menu is displayed at @racket[x] and @racket[y] in editor coordinates.

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(refresh-delayed?)
           boolean?]{

@methspec{

Returns @racket[#t] if updating on this administrator's
 @techlink{display} is currently delayed (usually by
 @xmethod[editor<%> begin-edit-sequence] in an enclosing editor).

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(resized [refresh? any/c])
           void?]{

@methspec{

Called by the editor to notify its @techlink{display} that the
 editor's size or scroll count has changed, so the scrollbars need to
 be adjusted to reflect the new size. The editor generally needs to be
 updated after a resize, but the editor decides whether the update
 should occur immediately. If @racket[refresh?] is not @racket[#f],
 then the editor is requesting to be updated immediately.

}
@methimpl{

Does nothing.

}}


@defmethod[(scroll-to [localx real?]
                      [localy real?]
                      [w (and/c real? (not/c negative?))]
                      [h (and/c real? (not/c negative?))]
                      [refresh? any/c #t]
                      [bias (or/c 'start 'end 'none) 'none])
           boolean?]{
@methspec{

Called by the editor to request scrolling so that the given region is
visible. The editor generally needs to be updated after a scroll, but
the editor decides whether the update should occur immediately.

The @racket[localx], @racket[localy], @racket[w], and @racket[h]
 arguments specify a region of the editor to be made visible by the
 scroll (in editor coordinates).

If @racket[refresh?] is not @racket[#f], then the editor is requesting
 to be updated immediately.

The @racket[bias] argument is one of:
@itemize[
@item{@racket['start] --- if the range doesn't fit in the visible area, show the top-left region}
@item{@racket['none] --- no special scrolling instructions}
@item{@racket['end] --- if the range doesn't fit in the visible area, show the bottom-right region}
]

The return value is @racket[#t] if the @techlink{display} is scrolled,
 @racket[#f] if not (either because the requested region is already
 visible, because the @techlink{display} has zero size, or because the
 editor is currently printing).

If an editor has multiple @techlink{displays}, then if any display
 currently has the keyboard focus, it is scrolled. Otherwise, the
 ``primary owner'' of the editor (see @method[editor-canvas%
 call-as-primary-owner]) is scrolled.

}
@methimpl{

Return @racket[#f]

}}

@defmethod[(update-cursor)
           void?]{

@methspec{

Queues an update for the cursor in the @techlink{display} for this
 editor.  The actual cursor used will be determined by calling the
 editor's @method[editor<%> adjust-cursor] method.

}
@methimpl{

Does nothing.

}}}

