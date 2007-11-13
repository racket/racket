#lang scribble/doc
@require["common.ss"]

@defclass/title[scroll-event% event% ()]{

A @scheme[scroll-event%] object contains information about a scroll
 event. An instance of @scheme[scroll-event%] is always provided to
@method[canvas% on-scroll].

See
@method[scroll-event% get-event-type] for a list of the scroll event types.




@defconstructor[([event-type (one-of/c 'top 'bottom 'line-up 'line-down 
                                       'page-up 'page-down 'thumb)
                             'thumb]
                 [direction (one-of/c 'horizontal 'vertical) 'vertical]
                 [position (integer-in 0 10000) 0]
                 [time-stamp (and/c exact? integer?) 0])]{

See the corresponding @scheme[get-] and @scheme[set-] methods for
 information about @scheme[event-type], @scheme[direction], @scheme[position],
 and @scheme[time-stamp].

}

@defmethod[(get-direction)
           (one-of/c 'horizontal 'vertical)]{

Gets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @scheme['horizontal] or @scheme['vertical], respectively. See also
 @method[scroll-event% set-direction].

}

@defmethod[(get-event-type)
           (one-of/c 'top 'bottom 'line-up 'line-down 'page-up 'page-down 'thumb)]{

Returns the type of the event, one of the following:

@itemize{
@item{@scheme['top] --- user clicked a scroll-to-top button}
@item{@scheme['bottom] --- user clicked a scroll-to-bottom button}
@item{@scheme['line-up]  --- user clicked an arrow to scroll up or left one step}
@item{@scheme['line-down] --- user clicked an arrow to scroll down or right one step}
@item{@scheme['page-up]  --- user clicked an arrow to scroll up or left one page}
@item{@scheme['page-down] --- user clicked an arrow to scroll down or right one page}
@item{@scheme['thumb] --- user dragged the scroll position indicator}
}

}

@defmethod[(get-position)
           (integer-in 0 10000)]{

Returns the position of the scrollbar after the action triggering the
 event. See also @method[scroll-event% set-position].

}

@defmethod[(set-direction [direction (one-of/c 'horizontal 'vertical)])
           void?]{

Sets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @scheme['horizontal] or @scheme['vertical], respectively. See also
 @method[scroll-event% get-direction].

}

@defmethod[(set-event-type [type (one-of/c 'top 'bottom 'line-up 'line-down 
                                           'page-up 'page-down 'thumb)])
           void?]{

Sets the type of the event. See @method[scroll-event% get-event-type]
for information about each event type.

}

@defmethod[(set-position [position (integer-in 0 10000)])
           void?]{

Records the position of the scrollbar after the action triggering the
 event. (The scrollbar itself is unaffected). See also
 @method[scroll-event% get-position].

}}

