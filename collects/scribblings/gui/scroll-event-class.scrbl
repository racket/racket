#lang scribble/doc
@(require "common.rkt")

@defclass/title[scroll-event% event% ()]{

A @racket[scroll-event%] object contains information about a scroll
 event. An instance of @racket[scroll-event%] is always provided to
@method[canvas% on-scroll].

See
@method[scroll-event% get-event-type] for a list of the scroll event types.




@defconstructor[([event-type (or/c 'top 'bottom 'line-up 'line-down 
                                   'page-up 'page-down 'thumb)
                             'thumb]
                 [direction (or/c 'horizontal 'vertical) 'vertical]
                 [position (integer-in 0 10000) 0]
                 [time-stamp exact-integer? 0])]{

See the corresponding @racket[get-] and @racket[set-] methods for
 information about @racket[event-type], @racket[direction], @racket[position],
 and @racket[time-stamp].

}

@defmethod[(get-direction)
           (or/c 'horizontal 'vertical)]{

Gets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @racket['horizontal] or @racket['vertical], respectively. See also
 @method[scroll-event% set-direction].

}

@defmethod[(get-event-type)
           (or/c 'top 'bottom 'line-up 'line-down 
                 'page-up 'page-down 'thumb)]{

Returns the type of the event, one of the following:

@itemize[
@item{@racket['top] --- user clicked a scroll-to-top button}
@item{@racket['bottom] --- user clicked a scroll-to-bottom button}
@item{@racket['line-up]  --- user clicked an arrow to scroll up or left one step}
@item{@racket['line-down] --- user clicked an arrow to scroll down or right one step}
@item{@racket['page-up]  --- user clicked an arrow to scroll up or left one page}
@item{@racket['page-down] --- user clicked an arrow to scroll down or right one page}
@item{@racket['thumb] --- user dragged the scroll position indicator}
]

}

@defmethod[(get-position)
           (integer-in 0 10000)]{

Returns the position of the scrollbar after the action triggering the
 event. See also @method[scroll-event% set-position].

}

@defmethod[(set-direction [direction (or/c 'horizontal 'vertical)])
           void?]{

Sets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @racket['horizontal] or @racket['vertical], respectively. See also
 @method[scroll-event% get-direction].

}

@defmethod[(set-event-type [type (or/c 'top 'bottom 'line-up 'line-down 
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

