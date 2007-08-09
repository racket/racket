#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[scroll-event% event% ()]{

A @scheme[scroll-event%] object contains information about a scroll
 event. An instance of @scheme[scroll-event%] is always provided to
@method[canvas% on-scroll].

See
@method[scroll-event% get-event-type] for a list of the scroll event types.




@defconstructor[[event-type (symbols/c thumb page-down page-up line-down line-up bottom top) 'thumb]
                [direction (symbols/c vertical horizontal) 'vertical]
                [position (integer-in 0 10000) 0]
                [time-stamp (and/c exact? integer?) 0]]{

See the corresponding @scheme[get-] and @scheme[set-] methods for
 information about @scheme[event-type], @scheme[direction], @scheme[position],
 and @scheme[time-stamp].



}

@defmethod[(get-event-type)
           (symbols/c thumb page-down page-up line-down line-up bottom top)]{
@spec{

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

}}

@defmethod[(set-event-type [type (symbols/c thumb page-down page-up line-down line-up bottom top)])
           void?]{
@spec{

Sets the type of the event. See
@method[scroll-event% get-event-type] for information about each event type.

}}

@defmethod[(get-position)
           (integer-in 0 10000)]{
@spec{

Returns the position of the scrollbar after the action triggering the
 event. See also
@method[scroll-event% set-position].

}}

@defmethod[(set-position [position (integer-in 0 10000)])
           void?]{
@spec{

Records the position of the scrollbar after the action triggering the
 event. (The scrollbar itself is unaffected). See also
@method[scroll-event% get-position].

}}

@defmethod[(get-direction)
           (symbols/c vertical horizontal)]{
@spec{

Gets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @scheme['horizontal] or @scheme['vertical], respectively. See
 also
@method[scroll-event% set-direction].

}}

@defmethod[(set-direction [direction (symbols/c vertical horizontal)])
           void?]{
@spec{

Sets the identity of the scrollbar that was modified by the event,
 either the horizontal scrollbar or the vertical scrollbar, as
 @scheme['horizontal] or @scheme['vertical], respectively. See
 also
@method[scroll-event% get-direction].

}
@impl{




}}}

