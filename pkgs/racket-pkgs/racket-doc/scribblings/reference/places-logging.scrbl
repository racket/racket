#lang scribble/doc 
@(require "mz.rkt" (for-label racket/place))

@title[#:tag "place-logging"]{Places Logging}

Place events are reported to a logger named @racket['place].
In addition to its string message, each event logged for a place has
a data value that is an instance of a @racket[place-event]
@tech{prefab} structure:

@racketblock[
(struct place-event (place-id action value time)
  #:prefab)
]

The @racket[place-id] field is an exact integer that identifies a
place.

The @racket[time] field is an inexact number that represents time in
the same way as @racket[current-inexact-milliseconds].

The @racket[action] field is a symbol:

@itemlist[

 @item{@racket['create]: a place was created. This event is logged in the
       creating place, and the event's @racket[value] field has the
       ID for the created place.}

 @item{@racket['reap]: a place that was previously created in the
       current place has exited (and that fact has been detected,
       possibly via @racket[place-wait]). The event's @racket[value]
       field has the ID for the exited place.}

 @item{@racket['enter]: a place has started, logged within the started
       place. The event's @racket[value] field has @racket[#f].}

 @item{@racket['exit]: a place is exiting, logged within the exiting
       place. The event's @racket[value] field has @racket[#f].}

 @item{@racket['put]: a place-channel message has been sent. The
       event's @racket[value] field is a positive exact integer that
       approximates the message's size.}

 @item{@racket['get]: a place-channel message has been received. The
       event's @racket[value] field is a positive exact integer that
       approximates the message's size.}

]

@history[#:changed "6.0.0.2" @elem{Added logging via @racket['place]
         and @racket[place-event].}]
