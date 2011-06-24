#lang scribble/doc
@(require "common.rkt" (for-label mrlib/gif file/gif))

@title{GIF and Animated GIF Writing}

@defmodule[mrlib/gif]


@defproc[(write-gif [bitmap (or/c (is-a?/c bitmap%)
                                  (-> (is-a?/c bitmap%)))]
                    [filename path-string])
         void?]{

Writes the given @racket[bitmap] to @racket[filename] as a GIF image,
where @racket[bitmap] is either an instance of @racket[bitmap%] or a
thunk (to be called just once) that generates such an object. If the
bitmap uses more than 256 colors, it is automatically quantized using
a simple algorithm; see @racket[quantize]. If the bitmap has a mask
bitmap via @method[bitmap% get-loaded-mask], it is used to determine
transparent pixels in the generated GIF image.}

@defproc[(write-animated-gif [bitmaps (and/c 
					(listof (or/c (is-a?/c bitmap%)
						      (-> (is-a?/c bitmap%))))
					pair?)]
                             [delay-csec (integer-in 0 #xFFFFFFFF)]
                             [filename path-string]
                             [#:loop loop? any/c (and delay-csec #t)]
                             [#:one-at-a-time? one-at-a-time? any/c #f]
                             [#:last-frame-delay last-frame-delay (or/c (integer-in 0 #xFFFFFFFF) false/c) #f])
         void?]{

Writes the bitmaps in @racket[bitmaps] to @racket[filename] as an
animated GIF. The @racket[bitmaps] list can contain a mixture of
@racket[bitmap%] objects and thunks (each called just once) that
produce @racket[bitmap%] objects. The @racket[delay-csec] argument is
the amount of time in 1/100s of a second to wait between transitions.
If @racket[loop?] is a true value, then the GIF is marked as a looping
animation.

If @racket[one-at-a-time?] is @racket[#f], then the content of all
images is collected and quantized at once, to produce a single
colortable; a drawback to this approach is that it uses more memory,
and it allows less color variation among animation frames. Even when
@racket[one-at-a-time?] is @racket[#f], the result of each thunk in
@racket[bitmaps] is converted to a byte-string one at a time.

If @racket[one-at-a-time?] is true, then the bitmaps are quantized and
written to the file one at a time; that is, for each thunk in
@racket[bitmaps], its result is written and discarded before another
thunk is called. A drawback to this approach is that a separate
colortable is written for each frame in the animation, which can make
the resulting file large.

If @racket[last-frame-delay] is not false, a delay of
@racket[last-frame-delay] (in 1/100s of a second) is added to the last
frame. This extra delay is useful when @racket[loop?] is true.}
