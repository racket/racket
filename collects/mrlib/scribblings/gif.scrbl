#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/gif
                     file/gif))

@title{GIF and Animated GIF Writing}

@defmodule[mrlib/gif]


@defproc[(write-gif [bitmap (or/c (is-a?/c bitmap%)
                                  (-> (is-a?/c bitmap%)))]
                    [filename path-string])
         void?]{

Writes the given @scheme[bitmap] to @scheme[filename] as a GIF image,
where @scheme[bitmap] is either an instance of @scheme[bitmap%] or a
thunk (to be called just once) that generates such an object. If the
bitmap uses more than 256 colors, it is automatically quantized using
a simple algorithm; see @scheme[quantize]. If the bitmap has a mask
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

Writes the bitmaps in @scheme[bitmaps] to @scheme[filename] as an
animated GIF. The @scheme[bitmaps] list can contain a mixture of
@scheme[bitmap%] objects and thunks (each called just once) that
produce @scheme[bitmap%] objects. The @scheme[delay-csec] argument is
the amount of time in 1/100s of a second to wait between transitions.
If @scheme[loop?] is a true value, then the GIF is marked as a looping
animation.

If @scheme[one-at-a-time?] is @scheme[#f], then the content of all
images is collected and quantized at once, to produce a single
colortable; a drawback to this approach is that it uses more memory,
and it allows less color variation among animation frames. Even when
@scheme[one-at-a-time?] is @scheme[#f], the result of each thunk in
@scheme[bitmaps] is converted to a byte-string one at a time.

If @scheme[one-at-a-time?] is true, then the bitmaps are quantized and
written to the file one at a time; that is, for each thunk in
@scheme[bitmaps], its result is written and discarded before another
thunk is called. A drawback to this approach is that a separate
colortable is written for each frame in the animation, which can make
the resulting file large.

If @scheme[last-frame-delay] is not false, a delay of
@scheme[last-frame-delay] (in 1/100s of a second) is added to the last
frame. This extra delay is useful when @scheme[loop?] is true.}
