#lang scribble/doc

@(require (for-label pict racket)
          scribble/manual)

@title{Animation Helpers}

These functions are designed to work with the
slide constructors in @racketmodname[slideshow/play].

@declare-exporting[pict slideshow/play]

@section{Pict Interoplations}

@defproc[(fade-pict [n (real-in 0.0 1.0)] [p1 pict?] [p2 pict?]
                    [#:combine combine (pict? pict? . -> . pict?) cc-superimpose])
         pict?]{

Interpolates @racket[p1] and @racket[p2], where the result with
@racket[n] as @racket[0.0] is @racket[p1], and the result with
@racket[n] as @racket[1.0] is @racket[p2]. For intermediate points,
@racket[p1] fades out while @racket[p2] fades in as @racket[n] changes
from @racket[0.0] to @racket[1.0]. At the same time, the width and
height of the generated pict are intermediate between
@racket[p1] and @racket[p2], and the relative baselines and last
pict correspondingly morph within the bounding box.

The @racket[combine] argument determines how @racket[p1] and
@racket[p2] are aligned for morphing. For example, if @racket[p1] and
@racket[p2] both contain multiple lines of text with the same line
height but different number of lines, then using
@racket[ctl-superimpose] would keep the ascent line in a fixed
location relative to the top of the resulting pict as the rest of the
shape morphs around it.}

@defproc[(fade-around-pict [n (real-in 0.0 1.0)]
                           [p1 pict?] 
                           [make-p2 (pict? . -> . pict?)])
         pict?]{

Similar to @racket[fade-pict], but the target is not a fixed
@racket[_p2], but instead a function @racket[make-p2] that takes a
@racket[launder]ed @racket[ghost] of @racket[p1] and places it into a
larger scene. Also, @racket[p1] does not fade out as @racket[n]
increases; instead, @racket[p1] is placed wherever its ghost appears
in the result of @racket[make-p2].

For example,

@RACKETBLOCK[
(lambda (n)
  (fade-around-pict n
                    (code x)
                    (lambda (g) (code (+ #,x 1)))))
]

animates the wrapping of @racket[x] with a @racket[(+ .... 1)] form.}

@defproc[(slide-pict [base pict?]
                     [p pict?]
                     [p-from pict?]
                     [p-to pict?]
                     [n (real-in 0.0 1.0)])
         pict?]{

Pins @racket[p] onto @racket[base], sliding from @racket[p-from] to
@racket[p-to] (which are picts within @racket[base]) as
@racket[n] goes from @racket[0.0] to @racket[1.0]. The top-left
locations of @racket[p-from] and @racket[p-to] determine the placement
of the top-left of @racket[p].

The @racket[p-from] and @racket[p-to] picts are typically
@racket[launder]ed @racket[ghost]s of @racket[p] within @racket[base],
but they can be any picts within @racket[base].}

@defproc[(slide-pict/center [base pict?]
                            [p pict?]
                            [p-from pict?]
                            [p-to pict?]
                            [n (real-in 0.0 1.0)])
         pict?]{

  Like @racket[slide-pict], but aligns the center of @racket[p]
  with @racket[p-from] and @racket[p-to].
}

@; --------------------------------------------------

@section{Merging Animations}

@defproc[(sequence-animations [gen (-> (real-in 0.0 1.0) pict?)]
                              ...)
         (-> (real-in 0.0 1.0) pict?)]{

Converts a list of @racket[gen] functions into a single function that
uses each @racket[gen] in sequence.}

@defproc[(reverse-animations [gen (-> (real-in 0.0 1.0) pict?)]
                             ...)
         (-> (real-in 0.0 1.0) pict?)]{

Converts a list of @racket[gen] functions into a single function that
run @racket[(sequence-animations gen ...)] in reverse.}

@; --------------------------------------------------

@section{Stretching and Squashing Time}

@deftogether[(
@defproc[(fast-start [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-end [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-edges [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-middle [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
)]{

Monotonically but non-uniformly maps @racket[n] with fixed
points at @racket[0.0] and @racket[1.0].

The @racket[fast-start] mapping is convex, so that

@racketblock[(slide-pict _base p _p1 _p2 (fast-start n))]

appears to move quickly away from @racket[_p1] and then slowly as it
approaches @racket[_p2], assuming that @racket[n] increases uniformly.

The @racket[fast-end] mapping is concave, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-end _n))]

appears to move slowly away from @racket[_p1] and then quicly as it
approaches @racket[_p2], assuming that @racket[_n] increases uniformly.

The @racket[fast-edges] mapping is convex at first and concave at the
end, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-edges _n))]

appears to move quickly away from @racket[_p1], then more slowly, and
then quickly again near @racket[_p2], assuming that @racket[_n] increases
uniformly.

The @racket[fast-middle] mapping is concave at first and convex at the
end, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-middle _n))]

appears to move slowly away from @racket[_p1], then more quickly, and
then slowly again near @racket[_p2], assuming that @racket[_n] increases
uniformly.}

@defproc[(split-phase [n (real-in 0.0 1.0)])
         (values (real-in 0.0 1.0) (real-in 0.0 1.0))]{

Splits the progression of @racket[n] from @racket[0.0] to @racket[1.0]
into a progression from @racket[(values 0.0 0.0)] to @racket[(values
1.0 0.0)] and then @racket[(values 1.0 0.0)] to @racket[(values 1.0
1.0)].}
