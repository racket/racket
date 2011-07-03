#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
          "../utils.rkt"
          (for-label racket/base
                     slideshow
                     unstable/gui/ppict
                     unstable/gui/pslide))

@title[#:tag "ppict"]{Progressive Picts and Slides}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define the-eval (make-base-eval))
@(the-eval '(require slideshow/pict unstable/gui/ppict))

@section[#:tag "ppicts"]{Progressive Picts}

@defmodule[unstable/gui/ppict]

A @deftech{progressive pict} or ``ppict'' is a kind of @racket[pict]
that has an associated ``pict placer,'' which generally represents a
position and alignment. New picts can be placed on the progressive
pict by calling @racket[ppict-add], and the placer can be updated by
calling @racket[ppict-go]. The @racket[ppict-do] function provides a
compact notation for sequences of those two operations.

@examples[#:eval the-eval
(define base
  (ppict-do (colorize (rectangle 200 200) "gray")
            (coord 1/2 1/2 'cc)
            (colorize (hline 200 1) "gray")
            (coord 1/2 1/2 'cc)
            (colorize (vline 1 200) "gray")))
base
(define circles-down-1
  (ppict-do base
            (grid 2 2 1 0 'ct)
            10
            (circle 20)
            (circle 20)
            30
            (circle 20)))
circles-down-1
(define circles-down-2
  (ppict-do circles-down-1
            (colorize (circle 20) "red")
            40
            (colorize (circle 20) "red")))
(code:line (inset circles-down-2 20) (code:comment "draws outside its bounding box"))
(inset (clip circles-down-2) 20)
]


@defproc[(ppict? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a @tech{progressive pict},
@racket[#f] otherwise.
}

@defproc[(ppict-do [p pict?]
                   [cmd (or/c pict? real? #f placer?)] ...)
         pict?]{

Starting with @racket[p], applies @racket[ppict-go] for every
@racket[cmd] that is a placer and @racket[ppict-add] for every
sequence of @racket[cmd]s that are picts, real numbers, and
@racket[#f].

A spacing change, represented by a real number, only affects added
picts up until the next placer is installed; when a placer is
installed, the spacing is reset to @racket[0].

For example, the following code
@racketblock[
(ppict-do (colorize (rectangle 200 200) "gray")
          (coord 1/2 1/2 'cc)
          (colorize (hline 200 1) "gray")
          (coord 1/2 1/2 'cc)
          (colorize (vline 1 200) "gray"))
]
is equivalent to
@racketblock[
(let ([pp (colorize (rectangle 200 200) "gray")]
      [pp (ppict-go pp (coord 1/2 1/2 'cc))]
      [pp (ppict-add pp (colorize (hline 200 1) "gray"))]
      [pp (ppict-go pp (coord 1/2 1/2 'cc))]
      [pp (ppict-add pp (colorize (vline 1 200) "gray"))])
  pp)
]
}

@defproc[(ppict-go [p pict?] [pl placer?]) ppict?]{

Creates a @tech{progressive pict} with the given base pict @racket[p]
and the placer @racket[pl].
}

@defproc[(ppict-add [pp ppict?]
                    [elem (or/c pict? real? #f)] ...)
         pict?]{

Creates a new pict by adding each @racket[elem] pict on top of
@racket[pp] according to @racket[pp]'s placer. The result pict may or
may not be a @tech{progressive pict}, depending on the placer used.

An @racket[elem] that is a real number changes the spacing for
subsequent additions. A @racket[elem] that is @racket[#f] is
discarded; it is permitted as a convenience for conditionally
including sub-picts. Note that @racket[#f] is not equivalent to
@racket[(blank 0)], since the latter will cause spacing to be added
around it.
}

@defproc[(placer? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a placer, @racket[#f] otherwise.
}

@defproc[(coord [relx real?] [rely real?]
                [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
                [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         placer?]{

Returns a placer that places picts according to a reference point
determined by @racket[relx] and @racket[rely], which are interpeted as
fractions of the width and height of the base @tech{progressive
pict}. That is, @racket[0], @racket[0] is the top left corner of the
base's bounding box, and @racket[1], @racket[1] is the bottom right.

Additions are aligned according to @racket[align], a symbol whose name
consists of a horizontal alignment character followed by a vertical
alignment character. If @racket[align] is @racket['lt], the pict is
placed so that its left-top corner is at the reference point; if
@racket[align] is @racket['rc], the pict is placed so that the center
of its bounding box's right edge coincides with the reference point.

By default, if there are multiple picts to be placed, they are
verictally appended, aligned according to the horizontal component of
@racket[align]. For example, if @racket[align] is @racket['cc], the
default @racket[composer] is @racket[vc-append]; for @racket['lt], the
default @racket[composer] is @racket[vl-append]. The spacing is
initially @racket[0].

@;{
The result of @racket[ppict-add] using a @racket[coord] placer is
another progressive pict only if 
}

@examples[#:eval the-eval
(ppict-do base 
          (coord 1/3 3/4 'cc)
          (circle 20))
(ppict-do base
          (coord 1 0 'rt)
          50 (code:comment "change spacing")
          (text "abc")
          (text "12345")
          0  (code:comment "and again")
          (text "ok done"))
(ppict-do base
          (coord 0 0 'lt #:compose ht-append)
          (circle 10)
          (circle 20)
          (circle 30))
]
}

@defproc[(grid [cols exact-positive-integer?]
               [rows exact-positive-integer?]
               [col exact-nonnegative-integer?]
               [row exact-nonnegative-integer?]
               [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
               [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         placer?]{

Returns a placer that places picts according to a position in a
virtual grid. The @racket[row] and @racket[col] indexes are numbered
starting at @racket[0].

Uses of @racket[grid] can be translated into uses of @racket[coord],
but the translation depends on the alignment. For example,
@racket[(grid 2 2 0 0 'lt)] is equivalent to @racket[(coord 0 0 'lt)],
but @racket[(grid 2 2 0 0 'rt)] is equivalent to @racket[(coord 1/2 0 'rt)].

@examples[#:eval the-eval
(define none-for-me-thanks
  (ppict-do base
            (grid 2 2 0 0 'lt)
            (text "You do not like")
            (colorize (text "green eggs and ham?") "darkgreen")))
none-for-me-thanks
(ppict-do none-for-me-thanks
          (grid 2 2 1 0 'rb)
          (colorize (text "I do not like them,") "red")
          (text "Sam-I-am."))
]
}


@section[#:tag "pslide"]{Progressive Slides}

@defmodule[unstable/gui/pslide]

@defform/subs[(pslide pslide-fragment ...)
              ([pslide-fragment (code:line #:go placer-expr)
                                (code:line #:next)
                                (code:line elem-expr)])
              #:contracts ([placer-expr placer?]
                           [elem-expr (or/c pict? real? #f)])]{

Constructs a slide using the @tech{progressive pict} mechanism. A
@racket[#:go] directive updates the current placer; a @racket[#:next]
directive causes a slide to be emitted with the contents thus far (but
whose alignment takes into account contents yet to be added); and
other elements have the same meaning as in @racket[ppict-add].

Remember to include @racket[gap-size] after updating the current
placer if you want @racket[slide]-like spacing.

@examples[#:eval the-eval
(eval:alts (pslide #:go (coord 0 0 'lt)
                   (t "You do not like")
                   (colorize (t "green eggs and ham?") "darkgreen")
                   #:next
                   #:go (coord 1 1 'rb)
                   (colorize (t "I do not like them,") "red")
                   (t "Sam-I-am."))
           (let* ([slide1
                   (ppict-do (colorize (filled-rectangle 200 150) "white")
                             (coord 1/20 1/20 'lt) ;; for margins
                             (text "You do not like")
                             (colorize (text "green eggs and ham?")
                             "darkgreen"))]
                  [slide2
                   (ppict-do slide1
                             (coord 19/20 19/20 'rb) ;; for margins
                             (colorize (text "I do not like them,") "red")
                             (text "Sam-I-am."))]
                  [slides
                   (inset
                    (vl-append -5 
                               (colorize (text "slides" '(bold . roman)) "white")
                               (inset (hc-append 20 slide1 slide2) 15))
                    5)])
             (cc-superimpose
              (colorize (filled-rectangle (pict-width slides) (pict-height slides))
                        "darkgray")
              slides)))
]
}

@defparam[pslide-base-pict make-base-pict (-> pict)]{

Controls the initial pict used by @racket[pslide]. The default value
is
@racketblock[
(lambda () (blank client-w client-h))
]
}

@defparam[pslide-default-placer placer placer?]{

Controls the initial placer used by @racket[pslide]. The default value
is
@racketblock[
(coord 1/2 1/2 'cc)
]
}

@(close-eval the-eval)
