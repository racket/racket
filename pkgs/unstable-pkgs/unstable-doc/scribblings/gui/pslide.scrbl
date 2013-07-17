#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
          "../utils.rkt"
          (for-label racket/base
                     slideshow
                     unstable/gui/ppict
                     unstable/gui/pslide
                     unstable/gui/pict))

@title[#:tag "ppict"]{Progressive Picts and Slides}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define the-eval (make-base-eval))
@(the-eval '(require pict unstable/gui/ppict unstable/gui/private/tag-pict))

@section[#:tag "ppicts"]{Progressive Picts}

@defmodule[unstable/gui/ppict]

A @deftech{progressive pict} or ``ppict'' is a kind of @racket[pict]
that has an associated ``pict placer,'' which generally represents a
position and alignment. New picts can be placed on the progressive
pict by calling @racket[ppict-add], and the placer can be updated by
calling @racket[ppict-go]. The @racket[ppict-do] form provides a
compact notation for sequences of those two operations.

@deftogether[[
@defform[(ppict-do base-expr ppict-do-fragment ...)]
@defform/subs[(ppict-do* base-expr ppic-do-fragment ...)
              ([ppict-do-fragment (code:line #:go placer-expr)
                                  (code:line #:set pict-expr)
                                  (code:line #:next)
                                  (code:line #:alt (ppict-do-fragment ...))
                                  (code:line elem-expr)])
              #:contracts ([base-expr pict?]
                           [placer-expr placer?]
                           [pict-expr pict?]
                           [elem-expr (or/c pict? real? #f)])]]]{

Builds a pict (and optionally a list of intermediate picts)
progressively. The @racket[ppict-do] form returns only the final pict;
any uses of @racket[#:next] are ignored. The @racket[ppict-do*] form
returns two values: the final pict and a list of all partial picts
emitted due to @racket[#:next] (the final pict is not included).

A @racket[#:go] fragment changes the current placer. A @racket[#:set]
fragment replaces the current pict state altogether with a new
computed pict. A @racket[#:next] fragment saves a pict including only
the contents emitted so far (but whose alignment takes into account
picts yet to come). A @racket[#:alt] fragment saves the current pict
state, executes the sub-sequence that follows, saves the result (as if
the sub-sequence ended with @racket[#:next]), then restores the saved
pict state before continuing.

The @racket[elem-expr]s are interpreted by the current placer. A
numeric @racket[elem-expr] usually represents a spacing change, but
some placers do not support them. A spacing change only affects added
picts up until the next placer is installed; when a new placer is
installed, the spacing is reset, usually to @racket[0].

The @racket[ppict-do-state] form tracks the current state of the
pict. It is updated before a @racket[#:go] or @racket[#:set] fragment
or before a sequence of @racket[elem-expr]s. It is not updated in the
middle of a chain of @racket[elem-expr]s, however.

@examples[#:eval the-eval
(define base
  (ppict-do (colorize (rectangle 200 200) "gray")
            #:go (coord 1/2 1/2 'cc)
            (colorize (hline 200 1) "gray")
            #:go (coord 1/2 1/2 'cc)
            (colorize (vline 1 200) "gray")))
base
]
The use of @racket[ppict-do] in the defnition of @racket[base] above
is equivalent to
@racketblock[
(let* ([pp (colorize (rectangle 200 200) "gray")]
       [pp (ppict-go pp (coord 1/2 1/2 'cc))]
       [pp (ppict-add pp (colorize (hline 200 1) "gray"))]
       [pp (ppict-go pp (coord 1/2 1/2 'cc))]
       [pp (ppict-add pp (colorize (vline 1 200) "gray"))])
  pp)
]

@examples[#:eval the-eval
(define circles-down-1
  (ppict-do base
            #:go (grid 2 2 2 1 'ct)
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
(ppict-do base
          #:go (coord 0 0 'lt)
          (tag-pict (circle 20) 'circA)
          #:go (coord 1 1 'rb)
          (tag-pict (circle 20) 'circB)
          #:set (let ([p ppict-do-state])
                  (pin-arrow-line 10 p
                                  (find-tag p 'circA) rb-find
                                  (find-tag p 'circB) lt-find)))
(let-values ([(final intermediates)
              (ppict-do* base
                         #:go (coord 1/4 1/2 'cb)
                         (text "shapes:")
                         #:go (coord 1/2 1/2 'lb)
                         #:alt [(circle 20)]
                         #:alt [(rectangle 20 20)]
                         (text "and more!"))])
  (append intermediates (list final)))
]

More examples of @racket[ppict-do] are scattered throughout this
section.
}

@defidform[ppict-do-state]{

Tracks the current state of a @racket[ppict-do] or @racket[ppict-do*]
form.
}

@defproc[(ppict? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a @tech{progressive pict},
@racket[#f] otherwise.
}

@defproc[(ppict-go [p pict?] [pl placer?]) ppict?]{

Creates a @tech{progressive pict} with the given base pict @racket[p]
and the placer @racket[pl].
}

@deftogether[[
@defproc[(ppict-add [pp ppict?]
                    [elem (or/c pict? real? #f 'next)] ...)
         pict?]
@defproc[(ppict-add* [pp ppict?]
                     [elem (or/c pict? real? #f 'next)] ...)
         (values pict? (listof pict?))]]]{

Creates a new pict by adding each @racket[elem] pict on top of
@racket[pp] according to @racket[pp]'s placer. The result pict may or
may not be a @tech{progressive pict}, depending on the placer
used. The @racket[ppict-add] function only the final pict; any
occurrences of @racket['next] are ignored. The @racket[ppict-add*]
function returns two values: the final pict and a list of all partial
picts emitted due to @racket['next] (the final pict is not included).

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

@defproc[(refpoint-placer? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a placer based on a reference
point, @racket[#f] otherwise.
}

@defproc[(coord [rel-x real?] 
                [rel-y real?]
                [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
                [#:abs-x abs-x real? 0]
                [#:abs-y abs-y real? 0]
                [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to @racket[rel-x] and
@racket[rel-y], which are interpeted as fractions of the width and
height of the base @tech{progressive pict}. That is, @racket[0],
@racket[0] is the top left corner of the base's bounding box, and
@racket[1], @racket[1] is the bottom right. Then @racket[abs-x] and
@racket[abs-y] offsets are added to get the final reference point.

Additions are aligned according to @racket[align], a symbol whose name
consists of a horizontal alignment character followed by a vertical
alignment character. For example, if @racket[align] is @racket['lt],
the pict is placed so that its left-top corner is at the reference
point; if @racket[align] is @racket['rc], the pict is placed so that
the center of its bounding box's right edge coincides with the
reference point.

By default, if there are multiple picts to be placed, they are
vertically appended, aligned according to the horizontal component of
@racket[align]. For example, if @racket[align] is @racket['cc], the
default @racket[composer] is @racket[vc-append]; for @racket['lt], the
default @racket[composer] is @racket[vl-append]. The spacing is
initially @racket[0].

@examples[#:eval the-eval
(ppict-do base 
          #:go (coord 1/2 1/2 'rb)
          (colorize (circle 20) "red")
          #:go (coord 1/2 1/2 'lt)
          (colorize (circle 20) "darkgreen"))
(ppict-do base
          #:go (coord 1 0 'rt #:abs-x -5 #:abs-y 10)
          50 (code:comment "change spacing")
          (text "abc")
          (text "12345")
          0  (code:comment "and again")
          (text "ok done"))
(ppict-do base
          #:go (coord 0 0 'lt #:compose ht-append)
          (circle 10)
          (circle 20)
          (circle 30))
]
}

@defproc[(grid [cols exact-positive-integer?]
               [rows exact-positive-integer?]
               [col exact-integer?]
               [row exact-integer?]
               [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
               [#:abs-x abs-x real? 0]
               [#:abs-y abs-y real? 0]
               [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to a position in a
virtual grid. The @racket[row] and @racket[col] indexes are numbered
starting at @racket[1].

Uses of @racket[grid] can be translated into uses of @racket[coord],
but the translation depends on the alignment. For example,
@racket[(grid 2 2 1 1 'lt)] is equivalent to @racket[(coord 0 0 'lt)],
but @racket[(grid 2 2 1 1 'rt)] is equivalent to @racket[(coord 1/2 0 'rt)].

@examples[#:eval the-eval
(define none-for-me-thanks
  (ppict-do base
            #:go (grid 2 2 1 1 'lt)
            (text "You do not like")
            (colorize (text "green eggs and ham?") "darkgreen")))
none-for-me-thanks
(ppict-do none-for-me-thanks
          #:go (grid 2 2 2 1 'rb)
          (colorize (text "I do not like them,") "red")
          (text "Sam-I-am."))
]
}

@defproc[(cascade [step-x (or/c real? 'auto) 'auto]
                  [step-y (or/c real? 'auto) 'auto])
         placer?]{

Returns a placer that places picts by evenly spreading them diagonally
across the base pict in ``cascade'' style. This placer does not
support changing the spacing by including a real number within the
pict sequence.

When a list picts is to be placed, their bounding boxes are normalized
to the maximum width and height of all picts in the list; each pict is
centered in its new bounding box. The picts are then cascaded so there
is @racket[step-x] space between each of the picts' left edges; there
is also @racket[step-x] space between the base pict's left edge and
the first pict's left edge. Similarly for @racket[step-y] and the
vertical spacing.

If @racket[step-x] or @racket[step-y] is @racket['auto], the spacing
between the centers of the picts to be placed is determined
automatically so that the inter-pict spacing is the same as the
spacing between the last pict and the base.

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (colorize (filled-rectangle 100 100) "red")
          (colorize (filled-rectangle 100 100) "blue"))
(ppict-do base
          #:go (cascade 40 20)
          (colorize (filled-rectangle 100 100) "red")
          (colorize (filled-rectangle 100 100) "blue"))
]
}

@defproc[(tile [cols exact-positive-integer?]
               [rows exact-positive-integer?])
         placer?]{

Returns a placer that places picts by tiling them in a grid
@racket[cols] columns wide and @racket[rows] rows high.

@examples[#:eval the-eval
(ppict-do base
          #:go (tile 2 2)
          (circle 50)
          (rectangle 50 50)
          (jack-o-lantern 50)
          (standard-fish 50 30 #:color "red"))
]
}

@defproc[(at-find-pict [find-path (or/c tag-path? pict-path?)]
                       [finder procedure? cc-find]
                       [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
                       [#:abs-x abs-x real? 0]
                       [#:abs-y abs-y real? 0]
                       [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to a reference point
based on an existing pict within the base.

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (tag-pict (standard-fish 40 20 #:direction 'right #:color "red") 'red-fish)
          (tag-pict (standard-fish 50 30 #:direction 'left #:color "blue") 'blue-fish)
          #:go (at-find-pict 'red-fish rc-find 'lc #:abs-x 10)
          (text "red fish"))
]
}

@defproc[(merge-refpoints [x-placer refpoint-placer?] 
                          [y-placer refpoint-placer?])
         refpoint-placer?]{

Returns a placer like @racket[x-placer] except that the y-coordinate of its
reference point is computed by @racket[y-placer].

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (tag-pict (standard-fish 40 20 #:direction 'right #:color "red") 'red-fish)
          (tag-pict (standard-fish 50 30 #:direction 'left #:color "blue") 'blue-fish)
          #:go (merge-refpoints (coord 1 0 'rc)
                                (at-find-pict 'red-fish))
          (text "red fish"))
]
}


@section[#:tag "pslide"]{Progressive Slides}

@defmodule[unstable/gui/pslide]

@defform[(pslide ppict-do-fragment ...)]{

Produce slide(s) using @tech{progressive picts}. See @racket[ppict-do]
for an explanation of @racket[ppict-do-fragment]s.

Note that like @racket[slide] but unlike @racket[ppict-do*], the
number of slides produced is one greater than the number of
@racket[#:next] uses; that is, a slide is created for the final pict.

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
           (let-values ([(final slides0)
                         (ppict-do* (colorize (filled-rectangle 200 150) "white")
                                    #:go (coord 1/20 1/20 'lt) ;; for margins
                                    (text "You do not like")
                                    (colorize (text "green eggs and ham?")
                                              "darkgreen")
                                    #:next
                                    #:go (coord 19/20 19/20 'rb) ;; for margins
                                    (colorize (text "I do not like them,") "red")
                                    (text "Sam-I-am.")
                                    #:next)])
             (let ([slides
                    (inset
                     (vl-append -10 
                                (colorize (text "slides" '(bold . roman)) "white")
                                (inset (apply hc-append 20 slides0) 15))
                     5)])
               (cc-superimpose
                (colorize (filled-rectangle (pict-width slides) (pict-height slides))
                          "darkgray")
                slides))))
]

Note that the text is not flush against the sides of the slide,
because @racket[pslide] uses a base pict the size of the client
area, excluding the margins.
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
