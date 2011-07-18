#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
          "../utils.rkt"
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/future
                     slideshow/pict
                     racket/draw
                     unstable/gui/blur))

@title[#:tag "blur"]{Blur}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define the-eval (make-base-eval))
@(the-eval '(require slideshow/pict unstable/gui/blur))

@defmodule[unstable/gui/blur]

@defproc[(blur [p pict?]
               [h-radius (and/c real? (not/c negative?))]
               [v-radius (and/c real? (not/c negative?)) h-radius])
         pict?]{

Applies a gaussian blur to @racket[p]. The blur radii,
@racket[h-radius] and @racket[v-radius], control the blurriness of the
resulting pict.

Note: blurring is fairly slow. It takes time proportional to
@racketblock[(* (pict-width p) (pict-height p) (+ h-radius v-radius))]
although it may be sped up by a factor of up to
@racket[(processor-count)] due to its use of @racket[future]s.

@examples[#:eval the-eval
(blur (text "blur" null 40) 5)
(blur (text "more blur" null 40) 10)
(blur (text "much blur" null 40) 20)
(blur (text "horiz. blur" null 40) 10 0)
]
The resulting pict has the same bounding box as @racket[p], so when
picts are automatically @racket[clip]ped (as in Scribble documents),
the pict should be @racket[inset] by the blur radius.
@examples[#:eval the-eval
(inset (blur (text "more blur" null 40) 10) 10)
]
}               

@defproc[(blur-bitmap! [bitmap (is-a?/c bitmap%)]
                       [h-radius (and/c real? (not/c negative?))]
                       [v-radius (and/c real? (not/c negative?)) h-radius])
         void?]{

Blurs @racket[bitmap] using blur radii @racket[h-radius] and
@racket[v-radius].
}

@defproc[(shadow [p pict?]
                 [radius (and/c real? (not/c negative?))]
                 [dx real? 0]
                 [dy real? dx]
                 [#:color color (or/c #f string? (is-a?/c color%)) #f]
                 [#:shadow-color shadow-color (or/c #f string? (is-a?/c color%)) #f])
         pict?]{

Creates a shadow effect by superimposing @racket[p] over a
blurred version of @racket[p]. The shadow is offset from @racket[p] by
(@racket[dx], @racket[dy]) units.

If @racket[color] is not @racket[#f], the foreground part is
@racket[(colorize p color)] instead. If @racket[shadow-color] is not
@racket[#f], the shadow part is produced by blurring @racket[(colorize p
shadow-color)].

The resulting pict has the same bounding box as @racket[p].

@examples[#:eval the-eval
(inset (shadow (text "shadow" null 50) 10) 10)
(inset (shadow (text "shadow" null 50) 10 5) 10)
(inset (shadow (text "shadow" null 50) 
               5 0 2 #:color "white" #:shadow-color "red")
       10)
]
}

@(close-eval the-eval)
