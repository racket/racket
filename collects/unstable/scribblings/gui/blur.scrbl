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
               [v-radius (and/c real? (not/c negative?)) h-radius]
               [#:mode mode (or/c 'gaussian 'iterated-box 'single-box) 'iterated-box])
         pict?]{

Blurs @racket[p] using a gaussian blur (if @racket[mode] is
@racket['gaussian]), an iterated box blur that approximates a gaussian
blur (if @racket[mode] is @racket['iterated-box]), or a single box
blur (if @racket[mode] is @racket['single-box]). The @racket[h-radius]
and @racket[v-radius] arguments control the strength of the horizontal
and vertical components of the blur, respectively. They are given in
terms of pict units, which may not directly correspond to screen pixels.

The @racket['gaussian] blur mode is quite slow for large blur
radii. It takes work proportional to
@racketblock[(* (pict-width p) (pict-height p) (+ h-radius v-radius))]
The @racket['iterated-box] blur mode is much faster; it takes work
proportional to
@racketblock[(* (pict-width p) (pict-height p))]
The genuine @racket['gaussian] mode generally produces smoother and
lighter images, but the @racket['iterated-box] approximation is
acceptable for most uses. All modes may be sped up by a factor of up
to @racket[(processor-count)] due to the use of @racket[future]s.

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

Genuine @racket['gaussian] blur compared with @racket['iterated-box] blur:
@examples[#:eval the-eval
(vl-append (inset (blur (text "blur" null 40) 10 #:mode 'gaussian) 10)
           (inset (blur (text "blur" null 40) 10 #:mode 'iterated-box) 10))
]
}               

@defproc[(blur-bitmap! [bitmap (is-a?/c bitmap%)]
                       [h-radius (and/c real? (not/c negative?))]
                       [v-radius (and/c real? (not/c negative?)) h-radius]
                       [#:mode mode (or/c 'gaussian 'iterated-box 'single-box) 'iterated-box])
         void?]{

Blurs @racket[bitmap] using blur radii @racket[h-radius] and
@racket[v-radius] and mode @racket[mode].
}

@defproc[(shadow [p pict?]
                 [radius (and/c real? (not/c negative?))]
                 [dx real? 0]
                 [dy real? dx]
                 [#:color color (or/c #f string? (is-a?/c color%)) #f]
                 [#:shadow-color shadow-color (or/c #f string? (is-a?/c color%)) #f]
                 [#:mode mode (or/c 'gaussian 'iterated-box 'single-box) 'iterated-box])
         pict?]{

Creates a shadow effect by superimposing @racket[p] over a
blurred version of @racket[p]. The shadow is offset from @racket[p] by
(@racket[dx], @racket[dy]) units.

If @racket[color] is not @racket[#f], the foreground part is
@racket[(colorize p color)]; otherwise it is just @racket[p]. If
@racket[shadow-color] is not @racket[#f], the shadow part is produced
by blurring @racket[(colorize p shadow-color)]; otherwise it is
produced by blurring @racket[p].

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
