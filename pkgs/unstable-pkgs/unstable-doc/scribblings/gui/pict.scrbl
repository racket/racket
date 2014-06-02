#lang scribble/manual
@(require "../utils.rkt"
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/draw
                     racket/future
                     pict
                     unstable/contract
                     unstable/gui/pict))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/math pict unstable/gui/pict))

@title[#:tag "pict"]{Pict Utilities}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/gui/pict]

The functions and macros exported by this module are also exported by
@racketmodname[unstable/gui/slideshow].

@;{----------------------------------------}

@section{Pict Colors}

@defproc[(color [c color/c] [p pict?]) pict?]{

Applies color @racket[c] to picture @racket[p].  Equivalent to @racket[(colorize
p c)].

@examples[#:eval the-eval
(color "red" (disk 20))
]
}

@deftogether[(
@defproc[(red [pict pict?]) pict?]
@defproc[(orange [pict pict?]) pict?]
@defproc[(yellow [pict pict?]) pict?]
@defproc[(green [pict pict?]) pict?]
@defproc[(blue [pict pict?]) pict?]
@defproc[(purple [pict pict?]) pict?]
@defproc[(black [pict pict?]) pict?]
@defproc[(brown [pict pict?]) pict?]
@defproc[(gray [pict pict?]) pict?]
@defproc[(white [pict pict?]) pict?]
@defproc[(cyan [pict pict?]) pict?]
@defproc[(magenta [pict pict?]) pict?]
)]{

These functions apply appropriate colors to picture @racket[p].

@examples[#:eval the-eval
(red (disk 20))
]
}

@deftogether[(
@defproc[(light [color color/c]) color/c]
@defproc[(dark [color color/c]) color/c]
)]{

These functions produce ligher or darker versions of a color.

@examples[#:eval the-eval
(hc-append (colorize (disk 20) "red")
           (colorize (disk 20) (dark "red"))
           (colorize (disk 20) (light "red")))
]
}

@defthing[color/c flat-contract?]{

This contract recognizes color strings, @racket[color%] instances, and RGB color
lists.
}

@;{----------------------------------------}

@section{Pict Manipulation}

@defproc[(fill [pict pict?] [width (or/c real? #f)] [height (or/c real? #f)])
         pict?]{

Extends @racket[pict]'s bounding box to a minimum @racket[width] and/or
@racket[height], placing the original picture in the middle of the space.

@examples[#:eval the-eval
(frame (fill (disk 20) 40 40))
]
}

@defproc[(scale-to [pict pict?]
                   [width real?]
                   [height real?]
                   [#:mode mode (or/c 'preserve 'inset 'distort) 'preserve])
         pict?]{

Scales @racket[pict] so that its width and height are at most
@racket[width] and @racket[height], respectively. If @racket[mode] is
@racket['preserve], the width and height are scaled by the same factor
so @racket[pict]'s aspect ratio is preserved; the result's bounding
box may be smaller than @racket[width] by @racket[height]. If
@racket[mode] is @racket['inset], the aspect ratio is preserved as
with @racket['preserve], but the resulting pict is centered in a
bounding box of exactly @racket[width] by @racket[height]. If
@racket[mode] is @racket['distort], the width and height are scaled
separately.

@examples[#:eval the-eval
(frame (scale-to (circle 100) 40 20))
(frame (scale-to (circle 100) 40 20 #:mode 'inset))
(frame (scale-to (circle 100) 40 20 #:mode 'distort))
]
}

@subsection{Conditional Manipulations}

These pict transformers all take boolean arguments that determine whether to
transform the pict or leave it unchanged.  These transformations can be useful
for staged slides, as the resulting pict always has the same size and shape, and
its contents always appear at the same position, but changing the boolean
argument between slides can control when the transformation occurs.

@deftogether[(
@defproc[(show [pict pict?] [show? truth/c #t]) pict?]
@defproc[(hide [pict pict?] [hide? truth/c #t]) pict?]
)]{

These functions conditionally show or hide an image, essentially choosing
between @racket[pict] and @racket[(ghost pict)].  The only difference between
the two is the default behavior and the opposite meaning of the @racket[show?]
and @racket[hide?] booleans.  Both functions are provided for mnemonic purposes.
}

@defproc[(strike [pict pict?] [strike? truth/c #t]) pict?]{

Displays a strikethrough image by putting a line through the middle of
@racket[pict] if @racket[strike?] is true; produces @racket[pict] unchanged
otherwise.

@examples[#:eval the-eval
(strike (colorize (disk 20) "yellow"))
]
}

@defproc[(shade [pict pict?]
                [shade? truth/c #t]
                [#:ratio ratio (real-in 0 1) 1/2])
         pict?]{

Shades @racket[pict] to show with @racket[ratio] of its normal opacity; if
@racket[ratio] is @racket[1] or @racket[shade?] is @racket[#f], shows
@racket[pict] unchanged.

@examples[#:eval the-eval
(shade (colorize (disk 20) "red"))
]
}

@subsection{Conditional Combinations}

These pict control flow operators decide which pict of several to use.  All
branches are evaluated; the resulting pict is a combination of the pict chosen
by normal conditional flow with @racket[ghost] applied to all the other picts.
The result is a picture large enough to accommodate each alternative, but showing
only the chosen one.  This is useful for staged slides, as the pict chosen may
change with each slide but its size and position will not.

@defform/subs[(pict-if maybe-combine test-expr then-expr else-expr)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses either @racket[then-expr] or @racket[else-expr] based on
@racket[test-expr], similarly to @racket[if].  Combines the chosen, visible
image with the other, invisible image using @racket[combine-expr], defaulting to
@racket[pict-combine].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-if x
                    (disk 20)
                    (disk 40)))])
  (hc-append 10
             (frame (f #t))
             (frame (f #f))))
]
}

@defform/subs[(pict-cond maybe-combine [test-expr pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on the first successful @racket[test-expr],
similarly to @racket[cond].  Combines the chosen, visible image with the other,
invisible images using @racket[combine-expr], defaulting to
@racket[pict-combine].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-cond
             [(eq? x 'circle) (circle 20)]
             [(eq? x 'disk) (disk 40)]
             [(eq? x 'text) (text "ok" null 20)]))])
  (hc-append 10
             (frame (f 'circle))
             (frame (f 'disk))
             (frame (f 'text))))
]
}

@defform/subs[(pict-case test-expr maybe-combine [literals pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on @racket[test-expr] and each list of
@racket[literals], similarly to @racket[case].  Combines the chosen, visible
image with the other, invisible images using @racket[combine-expr], defaulting
to @racket[pict-combine].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-case x
             [(circle) (circle 20)]
             [(disk) (disk 40)]
             [(text) (text "ok" null 20)]))])
  (hc-append 10
             (frame (f 'circle))
             (frame (f 'disk))
             (frame (f 'text))))
]
}

@defform/subs[(pict-match test-expr maybe-combine [pattern pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on @racket[test-expr] and each
@racket[pattern], similarly to @racket[match].  Combines the chosen, visible
image with the other, invisible images using @racket[combine-expr], defaulting
to @racket[pict-combine].

}

@defform[#:id pict-combine pict-combine]{

This syntax parameter determines the default pict combining form used by the
above macros.  It defaults to @racket[lbl-superimpose].
}

@defform[(with-pict-combine combine-id body ...)]{

Sets @racket[pict-combine] to refer to @racket[combine-id] within each of the
@racket[body] terms, which are spliced into the containing context.

@examples[#:eval the-eval
(let ([f (lambda (x)
           (with-pict-combine cc-superimpose
             (pict-case x
               [(circle) (circle 20)]
               [(disk) (disk 40)]
               [(text) (text "ok" null 20)])))])
  (hc-append 10
             (frame (f 'circle))
             (frame (f 'disk))
             (frame (f 'text))))
]
}

@section{Shapes with Borders}

@addition{Vincent St-Amour}

@deftogether[(
@defproc[(ellipse/border [w real?] [h real?]
                         [#:color color color/c "white"]
                         [#:border-color border-color color/c "black"]
                         [#:border-width border-width real? 2])
         pict?]
@defproc[(circle/border [diameter real?]
                        [#:color color color/c "white"]
                        [#:border-color border-color color/c "black"]
                        [#:border-width border-width real? 2])
         pict?]
@defproc[(rectangle/border [w real?] [h real?]
                           [#:color color color/c "white"]
                           [#:border-color border-color color/c "black"]
                           [#:border-width border-width real? 2])
         pict?]
@defproc[(rounded-rectangle/border [w real?] [h real?]
                                   [#:color color color/c "white"]
                                   [#:border-color border-color color/c "black"]
                                   [#:border-width border-width real? 2]
                                   [#:corner-radius corner-radius real? -0.25]
                                   [#:angle angle real? 0])
         pict?]
)]{
These functions create shapes with border of the given color and width.

@examples[#:eval the-eval
(ellipse/border 80 40 #:border-color "blue")
(rounded-rectangle/border 60 60 #:color "red" #:angle 1 #:border-width 3)
(circle/border 40 #:color "green" #:border-color "purple")
(rectangle/border 200 20 #:border-width 5)
]
}

@section{Lines with Labels}

@addition{Scott Owens}

@deftogether[(
@defproc[(pin-label-line [label pict?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f) #f]
			 [#:end-angle end-angle (or/c real? #f) #f]
			 [#:start-pull start-pull real? 1/4]
			 [#:end-pull end-pull real? 1/4]
			 [#:line-width line-width (or/c real? #f) #f]
			 [#:color color (or/c #f string? (is-a?/c color%)) #f]
			 [#:alpha alpha (real-in 0 1) 1]
			 [#:style style pen-style/c 'solid]
			 [#:under? under? any/c #f]
			 [#:x-adjust x-adjust real? 0]
			 [#:y-adjust y-adjust real? 0])
	 pict?]
@defproc[(pin-arrow-label-line [label pict?] [arrow-size real?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f) #f]
			 [#:end-angle end-angle (or/c real? #f) #f]
			 [#:start-pull start-pull real? 1/4]
			 [#:end-pull end-pull real? 1/4]
			 [#:line-width line-width (or/c real? #f) #f]
			 [#:color color (or/c #f string? (is-a?/c color%)) #f]
			 [#:alpha alpha (real-in 0 1) 1]
			 [#:style style pen-style/c 'solid]
			 [#:solid? solid? boolean? #t]
			 [#:under? under? any/c #f]
			 [#:hide-arrowhead? hide-arrowhead? any/c #f]
			 [#:x-adjust x-adjust real? 0]
			 [#:y-adjust y-adjust real? 0])
	 pict?]
@defproc[(pin-arrows-label-line [label pict?] [arrow-size real?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f) #f]
			 [#:end-angle end-angle (or/c real? #f) #f]
			 [#:start-pull start-pull real? 1/4]
			 [#:end-pull end-pull real? 1/4]
			 [#:line-width line-width (or/c real? #f) #f]
			 [#:color color (or/c #f string? (is-a?/c color%)) #f]
			 [#:alpha alpha (real-in 0 1) 1]
			 [#:style style pen-style/c 'solid]
			 [#:solid? solid? boolean? #t]
			 [#:under? under? any/c #f]
			 [#:hide-arrowhead? hide-arrowhead? any/c #f]
			 [#:x-adjust x-adjust real? 0]
			 [#:y-adjust y-adjust real? 0])
	 pict?]
)]{
These functions behave like @racket[pin-line], @racket[pin-arrow-line]
and @racket[pin-arrows-line] with the addition of a label attached to
the line.

@examples[#:eval the-eval
(let* ([a (red (disk 20))]
       [b (blue (filled-rectangle 20 20))]
       [p (vl-append a (hb-append (blank 100) b))])
  (pin-arrow-label-line 
   (rotate (text "label" null 10) (/ pi -4))
   10 p
   a rb-find
   b lt-find))
]
}

@section{Blur}
@addition{Ryan Culpepper}

@defproc[(blur [p pict?]
               [h-radius (and/c real? (not/c negative?))]
               [v-radius (and/c real? (not/c negative?)) h-radius])
         pict?]{

Blurs @racket[p] using an iterated box blur that approximates a
gaussian blur. The @racket[h-radius] and @racket[v-radius] arguments
control the strength of the horizontal and vertical components of the
blur, respectively. They are given in terms of pict units, which may
not directly correspond to screen pixels.

The @racket[blur] function takes work proportional to
@racketblock[(* (pict-width p) (pict-height p))]
but it may be sped up by a factor of up to @racket[(processor-count)]
due to the use of @racket[future]s.

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

@defproc[(blur-bitmap! [bitmap (is-a?/c bitmap%)]
                       [h-radius (and/c real? (not/c negative?))]
                       [v-radius (and/c real? (not/c negative?)) h-radius])
         void?]{

Blurs @racket[bitmap] using blur radii @racket[h-radius] and
@racket[v-radius].
}


@subsection{Tagged Picts}

@defproc[(tag-pict [p pict?] [tag symbol?]) pict?]{

Returns a pict like @racket[p] that carries a symbolic tag. The tag
can be used with @racket[find-tag] to locate the pict.
}

@defproc[(find-tag [p pict?] [find tag-path?])
         (or/c pict-path? #f)]{

Locates a sub-pict of @racket[p]. Returns a pict-path that can be used
with functions like @racket[lt-find], etc.

@examples[#:eval the-eval
(let* ([a (tag-pict (red (disk 20)) 'a)]
       [b (tag-pict (blue (filled-rectangle 20 20)) 'b)]
       [p (vl-append a (hb-append (blank 100) b))])
  (pin-arrow-line 10 p
                  (find-tag p 'a) rb-find
                  (find-tag p 'b) lt-find))
]
}

@defproc[(find-tag* [p pict?] [find tag-path?])
         (listof pict-path?)]{

Like @racket[find-tag], but returns all pict-paths corresponding to
the given tag-path.

@examples[#:eval the-eval
(let* ([a (lambda () (tag-pict (red (disk 20)) 'a))]
       [b (lambda () (tag-pict (blue (filled-rectangle 20 20)) 'b))]
       [as (vc-append 10 (a) (a) (a))]
       [bs (vc-append 10 (b) (b) (b))]
       [p (hc-append as (blank 60 0) bs)])
  (for*/fold ([p p])
      ([apath (in-list (find-tag* p 'a))]
       [bpath (in-list (find-tag* p 'b))])
    (pin-arrow-line 4 p
                    apath rc-find
                    bpath lc-find)))
]
}

@defproc[(tag-path? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a symbol or a non-empty list of
symbols, @racket[#f] otherwise.
}

@section{Shadow Frames}

@defproc[(shadow-frame [pict pict?] ...
                       [#:sep separation real? 5]
                       [#:margin margin real? 20]
                       [#:background-color bg-color (or/c string? (is-a?/c color%)) "white"]
                       [#:frame-color frame-color (or/c string? (is-a?/c color%)) "gray"]
                       [#:frame-line-width frame-line-width (or/c real? #f) 0]
                       [#:shadow-side-length shadow-side-length real? 4]
                       [#:shadow-top-y-offset shadow-top-y-offset real? 10]
                       [#:shadow-bottom-y-offset shadow-bottom-y-offset real? 4]
                       [#:shadow-descent shadow-descent (and/c real? (not/c negative?)) 40]
                       [#:shadow-alpha-factor shadow-alpha-factor real? 3/4]
                       [#:blur blur-radius (and/c real? (not/c negative?)) 20])
         pict?]{

Surrounds the @racket[pict]s with a rectangular frame that casts a
symmetric ``curled paper'' shadow.

The @racket[pict]s are vertically appended with @racket[separation]
space between them. They are placed on a rectangular background of
solid @racket[bg-color] with @racket[margin] space on all sides. A
frame of @racket[frame-color] and @racket[frame-line-width] is added
around the rectangle. The rectangle casts a shadow that extends
@racket[shadow-side-length] to the left and right, starts
@racket[shadow-top-y-offset] below the top of the rectangle and
extends to @racket[shadow-bottom-y-offset] below the bottom of the
rectangle in the center and an additional @racket[shadow-descent]
below that on the sides. The shadow is painted using a linear
gradient; @racket[shadow-alpha-factor] determines its density at the
center. Finally, the shadow is blurred by @racket[blur-radius]; all
previous measurements are pre-blur measurements.

@examples[#:eval the-eval
(scale (shadow-frame (text "text in a nifty frame" null 60)) 1/2)
]
}

@defproc[(arch [outer-width real?]
               [inner-width real?]
               [solid-height real?]
               [leg-height real?])
         pict?]{

Creates an arch.

@examples[#:eval the-eval
(colorize (arch 100 80 20 20) "red")
]
}

@section{Drawing}
@addition{Jay McCarthy}

@defproc[(draw-pict-centered [p pict?] [dc (is-a?/c dc<%>)] [w real?] [h real?]) void?]{
 Draws the pict @racket[p] on @racket[dc] centered in a @racket[w]x@racket[h] rectangle.
}

@section{Additional combinators}

@addition{Asumu Takikawa}

@defproc[(backdrop [pict pict?] [#:color color color/c "white"]) pict?]{
Adds a background highlighted with @racket[color] to
@racket[pict].

@examples[#:eval the-eval
  (backdrop (circle 20) #:color "whitesmoke")
  (backdrop (text "broccoli rabé") #:color "PaleGreen")
]}

@defproc[(cross-out [pict pict?]
                    [#:width width real? 1]
                    [#:style style
                     (or/c 'transparent 'solid 'xor
                           'hilite 'dot 'long-dash 'short-dash
                           'dot-dash 'xor-dot 'xor-long-dash
                           'xor-short-dash 'xor-dot-dash)
                     'solid]
                    [#:color color color/c "black"])
                    pict?]{
  Crosses out @racket[pict] with two diagonal lines drawn with
  the given line @racket[width] and with the line @racket[style].
  The lines are colored with @racket[color].

@examples[#:eval the-eval
  (cross-out (circle 20))
  (cross-out (rectangle 30 20) #:width 2 #:style 'long-dash)
  (cross-out (text "rapini") #:width 3 #:color "red")
]}

@(close-eval the-eval)

@section{Alignment}

@(require (for-label unstable/gui/pict/align))
@defmodule[unstable/gui/pict/align]

@defthing[align/c contract?]{A contract for the values @racket['(lt ct rt lc cc rc lb cb rb)].}
@defthing[halign/c contract?]{A contract for the values @racket['(l c r)].}
@defthing[valign/c contract?]{A contract for the values @racket['(t c b)].}

@defproc[(align->h [a align/c]) halign/c]{Extracts the @racket[halign/c] part from @racket[a].}
@defproc[(align->v [a align/c]) valign/c]{Extracts the @racket[valign/c] part from @racket[a].}

@defproc[(align->frac [a (or/c halign/c valign/c)]) real?]{Computes the fraction corresponding to an alignment where the top-left is @racket[0].}

@defproc[(halign->vcompose [ha halign/c]) procedure?]{Returns the @racket[h*-append] function for horizontal alignment.}
@defproc[(valign->hcompose [va valign/c]) procedure?]{Returns the @racket[v*-append] function for vertical alignment.}

@defproc[(pin-over/align [scene pict?] [x real?] [y real?] [halign halign/c] [valign valign/c] [pict pict?]) pict?]{Pins @racket[pict] over @racket[scene] centered at @racket[x]x@racket[y] aligned as specified in @racket[halign] and @racket[valign].}

@section{PLT Logos}

@(require (for-label unstable/gui/pict/plt-logo))
@defmodule[unstable/gui/pict/plt-logo]

@addition{Jay McCarthy}

@defproc[(make-plt-title-background [w real?] [h real?]) pict?]{Draws the PLT logo inside a @racket[w]x@racket[h] rectangle.}
