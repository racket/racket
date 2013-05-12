#lang scribble/doc
@(require "pict-diagram.rkt"
          scribble/eval scribble/manual
          pict/face pict
          (for-label racket/gui 
                     slideshow/base slideshow/code
                     pict/flash pict/face pict/balloon
                     (except-in racket only drop)
                     pict
                     pict/convert))


@(define ss-eval (make-base-eval))
@(ss-eval '(require pict racket/math racket/class racket/draw
                    racket/list pict/balloon pict/flash))

@title{More Pict Constructors}


@section{Dingbats}

@defproc[(cloud [w real?]
                [h real?] 
                [color (or/c string? (is-a?/c color%)) "gray"])
         pict?]{

Creates a fluffy cloud.

@examples[#:eval ss-eval
  (cloud 100 75)
  (cloud 100 75 "lavenderblush")
]}

@defproc[(file-icon [w real?]
                    [h real?] 
                    [color (or/c string? (is-a?/c color%) any/c)]
                    [shaded? any/c #f])
         pict?]{

Creates a Mac-like file icon, optionally shaded. If @racket[color] is
not a string or @racket[color%] object, it is treated as a boolean, in
which case true means @racket["gray"] and false means
@racket["white"].

@examples[#:eval ss-eval
  (file-icon 50 60 "bisque")
  (file-icon 50 60 "honeydew" #t)
]}

@defproc[(standard-fish [w real?]
                        [h real?] 
                        [#:direction direction (or/c 'left 'right) 'left] 
                        [#:color color (or/c string? (is-a?/c color%)) "blue"] 
                        [#:eye-color eye-color (or/c string? (is-a?/c color%) #f) "black"]
                        [#:open-mouth open-mouth (or/c boolean? real?) #f])
         pict?]{

Creates a fish swimming either @racket['left] or @racket['right].
If @racket[eye-color] is @racket[#f], no eye is drawn.

The @racket[open-mouth] argument can be either @racket[#f] (mouth
closed), @racket[#t] (mouth fully open), or a number: @racket[0.0] is
closed, @racket[1.0] is fully open, and numbers in between are
partially open.

@examples[#:eval ss-eval
  (standard-fish 100 50)
  (standard-fish 100 50 #:direction 'right #:color "chocolate")
  (standard-fish 100 50 #:eye-color "saddlebrown" #:color "salmon")
  (standard-fish 100 50 #:open-mouth #t #:color "olive")
]}

@defproc[(jack-o-lantern [size real?]
                         [pumpkin-color (or/c string? (is-a?/c color%)) "orange"] 
                         [face-color (or/c string? (is-a?/c color%)) "black"])
         pict?]{

Creates a jack-o-lantern; use the same pumpkin and face color to get a
plain pumpkin. The @racket[size] determines the width.

@examples[#:eval ss-eval
  (jack-o-lantern 100)
  (jack-o-lantern 100 "cadet blue" "khaki")
]}

@defproc[(angel-wing [w real?]
                     [h real?] 
                     [left? any/c])
         pict?]{

Creates an angel wing, left or right, or any size.  The color and pen
width for drawing the wing outline is the current one.

@examples[#:eval ss-eval
  (angel-wing 100 40 #f)
  (angel-wing 100 40 #t)
]}

@defproc[(desktop-machine [scale real?]
                          [style (listof symbol?) null])
         pict?]{

Produces a picture of ancient desktop computer. The @racket[scale]
argument scales the size relative to the base size of 120 by 115. 

The @racket[style] can include any of the following:

@itemlist[

 @item{@racket['plt] --- include a Racket logo on the machine's screen}

 @item{@racket['binary] --- put 1s and 0s on the machine's screen}

 @item{@racket['devil] --- like @racket['binary], and also give the machine 
                           horns and a tail}

]

@examples[#:eval ss-eval
  (desktop-machine 1)
  (desktop-machine 1 '(devil plt))
  (desktop-machine 1 '(plt binary))
]}
               
@defproc[(thermometer [#:height-% height-% (between/c 0 1) 1]
                      [#:color-% color-% (between/c 0 1) height-%]
                      [#:ticks ticks non-exact-negative-integer? 4]
                      [#:start-color start-color (or/c string? (is-a?/c color%)) "lightblue"]
                      [#:end-color end-color (or/c string? (is-a?/c color%)) "lightcoral"]
                      [#:top-circle-diameter top-circle-diameter positive-real? 40]
                      [#:bottom-circle-diameter bottom-circle-diameter positive-real? 80]
                      [#:stem-height stem-height positive-real? 180]
                      [#:mercury-inset mercury-inset positive-real? 8])
         pict?]{
  Produces a thermometer that consists of a semi-circle on top of a rectangle on
  top of a circle. The sizes of the three components are controlled via the
  @racket[top-circle-diameter], @racket[stem-height], and @racket[bottom-circle-diameter]
  arguments. 
  
  The mercury is drawn the same way, but by creating the three components inset from the
  versions that draw the boundary of the thermometer. This inset is conrolled by the
  @racket[mercury-inset] argument.
  
  The height of the mercury in the thermometer is controlled by the @racket[height-%] argument.
  Its color is interpolated between the @racket[start-color] and @racket[end-color], as 
  determined by the @racket[color-%] argument. 
  
  Finally, some number of ticks are drawn, basd on the @racket[ticks] argument.
  
@examples[#:eval ss-eval
  (thermometer #:stem-height 90
               #:bottom-circle-diameter 40
               #:top-circle-diameter 20
               #:mercury-inset 4)
]}

@; ----------------------------------------

@section{Balloon Annotations}

@defmodule[pict/balloon]{The @racketmodname[pict/balloon]
library provides functions for creating and placing cartoon-speech
balloons.}

@defproc[(wrap-balloon [pict pict?]
                       [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                       [dx real?]
                       [dy real?]
                       [color (or/c string? (is-a?/c color%)) balloon-color]
                       [corner-radius (and/c real? (not/c negative?)) 32])
         balloon?]{

Superimposes @racket[pict] on top of a balloon that wraps it.

The @racket[spike] argument indicates the corner from which a spike
protrudes from the balloon (i.e., the spike that points to whatever
the balloon is about). For example, @racket['n] means ``north,'',
which is a spike in the top middle of the balloon.

The @racket[dx] and @racket[dy] arguments specify how far the spike
should protrude.  For a @racket['w] spike, @racket[dx] should be
negative, etc.

The @racket[color] argument is the background color for the balloon.

The @racket[corner-radius] argument determines the radius of the cicle
used to roun the balloon's corners. As usual, if it is less than
@racket[1], then it acts as a ratio of the balloon's width or height.

The result is a balloon, not a pict. The @racket[balloon-pict]
function extracts a pict whose @tech{bounding box} does not include the
spike, but includes the rest of the image, and the
@racket[balloon-point-x] and @racket[balloon-point-y] functions
extract the location of the spike point. More typically, the
@racket[pin-balloon] function is used to add a balloon to a pict.}

@defproc[(pip-wrap-balloon [pict pict?]
                           [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                           [dx real?]
                           [dy real?]
                           [color (or/c string? (is-a?/c color%)) balloon-color]
                           [corner-radius (and/c real? (not/c negative?)) 32])
         pict?]{

Like @racket[wrap-balloon], but produces a zero-sized pict suitable
for use with @racket[pin-over].}


@defproc*[([(pin-balloon [balloon balloon?]
                         [base pict?]
                         [x real?]
                         [y real?])
            pict?]
           [(pin-balloon [balloon balloon?]
                         [base pict?]
                         [at-pict pict-path?]
                         [find (pict? pict-path? . -> . (values real? real?))])
            pict?])]{

Superimposes the pict in @racket[balloon] onto @racket[base] to
produce a new pict. The balloon is positioned so that its spike points
to the location specified by either @racket[x] and @racket[y]
(numbers) or at the position determined by combining @racket[base] and
@racket[at-pict] with @racket[find]. The @racket[find] function uses
its arguments like @racket[lt-find].

The resulting pict has the same @tech{bounding box}, descent, and ascent as
@racket[base], even if the balloon extends beyond the bounding box.

@examples[#:eval ss-eval
  (define a-pict (standard-fish 70 40))
  (pin-balloon (balloon 40 30 5 'se 5 5)
               (cc-superimpose (blank 300 150) a-pict)
               a-pict
               lc-find)
  (pin-balloon (wrap-balloon (text "Hello!") 'sw -5 3)
               (cc-superimpose (blank 300 150) a-pict)
               a-pict
               rt-find)
]}


@defproc[(balloon [w real?]
                  [h real?]
                  [corner-radius (and/c real? (not/c negative?))]
                  [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                  [dx real?]
                  [dy real?]
                  [color (or/c string? (is-a?/c color%)) balloon-color])
         balloon?]{

Creates a balloon, much like @racket[wrap-balloon] except that the balloon's
width is @racket[w] and its height is @racket[h].}

@defproc*[([(balloon? [v any/c]) boolean?]
           [(make-balloon [pict pict?] [x real?] [y real?]) balloon?]
           [(balloon-pict [balloon balloon?]) pict?]
           [(balloon-point-x [balloon balloon?]) real?]
           [(balloon-point-y [balloon balloon?]) real?])]{

A balloon encapsulates a pict and the position of the balloon's spike
relative to the balloon's top-left corner.}

@defthing[balloon-color (or/c string? (is-a?/c color%))]

The default background color for a balloon.

@defboolparam[balloon-enable-3d on?]{

A parameter that determines whether balloons are drawn with 3-D shading.}

@; ----------------------------------------

@section{Face}

@defmodule[pict/face]{The @racketmodname[pict/face] library
provides functions for a kind of @as-index{Mr. Potatohead}-style face
library.}

@defthing[default-face-color (or/c string (is-a?/c color%))]{

Orange.}

@; helper for the next defproc
@(define (small-face mood) (scale (face mood) 0.25))

@defproc[(face [mood symbol?]
               [color (or/c string (is-a?/c color%)) default-face-color])
         pict?]{

Returns a pict for a pre-configured face with the given base
color. The built-in configurations, selected by mood-symbol, are as
follows:

@tabular[#:sep @hspace[2]
  (list (list @para{@racket['unhappy] --- @racket[(face* 'none 'plain #t default-face-color 6)]}
              @(small-face 'unhappy))
        (list @para{@racket['sortof-unhappy] --- @racket[(face* 'worried 'grimace #t default-face-color 6)]}
              @(small-face 'sortof-unhappy))
        (list @para{@racket['sortof-happy] --- @racket[(face* 'worried 'medium #f default-face-color 6)]}
                    @(small-face 'sortof-happy))
        (list @para{@racket['happy] --- @racket[(face* 'none 'plain #f default-face-color 6)]}
                    @(small-face 'happy))
        (list @para{@racket['happier] --- @racket[(face* 'none 'large #f default-face-color 3)]}
                    @(small-face 'happier))
        (list @para{@racket['embarrassed] --- @racket[(face* 'worried 'medium #f default-face-color 3)]}
                    @(small-face 'embarrassed))
        (list @para{@racket['badly-embarrassed] --- @racket[(face* 'worried 'medium #t default-face-color 3)]}
                    @(small-face 'badly-embarrassed))
        (list @para{@racket['unhappier] --- @racket[(face* 'normal 'large #t default-face-color 3)]}
                    @(small-face 'unhappier))
        (list @para{@racket['happiest] --- @racket[(face* 'normal 'huge #f default-face-color 0 -3)]}
                    @(small-face 'happiest))
        (list @para{@racket['unhappiest] --- @racket[(face* 'normal 'huge #t default-face-color 0 -3)]}
                    @(small-face 'unhappiest))
        (list @para{@racket['mad] --- @racket[(face* 'angry 'grimace #t default-face-color 0)]}
                    @(small-face 'mad))
        (list @para{@racket['mean] --- @racket[(face* 'angry 'narrow #f default-face-color 0)]}
                    @(small-face 'mean))
        (list @para{@racket['surprised] --- @racket[(face* 'worried 'oh #t default-face-color -4 -3 2)]}
                    @(small-face 'surprised)))
]}

@defproc[(face* [eyebrow-kind (or/c 'none 'normal 'worried 'angry)]
                [mouth-kind (or/c 'plain 'smaller 'narrow 'medium 'large 
                                  'huge 'grimace 'oh 'tongue)]
                [frown? any/c]
                [color (or/c string (is-a?/c color%))]
                [eye-inset real?]
                [eyebrow-dy real?]
                [pupil-dx real?]
                [pupil-dy real?]
                [#:eyebrow-shading? eyebrow-on? any/c #t]
                [#:mouth-shading? mouth-on? any/c #t]
                [#:eye-shading? eye-on? any/c #t]
                [#:tongue-shading? tongue-on? any/c #t]
                [#:face-background-shading? face-bg-on? any/c #t]
                [#:teeth? teeth-on? any/c #t])
         pict?]{

Returns a pict for a face:

@itemize[

 @item{@racket[eyebrow-kind] determines the eyebrow shape.}

 @item{@racket[mouth-kind] determines the mouth shape, combined with
       @racket[frown?].}

 @item{@racket[frown?] determines whether the mouth is up or down.}

 @item{@racket[color] determines the face color.}

 @item{@racket[eye-inset] adjusts the eye size; recommend values are
       between 0 and 10.}

 @item{@racket[eyebrow-dy] adjusts the eyebrows; recommend values:
       between -5 and 5.}

 @item{@racket[pupil-dx] adjusts the pupil; recommend values are
       between -10 and 10.}

 @item{@racket[pupil-dy] adjusts the pupil; recommend values are
       between -15 and 15.}

]

The @racket[#:eyebrow-shading?] through
@racket[#:face-background-shading?] arguments control whether a
shading is used for on a particular feature in the face (shading tends
to look worse than just anti-aliasing when the face is small). The
@racket[#:teeth?] argument controls the visibility of the teeth for
some mouth shapes.}

@; ----------------------------------------

@section{Flash}

@defmodule[pict/flash]

@defproc[(filled-flash [width real?]
                       [height real?]
                       [n-points exact-positive-integer? 10]
                       [spike-fraction (real-in 0 1) 0.25]
                       [rotation real? 0])
         pict?]{

Returns a pict for a ``flash'': a spiky oval, like the yellow
background that goes behind a ``new!'' logo on web pages or a box of
cereal.
  
The @racket[height] and @racket[width] arguments determine the size of
the oval in which the flash is drawn, prior to rotation. The actual
height and width may be smaller if @racket[points] is not a multiple
of 4, and the actual height and width will be different if the flash
is rotated.

The @racket[n-points] argument determines the number of points on the
flash.

The @racket[spike-fraction] argument determines how big the flash
spikes are compared to the bounding oval.

The @racket[rotation] argument specifies an angle in radians for
counter-clockwise rotation.

The flash is drawn in the default color.

@examples[#:eval ss-eval
  (filled-flash 100 50)
  (filled-flash 100 50 8 0.25 (/ pi 2))
]}

@defproc[(outline-flash [width real?]
                        [height real?]
                        [n-points exact-positive-integer? 10]
                        [spike-fraction (real-in 0 1) 0.25]
                        [rotation real? 0])
         pict?]{

Like @racket[filled-flash], but drawing only the outline.

@examples[#:eval ss-eval
  (outline-flash 100 50)
  (outline-flash 100 50 8 0.25 (/ pi 2))
]}

@include-section["code.scrbl"]
               
@(close-eval ss-eval)

