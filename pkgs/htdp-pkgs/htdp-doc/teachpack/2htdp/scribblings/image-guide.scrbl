#lang scribble/doc

@(require (for-label 2htdp/image
                     (except-in lang/htdp-beginner posn make-posn posn? posn-x posn-y image?)
                     lang/posn
                     (only-in racket/base foldl)
                     (except-in racket/gui/base make-color make-pen)
                     file/convertible
                     pict/convert
                     (only-in mrlib/image-core render-image))
          "shared.rkt"
          "img-eval.rkt"
          scribble/decode
          scribble/manual
          scribble/eval
          scribble/core
          scribble/html-properties)

@(define guide-eval (make-img-eval))

@(define-syntax-rule 
   (image-examples exp ...)
   (examples #:eval guide-eval exp ...))

@(define-syntax-rule
   (image-interaction exp ...)
   (interaction #:eval guide-eval exp ...))

@(define-syntax-rule
   (image-interaction/bitmap exp)
   (interaction #:eval guide-eval 
                (eval:alts exp (make-object bitmap%
                                 (open-input-bytes (convert exp 'png-bytes))
                                 'png/alpha))))

@(define-syntax-rule
   (image-interaction/margin num exp)
   (begin
     (racketinput exp)
     (guide-eval '(extra-margin num))
     (interaction-eval-show #:eval guide-eval exp)
     (guide-eval '(extra-margin 0))))

@(interaction-eval #:eval guide-eval 
                   (require racket/list 
                            racket/local
                            file/convertible
                            (only-in racket/draw bitmap%)
                            racket/class))

@title[#:style 
       (style #f (list (render-convertible-as '(svg-bytes png-bytes))))
       #:tag "image-guide"]{Image Guide}

This section introduces the @racketmodname[2htdp/image] library
through a series of increasingly complex image constructions
and discusses some subtle details of cropping and outline
images.

@section{Overlaying, Above, and Beside: A House}

To build a simple-looking house, we can place a triangle above 
a rectangle.

@image-interaction[(above (triangle 40 "solid" "red")
                          (rectangle 40 30 "solid" "black"))]

We can give the house two roofs by putting two triangles next to
each other.

@image-interaction[(above (beside (triangle 40 "solid" "red")
                                  (triangle 40 "solid" "red"))
                          (rectangle 80 40 "solid" "black"))]

But if we want the new roof to be a little smaller, then they do not line
up properly.

@image-interaction[(above (beside (triangle 40 "solid" "red")
                                  (triangle 30 "solid" "red"))
                          (rectangle 70 40 "solid" "black"))]

Instead, we can use @racket[beside/align] to line up the two triangles
along their bottoms instead of along the middles (which is what
@racket[beside] does).

@image-interaction[
(define victorian 
  (above (beside/align "bottom"
                       (triangle 40 "solid" "red")
		       (triangle 30 "solid" "red"))
         (rectangle 70 40 "solid" "black")))
victorian
]
  
To add a door to the house, we can overlay a brown @racket[rectangle],
aligning it with the center bottom of the rest of the house.

@image-interaction[
(define door (rectangle 15 25 "solid" "brown"))
(overlay/align "center" "bottom" door victorian)]

We can use a similar technique to put a doorknob on the door, but instead of
overlaying the doorknob on the entire house, we can overlay it just on the
door.

@image-interaction[
(define door-with-knob
  (overlay/align "right" "center" (circle 3 "solid" "yellow") door))
(overlay/align "center" "bottom" door-with-knob victorian)]

@section{Rotating and Overlaying: A Rotary Phone Dial}

A rotary phone dial can be built by from a black disk and 10 little white ones
by placing the white disks, one at a time, at the top of the black disk and
then rotating the entire black disk. To get started, lets define a function
to make little white disks with numbers on them:

@image-interaction[(define (a-number digit)
                     (overlay
                      (text (number->string digit) 12 "black")
                      (circle 10 "solid" "white")))]

We'll use @racket[place-and-turn] to put the numbers onto the disk:

@image-interaction[(define (place-and-turn digit dial)
                     (rotate 30
                             (overlay/align "center" "top" 
                                            (a-number digit)
                                            dial)))]

For example:

@image-interaction[(place-and-turn
                    0
                    (circle 60 "solid" "black"))]

@image-interaction[(place-and-turn
                    8
                    (place-and-turn
                     9
                     (place-and-turn
                      0
                      (circle 60 "solid" "black"))))]

We can write a single function to put all of the numbers together into the dial:

@image-interaction[(define (place-all-numbers dial)
                     (place-and-turn
                      1
                      (place-and-turn
                       2
                       (place-and-turn
                        3
                        (place-and-turn
                         4
                         (place-and-turn
                          5
                          (place-and-turn
                           6
                           (place-and-turn
                            7
                            (place-and-turn
                             8
                             (place-and-turn
                              9
                              (place-and-turn
                               0
                               dial)))))))))))
                                              
                   (place-all-numbers (circle 60 "solid" "black"))]

That definition is long and tedious to write. We can shorten it using
@racket[foldl]:

@image-interaction[(define (place-all-numbers dial)
                     (foldl place-and-turn
                            dial
                            '(0 9 8 7 6 5 4 3 2 1)))
                                              
                   (place-all-numbers (circle 60 "solid" "black"))]

To finish off the dial, we need to rotate it a little bit to its natural
position and put a white disk in the center of it. Here's the inner dial:

@image-interaction[(define inner-dial
                     (overlay
                      (text "555-1234" 9 "black")
                      (circle 30 "solid" "white")))]

and here's a function to build the entire rotary dial, with an argument
that scales the dial:

@image-interaction[(define (rotary-dial f)
                     (scale
                      f
                      (overlay 
                       inner-dial
                       (rotate
                        -90
                        (place-all-numbers (circle 60 "solid" "black"))))))
                   (rotary-dial 2)]

Looking at the image, it feels like the numbers are too close to the edge of
the dial. So we can adjust the @racket[place-and-turn] function to put a little
black rectangle on top of each number. The rectangle is invisible because it
ends up on top of the black dial, but it does serve to push the digits down
a little.

@image-interaction[(define (place-and-turn digit dial)
                     (rotate 30
                             (overlay/align "center" "top" 
                                            (above 
                                             (rectangle 1 5 "solid" "black")
                                             (a-number digit))
                                            dial)))
                   
                   (rotary-dial 2)]

@section{Alpha Blending}

With shapes that have opaque colors like @racket["red"] and @racket["blue"], 
overlaying one on top completely blots out the one one the bottom. 

For example, the red rectangle here completely covers the blue one.

@image-interaction[(overlay
                    (rectangle 60 100 "solid" (color 127 255 127))
                    (rectangle 100 60 "solid" (color 127 127 255)))]

But @racketmodname[2htdp/image] also supports colors that are not 
completely opaque, via the (optional) fourth argument to @racket[color].

@image-interaction[(overlay
                    (rectangle 60 100 "solid" (color 0 255 0 127))
                    (rectangle 100 60 "solid" (color 0 0 255 127)))]

In this example, the color @racket[(color 0 255 0 127)] looks just
like the color @racket[(color 127 255 127)] when the background
is white. Since white is @racket[(color 255 255 255)], we end up 
getting @racket[1/2] of @racket[255] for the red and blue components
and @racket[255] for the green one. 

We can also use alpha blending to make some interesting effects. 
For example, the function @racket[spin-alot] takes an image argument
and repeatedly places it on top of itself, rotating it each time by 
@racket[1] degree.

@image-interaction[(define (spin-alot t)
                     (local [(define (spin-more i θ)
                               (cond
                                 [(= θ 360) i]
                                 [else 
                                  (spin-more (overlay i (rotate θ t))
                                             (+ θ 1))]))]
                       (spin-more t 0)))]

Here are some uses of @racket[spin-alot], first showing the original
shape and then the spun shape.

@image-interaction[(rectangle 12 120 "solid" (color 0 0 255))
                   (spin-alot (rectangle 12 120 "solid" (color 0 0 255 1)))
                   (triangle 120 "solid" (color 0 0 255))
                   (spin-alot (triangle 120 "solid" (color 0 0 255 1)))
                   (isosceles-triangle 120 30 "solid" (color 0 0 255))
                   (spin-alot (isosceles-triangle 120 30 "solid" (color 0 0 255 1)))]

@section{Recursive Image Functions}

It is also possible to make interesting looking shapes with little recursive functions.
For example, this function repeatedly puts white circles that grow, evenly spaced around 
the edge of the given shape:

@image-interaction[(define (swoosh image s)
                     (cond
                       [(zero? s) image]
                       [else (swoosh 
                              (overlay/align "center" "top"
                                             (circle (* s 1/2) "solid" "white")
                                             (rotate 4 image))
                              (- s 1))]))]

@image-interaction[(swoosh (circle 100 "solid" "black") 
                           94)]

More conventional fractal shapes can also be written using the image
library, e.g.:

@image-interaction[(define (sierpinski-carpet n)
                     (cond
                       [(zero? n) (square 1 "solid" "black")]
                       [else
                        (local [(define c (sierpinski-carpet (- n 1)))
                                (define i (square (image-width c) "solid" "white"))]
                          (above (beside c c c)
                                 (beside c i c)
                                 (beside c c c)))]))]

@image-interaction/bitmap[(sierpinski-carpet 5)]

We can adjust the carpet to add a little color:

@image-interaction[(define (colored-carpet colors)
                     (cond
                       [(empty? (rest colors)) 
                        (square 1 "solid" (first colors))]
                       [else
                        (local [(define c (colored-carpet (rest colors)))
                                (define i (square (image-width c) "solid" (car colors)))]
                          (above (beside c c c)
                                 (beside c i c)
                                 (beside c c c)))]))]

@image-interaction/bitmap[(colored-carpet 
                           (list (color #x33 #x00 #xff)
                                 (color #x66 #x00 #xff)
                                 (color #x99 #x00 #xff)
                                 (color #xcc #x00 #xff)
                                 (color #xff #x00 #xff)
                                 (color 255 204 0)))]

The Koch curve can be constructed by simply placing four
curves next to each other, rotated appropriately:

@image-interaction[(define (koch-curve n)
                     (cond
                       [(zero? n) (square 1 "solid" "black")]
                       [else
                        (local [(define smaller (koch-curve (- n 1)))]
                          (beside/align "bottom"
                                        smaller 
                                        (rotate 60 smaller)
                                        (rotate -60 smaller)
                                        smaller))]))
                   (koch-curve 5)]

And then put three of them together to form the Koch snowflake.

@image-interaction[(above 
                    (beside
                     (rotate 60 (koch-curve 5))
                     (rotate -60 (koch-curve 5)))
                    (flip-vertical (koch-curve 5)))]

@section[#:tag "rotate-center"]{Rotating and Image Centers}

When rotating an image, some times the image looks best when it
rotates around a point that is not the center of the image. The 
@racket[rotate] function, however, just rotates the image as
a whole, effectively rotating it around the center of its
bounding box.

For example, imagine a game where the hero is represented
as a triangle:
@image-interaction[(define (hero α) 
                     (triangle 30 "solid" (color 255 0 0 α)))
                   (hero 255)]
rotating the hero at the prompt looks reasonable:
@image-interaction[(rotate 10 (hero 255))
                   (rotate 20 (hero 255))
                   (rotate 30 (hero 255))]
but if the hero has to appear to spin in place, then it will not look
right, as you can kind of see if we use α-blending to represent
old positions of the hero:
@image-interaction[(overlay (rotate 0  (hero 255))
                            (rotate 10 (hero 125))
                            (rotate 20 (hero 100))
                            (rotate 30 (hero  75))
                            (rotate 40 (hero  50))
                            (rotate 50 (hero  25)))]
What we'd really want is for the hero to appear to rotate around
the centroid of the triangle. To achieve this effect, we can put
the hero onto a transparent circle such that the center of the whole
image lines up with the centroid of the triangle:
@image-interaction[(define (hero-on-blank α)
                     (define the-hero (hero α))
                     (define w (image-width the-hero))
                     (define h (image-height the-hero))
                     (define d (max w h))
                     (define dx (/ w 2))   (code:comment "centroid x offset")
                     (define dy (* 2/3 h)) (code:comment "centroid y offset")
                     (define blank  (circle d "solid" (color 255 255 255 0)))
                     (place-image/align the-hero (- d dx) (- d dy) "left" "top" blank))]
and now the rotating hero looks reasonable:
@image-interaction[(overlay (rotate 0  (hero-on-blank 255))
                            (rotate 10 (hero-on-blank 125))
                            (rotate 20 (hero-on-blank 100))
                            (rotate 30 (hero-on-blank  75))
                            (rotate 40 (hero-on-blank  50))
                            (rotate 50 (hero-on-blank  25)))]

@section{Image Interoperability}

Images can connect to other libraries.  Specifically:
@itemlist[@item{images are @racket[snip%] objects, so can
                be @method[text% insert]ed into @racket[text%]
                and @racket[pasteboard%] objects}
           @item{they implement the @racket[convert] protocol for @racket['png-bytes]}
           @item{they implement the @racket[pict-convert] protocol, and}
           @item{there is a low-level interface for drawing directly into
                 a @racket[dc<%>] object: @racket[render-image].}]

@section[#:tag "nitty-gritty"]{The Nitty Gritty of Pixels, Pens, and Lines}

The image library treats coordinates as if they are in the upper-left corner 
of each pixel, and infinitesimally small (unlike pixels which have some area).

Thus, when drawing a solid @racket[square] of whose side-length is 10, the image library
colors in all of the pixels enclosed by the @racket[square] starting at the upper
left corner of (0,0) and going down to the upper left corner of (10,10),
so the pixel whose upper left at (9,9) is colored in, but the pixel
at (10,10) is not. All told, 100 pixels get colored in, just as expected for
a @racket[square] with a side length of 10.

When drawing lines, however, things get a bit more complex. Specifically, 
imagine drawing the outline of that rectangle. Since the border is
between the pixels, there really isn't a natural pixel to draw to indicate
the border. Accordingly, when drawing an outline @racket[square] (without a 
@racket[pen] specification, but just a color as the last argument), 
the image library uses a pen whose width is 1 pixel, but draws a line
centered at the point (0.5,0.5) that goes down and around to the point (10.5,10.5).
This means that the outline slightly exceeds the bounding box of the shape.
Specifically, the upper and left-hand lines around the square are within
the bounding box, but the lower and right-hand lines are just outside.

This kind of rectangle is useful when putting rectangles next to each other
and avoiding extra thick lines on the interior. For example, consider
building a grid like this:

@image-interaction[
(define s1 (square 20 'outline 'black))
(define r1 (beside s1 s1 s1 s1 s1 s1))
(above  r1 r1 r1 r1 r1 r1)
]

The reason interior lines in this grid are the same thickness as the lines around the edge
is because the rectangles overlap with each other. 
That is, the upper-left rectangle's right edge is right on top of the
next rectangle's left edge.

The special case of adding 0.5 to each coordinate when drawing the square
applies to all outline polygon-based shapes that just pass color, 
but does not apply when a @racket[pen]
is passed as the last argument to create the shape.
For example, if using a pen of thickness 2 to draw a rectangle, we get a
shape that has a border drawing the row of pixels just inside and just outside
the shape. One might imagine that a pen of thickness 1 would draw an outline around the shape with
a 1 pixel thick line, but this would require 1/2 of each pixel to be illuminated, something
that is not possible. Instead, the same pixels are lit up as with the 2 pixel wide pen, but
with only 1/2 of the intensity of the color. So a 1 pixel wide black @racket[pen] object draws
a 2 pixel wide outline, but in gray.

@image-interaction[(define p1 (make-pen "black" 1 "solid" "round" "round"))]
@image-interaction/margin[2 (rectangle 20 20 "outline" p1)]

When combining pens and cropping, we can make a rectangle that has a line that is one pixel
wide, but where the line is drawn entirely within the rectangle. This rectangle has a two-pixel wide
black pen, but we can crop out the outer portion of the pen.

@image-interaction[(define p2 (make-pen "black" 2 "solid" "round" "round"))
                   (define s2 (crop 0 0 20 20 (rectangle 20 20 "outline" p2)))
                   s2]

Using that we can build a grid now too, but this grid has doubled lines on the
interior.

@image-interaction[
(define r2 (beside s2 s2 s2 s2 s2 s2))
(above  r2 r2 r2 r2 r2 r2)
]

While this kind of rectangle is not useful for building grids, it 
is important to be able to build rectangles whose drawing does not
exceed its bounding box. Specifically, this kind of drawing is used
by @racket[frame] and @racket[empty-scene] so that the extra drawn pixels
are not lost if the image is later clipped to its bounding box.

When using @racket[image->color-list] with outline shapes, the results
can be surprising for the same reasons. For example, a
2x2 black, outline rectangle consists of nine black pixels, as discussed above,
but since @racket[image->color-list] only returns the pixels that are 
within the bounding box, we see only three black pixels and one white one.

@image-interaction[(image->color-list
                    (rectangle 2 2 "outline" "black"))]

The black pixels are (most of) the upper and left edge of the outline shape,
and the one white pixel is the pixel in the middle of the shape.

@section[#:tag "nitty-gritty-alpha"]{The Nitty Gritty of Alpha Blending}

Alpha blending can cause imprecision in color comparisons resulting in
shapes that appear @racket[equal?] even though they were created with
different colors. This section explains how that happens.

To start, consider the color @racket[(make-color 1 1 1 50)].
This color is nearly the darkest shade of black, but with lots of transparency,
to it renders a light gray color on a white background, e.g.:
@image-interaction[(rectangle 100 100 "solid" (make-color 1 1 1 50))]
If the background had been green, the same rectangle would look like a darker shade of green:
@image-interaction[(overlay
                    (rectangle 100 100 "solid" (make-color 1 1 1 50))
                    (rectangle 200 200 "solid" "green"))]

Surprisingly, this shape is equal to one that (apparently) has a different
color in it:
@image-interaction[(equal? 
                    (rectangle 100 100 'solid (make-color 1 1 1 50))
                    (rectangle 100 100 'solid (make-color 2 2 2 50)))]
To understand why, we must look more carefully at how alpha blending
and image equality work. Image equality's definition is straightforward: two images
are equality if they are both drawn the same. That is, image equality
is defined by simply drawing the two shapes on a white background and
then comparing all of the pixels for the two drawings
(it is implemented more efficiently in some cases, however).

So, for those shapes to be equal, they must be drawn with the same colors.
To see what colors were actually drawn, we can use @racket[image->color-list].
Since these images use the same color in every pixel, we can examine just the first one:
@image-interaction[(first
                    (image->color-list
                     (rectangle 100 100 'solid (make-color 1 1 1 50))))
                   (first
                    (image->color-list
                     (rectangle 100 100 'solid (make-color 2 2 2 50))))]
As expected from the @racket[equal?] test, the two colors are the same, but
why should they be the same? This is where a subtle aspect of alpha blending 
and drawing comes up. In general, alpha blending works by taking the color
of any shapes below the one being drawn and then combining that color with
the new color. The precise amount of the combination is controlled by the alpha value. 
So, if a shape has an alpha value of @racket[α], then the drawing library
multiplies the new shapes color by @racket[(/ α 255)] and the existing shape's
color by @racket[(- 1 (/ α 255))] and then adds the results to get the final color.
(It does this for each of the red, green, and blue components separately.)

Going back to the two example rectangles,
the drawing library multiplies @code{50/255} by @racket[1] for the first
shape and multiplies @code{50/255} by @racket[2] for the second shape (since they
are both drawn on a white background). Then rounds them to integers, which
results in @racket[0] for both colors, making the images the same.
