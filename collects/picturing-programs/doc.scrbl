#lang scribble/doc
@(require 
  scribble/manual
  (for-label racket
             picturing-programs/main
             ;picturing-programs/io-stuff
	     ;picturing-programs/tiles
             ;picturing-programs/dummy
	     ; picturing-programs/map-image
             2htdp/image
             teachpack/2htdp/universe
             (only-in lang/htdp-beginner check-expect)
             ))

@; teachpack["picturing-programs"]{Picturing Programs}
@title{Picturing Programs Teachpack}
@author{Stephen Bloch}

@; defmodule[installed-teachpack/picturing-programs]
@defmodule[picturing-programs]

@section{About This Teachpack}

@;Testing, testing: @racket[(list 'testing 1 2 3)].
@;
@;This is a reference to the @racket[list] function (which is a nice link).
@;Now a reference to @racket[triangle] (good link),
@;and @racket[big-bang] (good link),
@;and @racket[show-it] (good link),
@;and @racket[crop-top] (underlined in red, not a link), 
@;and @racket[map-image] (underlined in red, not a link),
@;and @racket[dummyvar] (how does this look?),
@;and @racket[with-input-from-url] (underlined in red, not a link), 
@;which are defined in several different places.

Provides a variety of functions for combining and manipulating images
and running interactive animations.
It's intended to be used with the textbook
@hyperlink["http://www.picturingprograms.com" "Picturing Programs"].

@section{Installation}
This package should be bundled with DrRacket version 5.1 and later, so there should be
no installation procedure.

@section{Functions from image.rkt and universe.rkt}

This package includes all of 
@racketmodlink[2htdp/image]{the image teachpack} and
and
@racketmodlink[2htdp/universe]{the universe teachpack},
so if you're using this teachpack, @italic{don't} also load either of those.
See the above links for how to use those teachpacks.

It also supersedes the older @racket[tiles] and @racket[sb-world] teachpacks,
so if you have those, don't load them either; use this instead.

This package also provides the following additional functions:

@; @include-section{image.rkt}

@section{New image functions}

@defproc[(rotate-cw [img image?])
         image?]{Rotates an image 90 degrees clockwise.}

@defproc[(rotate-ccw [img image?])
         image?]{Rotates an image 90 degrees counterclockwise.}

@defproc[(rotate-180 [img image?])
         image?]{Rotates an image 180 degrees around its center.}

@defproc[(crop-top [img image?] [pixels natural-number/c])
         image?]{Chops off the specified number of pixels from the top of the image.}

@defproc[(crop-bottom [img image?] [pixels natural-number/c])
         image?]{Chops off the specified number of pixels from the bottom of the image.}

@defproc[(crop-left [img image?] [pixels natural-number/c]) 
         image?]{Chops off the specified number of pixels from the left side of the image.}

@defproc[(crop-right [img image?] [pixels natural-number/c]) 
         image?]{Chops off the specified number of pixels from the right side of the image.}

@defproc[(show-it [img image?]) 
         image?]{Returns the given image unaltered.  Useful as a draw handler for animations whose model is an image.}

@defproc[(reflect-vert [img image?]) 
         image?]{The same as @racket[flip-vertical]; retained for compatibility.}

@defproc[(reflect-horiz [img image?]) 
         image?]{The same as @racket[flip-horizontal]; retained for compatibility.} 

@section{Variables}
This teachpack also defines variable names for some of the pictures used in the textbook.

@defthing[pic:bloch image?]{A picture of the author, c. 2005.}
@defthing[pic:hieroglyphics image?]{A picture of a stone tablet with
hieroglyphics on it.}
@defthing[pic:hacker image?]{A picture of a student sitting at a
computer.}
@defthing[pic:book image?]{A picture of a book with a question mark.}
@defthing[pic:stick-figure image?]{A picture of a stick figure, built
from geometric primitives.}
@defthing[pic:scheme-logo image?]{A picture of a DrScheme/DrRacket
logo.}
@defthing[pic:calendar image?]{A picture of an appointment calendar.}

Note that these seven variable names happen to start with "pic:", to
distinguish them from anything you might define that happens to be named
"calendar" or "book", but you can name a variable anything you want; in
particular, there's no requirement that your names start with "pic:".

@section{Pixel functions}
The above functions allow you to operate on a picture as a whole, but sometimes
you want to manipulate a picture pixel-by-pixel.

@subsection{Colors and pixels}

Each pixel of a bitmap image has a @racket[color], a built-in structure with
four components --  red, green, blue, and alpha -- each represented by an
integer from 0 to 255.  Larger alpha values are "more opaque": an image with
alpha=255 is completely opaque, and one with alpha=0 is completely
transparent.

Even if you're not trying to get transparency effects, alpha is also used
for dithering to smooth out jagged edges.  In
@racket[(circle 50 "solid" "red")], the pixels inside the circle are pure
red, with alpha=255; the pixels outside the circle are transparent (alpha=0);
and the pixels on the boundary are red with various alpha values (for example,
if one quarter of a pixel's area is inside
the mathematical boundary of the circle, that pixel's alpha value will be
63).

@defproc[(name->color [name (or/c string? symbol?)])
         (or/c color? false/c)]{
Given a color name like "red", 'turquoise, "forest green", @italic{etc.}, returns the corresponding
color struct, showing the red, green, blue, and alpha components.  If the name isn't
recognized, returns @racket[false].}

@defproc[(colorize [thing (or/c color? string? symbol? false/c)])
	(or/c color? false/c)]{
Similar to @racket[name->color], but accepts colors and @racket[false] as
well: colors produce themselves, while @racket[false] produces a transparent
color.}

@defproc[(color=? [c1 (or/c color? string? symbol? false/c)]
		  [c2 (or/c color? string? symbol? false/c)])
	 boolean?]{
Compares two colors for equality.  As with @racket[colorize], treats
@racket[false] as a transparent color (i.e. with an alpha-component of 0).
All colors with alpha=0 are considered equal to one another, even if they have
different red, green, or blue components.}

@defproc[(get-pixel-color [x natural-number/c] [y natural-number/c] [pic image?])
         color?]{

Gets the color of a specified pixel in the given image.  If x and/or y are outside
the bounds of the image, returns a transparent color.}

@subsection{Specifying the color of each pixel of an image}
@defproc[(build-image [width natural-number/c]
                      [height natural-number/c]
                      [f (-> natural-number/c natural-number/c color?)])
         image?]{

Builds an image of the specified size and shape by calling the specified function
on the coordinates of each pixel.  For example,
@racketblock[
             (define (fuzz pic)
               (local [(define (near-pixel x y)
                         (get-pixel-color (+ x -3 (random 7))
                                          (+ y -3 (random 7))
                                          pic))]
                 (build-image (image-width pic)
                              (image-height pic)
                              near-pixel)))
                                           ]
produces a fuzzy version of the given picture by replacing each pixel with a
randomly chosen pixel near it.}

@defproc[(build4-image [width natural-number/c] [height natural-number/c]
		       [red-function (-> natural-number/c natural-number/c natural-number/c)]
		       [green-function (-> natural-number/c natural-number/c natural-number/c)]
		       [blue-function (-> natural-number/c natural-number/c natural-number/c)]
		       [alpha-function (-> natural-number/c natural-number/c
natural-number/c)])
		image?]{
A version of @racket[build-image] for students who don't know about structs yet.
Each of the four functions takes in the x and y coordinates of a pixel, and
should return an integer from 0 through 255 to determine that color component.}

@defproc[(build3-image [width natural-number/c] [height natural-number/c]
                       [red-function (-> natural-number/c natural-number/c natural-number/c)]
                       [green-function (-> natural-number/c natural-number/c natural-number/c)]
                       [blue-function (-> natural-number/c natural-number/c natural-number/c)])
         image?]{
Just like @racket[build4-image], but without specifying the alpha component
(which defaults to 255, fully opaque).}

@defproc[(map-image [f (-> natural-number/c natural-number/c color? color?)] [img image?])
         image?]{

Applies the given function to each pixel in a given image, producing a new image the same
size and shape.  For example,
@racketblock[
             (define (lose-red x y old-color)
               (make-color 0 (color-green old-color) (color-blue old-color)))
             
             (map-image lose-red my-picture)]
produces a copy of @racket[my-picture] with all the red leached out,
leaving only the blue and green components.

Since @racket[make-color] with three arguments defaults alpha to 255,
this function discards any alpha information (including edge-dithering)
that was in the original image.  To preserve this information, one could write
@racketblock[
(define (lose-red-but-not-alpha x y old-color)
  (make-color 0 (color-green old-color) (color-blue old-color) (color-alpha
old-color)))]

Another example:
@racketblock[
             (define (apply-gradient x y old-color)
               (make-color (min (* 3 x) 255)
                           0 
                           (min (* 3 y) 255)))
             
             (map-image apply-gradient my-picture)]
produces a picture the size of @racket[my-picture]'s bounding rectangle,
with a smooth color gradient with red increasing from left to
right and blue increasing from top to bottom.}

@defproc[(map4-image 
[red-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[green-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[blue-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[alpha-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[img image?])
image?]{

A version of map-image for students who don't know about structs yet.  Each of the
four given functions is assumed to have the contract
@racketblock[num(x) num(y) num(r) num(g) num(b) num(alpha) -> num]
For each pixel in the original picture, applies the four
functions to the x coordinate, y coordinate, red, green, blue, and alpha
components of the pixel.
The results of the four functions are used as the red, green, blue, and alpha
components in the corresponding pixel of the resulting picture.

For example,
@racketblock[
(define (zero x y r g b a) 0)
(define (same-g x y r g b a) g)
(define (same-b x y r g b a) b)
(define (same-alpha x y r g b a) a)
(map4-image zero same-g same-b same-alpha my-picture)]
produces a copy of @racket[my-picture] with all the red leached out,
leaving only the blue, green, and alpha components.

@racketblock[
(define (3x x y r g b a) (min (* 3 x) 255))
(define (3y x y r g b a) (min (* 3 y) 255))
(define (return-255 x y r g b a) 255)
(map4-image 3x zero 3y return-255 my-picture)]
produces an opaque picture the size of @racket[my-picture]'s bounding rectangle,
with a smooth color gradient with red increasing from left to
right and blue increasing from top to bottom.
}

@defproc[(map3-image
[red-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[green-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[blue-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[img image?])
image?]{
Like @racket[map4-image], but not specifying the alpha component.  Note that
the red, green, and blue functions also @italic{don't take in} alpha values.
Each of the three given functions is assumed to have the contract
@racketblock[num(x) num(y) num(r) num(g) num(b) -> num]
For each pixel in the original picture, applies the three functions to the x
coordinate, y coordinate, red, green, and blue components of the pixel.
The results are used as a the red, green, and blue components in the
corresponding pixel of the resulting picture.

The alpha component in the resulting picture is copied from the source
picture.  For example,
@racketblock[
(define (zero x y r g b) 0)
(define (same-g x y r g b) g)
(define (same-b x y r g b) b)
(map3-image zero same-g same-b my-picture)]
produces a copy of @racket[my-picture] with all the red leached out; parts of
the picture that were transparent are still transparent, and parts that were
dithered are still dithered.
@racketblock[
(define (3x x y r g b a) (min (* 3 x) 255))
(define (3y x y r g b a) (min (* 3 y) 255))
(map3-image zero 3x 3y my-picture)]
produces a @racket[my-picture]-shaped "window" on a color-gradient.
}

@defproc[(real->int [num real?])
         integer?]{
Not specific to colors, but useful if you're building colors by arithmetic.
For example,
@racketblock[
             (define (bad-gradient x y)
               (make-color (* 2.5 x) (* 1.6 y) 0))
             (build-image 50 30 bad-gradient)
             (define (good-gradient x y)
               (make-color (real->int (* 2.5 x)) (real->int (* 1.6 y)) 0))
             (build-image 50 30 good-gradient)
             ]
The version using @racket[bad-gradient] crashes because color components must be exact integers.
The version using @racket[good-gradient] works.}


@section{Input and Output}
This teachpack also provides several functions to help in testing
I/O functions (in Advanced Student language; ignore this section if
you're in a Beginner or Intermediate language):

@defproc[(with-input-from-string [input string?]
                                 [thunk (-> any/c)])
         any/c]{

Calls @tt{thunk}, which presumably uses @racket[read],
in such a way that @racket[read] reads from @tt{input} rather than from
the keyboard.}

@defproc[(with-output-to-string [thunk (-> any/c)])
         string?]{

Calls @tt{thunk}, which presumably uses @racket[display], @racket[print],
@racket[write], and/or @racket[printf], in such a way that its output is
accumlated into a string, which is then returned.}

@defproc[(with-input-from-file [filename string?]
           [thunk (-> any/c)]) any/c]{
Calls @tt{thunk}, which presumably uses @racket[read],
in such a way that @racket[read] reads from the specified file
rather than from the keyboard.}

@defproc[(with-output-to-file (filename string?) (thunk (-> any/c))) any/c]{
Calls @tt{thunk}, which presumably uses @racket[display], @racket[print],
@racket[write], and/or @racket[printf], in such a way that its output is
redirected into the specified file.}

@defproc[(with-input-from-url (url string?) (thunk (-> any/c))) any/c]{
Calls @tt{thunk}, which presumably uses @racket[read],
in such a way that @racket[read] reads from the HTML source of the
Web page at the specified URL rather than from the keyboard.}

@defproc[(with-io-strings (input string?) (thunk (-> any/c))) string?]{
Combines @racket[with-input-from-string] and @racket[with-output-to-string]:
calls @tt{thunk} with its input coming from @tt{input} and accumulates
its output into a string, which is returned.  Especially useful for testing:
@racketblock[
             (define (ask question)
               (begin (display question)
                      (read)))                  
             (define (greet)
               (local [(define name (ask "What is your name?"))]
                 (printf "Hello, ~a!" name)))
             (check-expect
              (with-io-strings "Steve" greet)
              "What is your name?Hello, Steve!")]
}

@; @include-section{worlds.scrbl}

@; @include-section{universes.scrbl}
