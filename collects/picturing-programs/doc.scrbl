#lang scribble/manual
@(require 
  (for-label racket
             "main.rkt"
             "io-stuff.rkt"
             ; "sb-universe.rkt"
             "tiles.rkt"
             "map-image.rkt"
             2htdp/image
             teachpack/2htdp/universe
             (only-in lang/htdp-beginner check-expect)
             ))

@; teachpack["picturing-programs"]{Picturing Programs}
@title{Picturing Programs Teachpack}
@author{Stephen Bloch}

@; defmodule[installed-teachpack/picturing-programs]
@defmodule[(planet sbloch/picturing-programs)]

@section{About This Teachpack}

@;Testing, testing: @racket[(list 'testing 1 2 3)].
@;
@;This is a reference to the @racket[list] function.
@;Now a reference to @racket[triangle],
@;and @racket[big-bang],
@;and @racket[show-it],
@;and @racket[crop-top], 
@;and @racket[map-image],
@;and @racket[with-input-from-url], 
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
so if you're using this teachpack, @emph{don't} also load either of those.
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

@defproc[(name->color [name string?])
         (or/c color? false/c)]{

Given a color name like "red", "turquoise", "forest green", @italic{etc.}, returns the corresponding
color struct, showing the red, green, and blue components.  If the name isn't
recognized, returns @racket[false].}

@defproc[(get-pixel-color [x natural-number/c] [y natural-number/c] [pic image?])
         color?]{

Gets the color of a specified pixel in the given image.  If x and/or y are outside the
bounds of the image, returns black.}

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

@defproc[(build3-image [width natural-number/c] [height natural-number/c]
                       [red-function (-> natural-number/c natural-number/c natural-number/c)]
                       [green-function (-> natural-number/c natural-number/c natural-number/c)]
                       [blue-function (-> natural-number/c natural-number/c natural-number/c)])
         image?]{
A version of @racket[build-image] for students who don't know about structs yet.
Each of the three functions takes in the x and y coordinates of a pixel, and
should return an integer from 0 through 255 to determine that color component.}

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

@racketblock[
             (define (apply-gradient x y old-color)
               (make-color (min (* 3 x) 255) 0 (min (* 3 y) 255)))
             
             (map-image apply-gradient my-picture)]
produces a picture the same size and shape as @racket[my-picture],
but with a smooth color gradient with red increasing from left to
right and blue increasing from top to bottom.}

@defproc[(map3-image 
[red-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[green-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[blue-func (-> natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c natural-number/c)]
[img image?])
image?]{

A version of map-image for students who don't know about structs yet.  Each of the three given functions is assumed
to have the contract @racketblock[num(x) num(y) num(r) num(g) num(b) -> num ]
For each pixel in the original picture, applies the three
functions to the x coordinate, y coordinate, red, green, and blue components of the picture.
The result of the first function is used as the red component, the second as green, and the third as blue
in the corresponding pixel of the resulting picture.

For example,
@racketblock[
(define (zero x y r g b) 0)
(define (same-g x y r g b) g)
(define (same-b x y r g b) b)
(map3-image zero same-g same-b my-picture)]
produces a copy of @racket[my-picture] with all the red leached out,
leaving only the blue and green components.

@racketblock[
(define (3x x y r g b) (min (* 3 x) 255))
(define (3y x y r g b) (min (* 3 y) 255))
(map3-image 3x zero 3y my-picture)]
produces a picture the same size and shape as @racket[my-picture],
but with a smooth color gradient with red increasing from left to
right and blue increasing from top to bottom.}

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


@subsection{Transparency}
Some image formats support @italic{transparency}, meaning that part of the image is
ignored when layering it with other images.

@defproc[(pixel-visible? [x natural-number/c] [y natural-number/c] [pic image?]) 
         boolean?]{

Checks transparency: returns @racket[false] if the specified pixel in the image is transparent,
@racket[true] if not.}

A @deftech{maybe-color} is either a color or @racket[false], which is treated as transparent.

@defproc[(maybe-color? [thing any/c]) 
         boolean?]{

Tests whether the argument is a @tech{maybe-color}.}

@defproc[(map-masked-image [f (-> natural-number/c natural-number/c maybe-color? maybe-color?)] [pic image?])
         image?]{

Like @racket[map-image], but the function will receive @racket[false] for any transparent pixel, and
any place that it returns @racket[false] will be treated as a transparent pixel.}

@defproc[(build-masked-image [width natural-number/c] 
                             [height natural-number/c]
                             [f (-> natural-number/c natural-number/c maybe-color?)])
         image?]{

Like @racket[build-image], but any place that the function returns @racket[false] will be treated
as a transparent pixel.}

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
