#lang scribble/doc

@begin[(require
	 (lib "manual.ss" "scribble"))
       
       (require-for-label 
	 (lib "lang.ss" "big")
	 "../image.ss")]

@title[#:tag "image"]{Manipulating Images: image.ss}

The  teachpack provides primitives for constructing and manipulating
images. Basic images are created as outlines or solid shapes. Additional
primitives allow for the composition of images.

;; {Mode} is one of the following two symbols or strings: 

;; -- @scheme['solid]

;; -- @scheme['outline]

;; -- @scheme["solid"]

;; -- @scheme["outline"]

Interpretation: @scheme['solid] is used for creating solid basic
  shapes; @scheme['outline] is used for creating outlines of basic
  shapes. Strings are used in an analogous manner. 

@scheme[(define-struct color (red blue green))]

;; A [CS] is a structure: @scheme[(make-color N N N)]

;; where N is between 0 and 255. 

;; [Color] is one of:

;; -- a color symbol, e.g., @scheme['blue]

;; -- a color string, e.g., @scheme["blue"]

;; -- a CS, e.g., @scheme[(make-color 0 0 255)], which also denotes blue. 

;; Interpretation: @scheme[Color] arguments are used to paint the shapes

;; or their outlines. See below for more information about color structs.

The following predicate specifies what a valid image color is: 

@defproc[(image-color? [x any]) boolean?]{
 Determines if the input is a valid image color.}

@section[#:tag "creational"]{Creating Basic Shapes}

@defproc[(rectangle [w number?] [h number?] [m Mode] [c Color]) image?]{
 Creates a @scheme[w] by @scheme[h] rectangle, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(circle [r number?] [m Mode] [c Color]) image?]{
 Creates a circle or disk of radius @scheme[r], filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(ellipse [w number?] [h number?] [m Mode] [c Color]) image?]{
 Creates a @scheme[w] by @scheme[h] ellipse, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(triangle [s number?] [m Mode] [c Color]) image?]{ 
 Creates an upward pointing equilateral
 triangle whose side is @scheme[s] pixels long, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(star [n (and/c number? (>=/c 2))]
	       [outer (and/c number? (>=/c 1))]
	       [inner (and/c number? (>=/c 1))]
	       [m Mode]
	       [c Color]) image?]{
 Creates a multi-pointed star with @scheme[n] points, an @scheme[outer]
 radius for the max distance of the points to the center, and
 an @scheme[inner] radius for the min distance to the center. }

@defproc[(line [x number?][y number?] [c Color]) image?]{
 Creates a line colored @scheme[c] from (0,0) to @scheme[(x,y)].
 See @scheme[add-line] below.
}

@defproc[(text [s string?] [f (and/c number? positive?)] [c Color]) Image]{
 Creates an image of the text @scheme[s] at point size @scheme[f] 
 and painted in color @scheme[c].}

@section[#:tag "composition"]{Composing Shapes}

@defproc[(add-line [i image?] 
		   [x number?]
		   [y number?]
		   [z number?]
		   [u number?]
		   [c Color]) image?]{
 Add a line from @scheme[(x,y)] to @scheme[(z,u)] to image @scheme[i] and
		   paints it color @scheme[c].}

@defproc[(overlay [img image?] [img2 image?] [img* image?] ...) image?]{
 Overlays all images on their pinhole (see below).
}
