#lang scribble/doc

@(require (for-label (only-in racket/contract and/c or/c any/c not/c)
                     2htdp/image
                     (except-in lang/htdp-beginner posn make-posn posn? posn-x posn-y image?)
                     lang/posn
                     (except-in racket/gui/base make-color make-pen)
                     (only-in racket/base path-string?))
          lang/posn
          "shared.rkt"
          "img-eval.rkt"
          scribble/decode
          scribble/manual)

@(require scribble/eval)

@(define img-eval (make-img-eval))

@(define-syntax-rule 
   (image-examples exp ...)
   (examples #:eval img-eval exp ...))

@(define-syntax-rule 
   (image-interaction exp ...)
   (interaction #:eval img-eval exp ...))

@(define-syntax-rule
   (image-interaction/margin num exp)
   (begin
     (racketinput exp)
     (img-eval '(extra-margin num))
     (interaction-eval-show #:eval img-eval exp)
     (img-eval '(extra-margin 0))))
     

@teachpack["image"]{Images}

@(define mode/color-and-nitty-text
   (make-splice
    @list{
     Note that when the @racket[mode] is @racket['outline] or @racket["outline"], the shape
     may draw outside of its bounding box and thus parts of the image may disappear
     when it is cropped. See @secref["nitty-gritty"] (in the @seclink["image-guide"])
     for a more careful explanation of the ramifications of this fact.

     If the @racket[_mode] argument is @racket['outline] or @racket["outline"], then the last
     argument can be a @racket[pen] struct or an @racket[image-color?], but if the @racket[_mode]
     is @racket['solid] or @racket["solid"], then the last argument must be an
     @racket[image-color?].}))

@(define crop-warning
   (make-splice
    @list{Some shapes (notably those with @racket['outline] or @racket["outline"] as
          the @racket[_mode] argument) draw outside of their bounding boxes and thus
          cropping them may remove part of them (often the lower-left and lower-right
          edges). See @secref["nitty-gritty"] (in the @seclink["image-guide"])
          for a more careful discussion of this issue.}))

@defmodule[#:require-form beginner-require 2htdp/image]

The image teachpack provides a number of basic image construction functions, along with
combinators for building more complex images out of existing images. Basic images include
various polygons, ellipses and circles, and text, as well as bitmaps (typically bitmaps 
come about via the @onscreen{Insert Image...} menu item in DrRacket).
Existing images can be rotated, scaled, flipped, and overlaid on top of each other.

@section{Basic Images}

@defproc*[([(circle [radius (and/c real? (not/c negative?))]
                    [mode mode?]
                    [color image-color?])
            image?]
           [(circle [radius (and/c real? (not/c negative?))]
                    [outline-mode (or/c 'outline "outline")]
                    [pen-or-color (or/c pen? image-color?)])
            image?])]{
  Constructs a circle with the given radius, height, mode, and color.
  
  @mode/color-and-nitty-text
  
   @image-examples[(circle 30 "outline" "red")
                   (circle 20 "solid" "blue")
                   (circle 20 100 "blue")]

}

@defproc*[([(ellipse [width (and/c real? (not/c negative?))]
                     [height (and/c real? (not/c negative?))]
                     [mode mode?] 
                     [color image-color?])
            image?]
           [(ellipse [width (and/c real? (not/c negative?))]
                     [height (and/c real? (not/c negative?))]
                     [mode (or/c 'outline "outline")] 
                     [pen-or-color (or/c image-color? pen?)])
            image?])]{
  Constructs an ellipse with the given width, height, mode, and color.

  @mode/color-and-nitty-text
  
  @image-examples[(ellipse 60 30 "outline" "black")
                  (ellipse 30 60 "solid" "blue")
                  (ellipse 30 60 100 "blue")] 
}

@defproc[(line [x1 real?] [y1 real?] [pen-or-color (or/c pen? image-color?)]) image?]{
  Constructs an image representing a line segment that connects the points
  (0,0) to (x1,y1).
  
  @image-examples[(line 30 30 "black")
                  (line -30 20 "red")
                  (line 30 -20 "red")]
}

@defproc[(add-line [image image?]
                   [x1 real?] [y1 real?]
                   [x2 real?] [y2 real?]
                   [pen-or-color (or/c pen? image-color?)])
         image?]{

  Adds a line to the image @racket[image], starting from the point (@racket[x1],@racket[y1])
  and going to the point (@racket[x2],@racket[y2]).
  Unlike @racket[scene+line], if the line passes outside of @racket[image], the image
  gets larger to accommodate the line.
  
  @image-examples[(add-line (ellipse 40 40 "outline" "maroon")
                            0 40 40 0 "maroon")
                  (add-line (rectangle 40 40 "solid" "gray")
                            -10 50 50 -10 "maroon")
                 (add-line
                   (rectangle 100 100 "solid" "darkolivegreen")
                   25 25 75 75 
                   (make-pen "goldenrod" 30 "solid" "round" "round"))]
}

@defproc[(add-curve [image image?] 
                    [x1 real?] [y1 real?] [angle1 angle?] [pull1 real?]
                    [x2 real?] [y2 real?] [angle2 angle?] [pull2 real?]
                    [pen-or-color (or/c pen? image-color?)])
         image?]{

Adds a curve to @racket[image], starting at the point
(@racket[x1],@racket[y1]), and ending at the point
(@racket[x2],@racket[y2]).

The @racket[angle1] and @racket[angle2] arguments specify the 
angle that the curve has as it leaves the initial point and
as it reaches the final point, respectively. 

The @racket[pull1] and @racket[pull2] arguments control how
long the curve tries to stay with that angle. Larger numbers
mean that the curve stays with the angle longer.

Unlike @racket[scene+curve], if the line passes outside of @racket[image], the image
  gets larger to accommodate the curve.


  @image-examples[(add-curve (rectangle 100 100 "solid" "black")
                             20 20 0 1/3
                             80 80 0 1/3
                             "white")
                  (add-curve (rectangle 100 100 "solid" "black")
                             20 20 0 1 
                             80 80 0 1
                             "white")
                  (add-curve 
                   (add-curve 
                    (rectangle 40 100 "solid" "black")
                    20 10 180 1/2
                    20 90 180 1/2
                    (make-pen "white" 4 "solid" "round" "round"))
                   20 10 0 1/2
                   20 90 0 1/2
                   (make-pen "white" 4 "solid" "round" "round"))
                  
                  (add-curve (rectangle 100 100 "solid" "black")
                             -20 -20 0 1 
                             120 120 0 1
                             "red")]
}

@defproc[(text [string string?] [font-size (and/c integer? (<=/c 1 255))] [color image-color?])
         image?]{
                
  Constructs an image that draws the given string, using the font size and color.
                 
  @image-examples[(text "Hello" 24 "olive")
                  (text "Goodbye" 36 "indigo")]
}

@defproc[(text/font [string string?] [font-size (and/c integer? (<=/c 1 255))] [color image-color?]
                    [face (or/c string? #f)]
                    [family (or/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system)]
                    [style (or/c 'normal 'italic 'slant)]
                    [weight (or/c 'normal 'bold 'light)]
                    [underline? any/c])
         image?]{
                
  Constructs an image that draws the given string, using a complete font specification.
  
  The @racket[face] and the @racket[family] combine to give the complete typeface. If 
  @racket[face] is available on the system, it is used, but if not then a default typeface
  based on the @racket[family] is chosen. The @racket[style] controls if the face is italic
  or not (on Windows and Mac OS X, @racket['slant] and @racket['italic] are the same),
  the @racket[weight] controls if it is boldface (or light), and @racket[underline?]
  determines if the face is underlined. For more details on these arguments, see @racket[font%],
  which ultimately is what this code uses to draw the font.
                 
  @image-examples[(text/font "Hello" 24 "olive"
                             "Gill Sans" 'swiss 'normal 'bold #f)
                  (text/font "Goodbye" 18 "indigo"
                             #f 'modern 'italic 'normal #f)
                  (text/font "not really a link" 18 "blue"
                             #f 'roman 'normal 'normal #t)]
}

@defthing[empty-image image?]{
  The empty image. Its width and height are both zero and it does not draw at all.
  
  @image-examples[(image-width empty-image)
                  (equal? (above empty-image
                                 (rectangle 10 10 "solid" "red"))
                          (beside empty-image
                                  (rectangle 10 10 "solid" "red")))]
  
  
  In most cases, combining an image with @racket[empty-image] produces the
  original image (as shown in the above example). In some situations,
  however, the combination can cause the resulting pict to have a different
  baseline (see @racket[image-baseline]) and thus not
  be equal.
  
  @image-examples[(image-baseline (above (text "Hello" 24 "olive") empty-image))
                  (image-baseline (text "Hello" 24 "olive"))
                  (equal? (above (text "Hello" 24 "olive") empty-image)
                          (text "Hello" 24 "olive"))]
  
  }

@section{Polygons}

@defproc*[([(triangle [side-length (and/c real? (not/c negative?))] 
                      [mode mode?]
                      [color image-color?])
            image?]
           [(triangle [side-length (and/c real? (not/c negative?))] 
                      [outline-mode (or/c 'outline "outline")]
                      [pen-or-color (or/c pen? image-color?)])
            image?])]{

   Constructs a upward-pointing equilateral triangle. 
  The @racket[side-length] argument 
  determines the 
  length of the side of the triangle.

    @mode/color-and-nitty-text
  
    @image-examples[(triangle 40 "solid" "tan")]

}

@defproc*[([(right-triangle [side-length1 (and/c real? (not/c negative?))]
                            [side-length2 (and/c real? (not/c negative?))]
                            [mode mode?]
                            [color image-color?])
            image?]
           [(right-triangle [side-length1 (and/c real? (not/c negative?))]
                            [side-length2 (and/c real? (not/c negative?))]
                            [outline-mode (or/c 'outline "outline")]
                            [pen-or-color (or/c pen? image-color?)])
            image?])]{
                 
  Constructs a triangle with a right angle where the two sides adjacent
  to the right angle have lengths @racket[side-length1] and @racket[side-length2].

  @mode/color-and-nitty-text
  
  @image-examples[(right-triangle 36 48 "solid" "black")]
}

@defproc*[([(isosceles-triangle [side-length (and/c real? (not/c negative?))] 
                                [angle angle?]
                                [mode mode?]
                                [color image-color?])
            image?]
           [(isosceles-triangle [side-length (and/c real? (not/c negative?))] 
                                [angle angle?]
                                [outline-mode (or/c 'outline "outline")]
                                [pen-or-color (or/c pen? image-color?)])
            image?])]{

 Creates a triangle with two equal-length sides, of length @racket[side-length]
 where the angle between those sides is @racket[angle]. The third
 leg is straight, horizontally. If the angle is less than 
 @racket[180], then the triangle will point up and if the @racket[angle]
 is more, then the triangle will point down. 
 
 @mode/color-and-nitty-text
 
 @image-examples[(isosceles-triangle 200 170 "solid" "seagreen")
                 (isosceles-triangle 60 30 "solid" "aquamarine")
                 (isosceles-triangle 60 330 "solid" "lightseagreen")]
}

                     
To create a triangle given known sides and angles, the following
family of functions are useful:
@itemlist[@item{@racket[triangle/sss], if all three sides are known}
          @item{@racket[triangle/ass], 
                @racket[triangle/sas], or
                @racket[triangle/ssa], 
                if two sides and their included angle are known}
          @item{@racket[triangle/aas], 
                @racket[triangle/asa], or 
                @racket[triangle/saa], 
                if two angles and their shared side are known.}]
                     

They all construct a triangle oriented as follows:

@image["triangle-xxx.png"]

@defproc*[([(triangle/sss [side-length-a (and/c real? (not/c negative?))] 
                          [side-length-b (and/c real? (not/c negative?))] 
                          [side-length-c (and/c real? (not/c negative?))] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/sss [side-length-a (and/c real? (not/c negative?))]
                          [side-length-b (and/c real? (not/c negative?))]
                          [side-length-c (and/c real? (not/c negative?))]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the side lengths a, b, and, c are given by @racket[side-length-a],
 @racket[side-length-b], and, @racket[side-length-c] respectively.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/sss 40 60 80 "solid" "seagreen")
                 (triangle/sss 80 40 60 "solid" "aquamarine")
                 (triangle/sss 80 80 40 "solid" "lightseagreen")]
}

@defproc*[([(triangle/ass [angle-a angle?] 
                          [side-length-b (and/c real? (not/c negative?))] 
                          [side-length-c (and/c real? (not/c negative?))] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/ass [angle-a angle?]
                          [side-length-b (and/c real? (not/c negative?))]
                          [side-length-c (and/c real? (not/c negative?))]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the angle A and side length a and b, are given by @racket[angle-a],
 @racket[side-length-b], and, @racket[side-length-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/ass 10  60 100 "solid" "seagreen")
                 (triangle/ass 90  60 100 "solid" "aquamarine")
                 (triangle/ass 130 60 100 "solid" "lightseagreen")]
}

@defproc*[([(triangle/sas [side-length-a (and/c real? (not/c negative?))] 
                          [angle-b angle?] 
                          [side-length-c (and/c real? (not/c negative?))] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/sas [side-length-a (and/c real? (not/c negative?))]
                          [angle-b angle?]
                          [side-length-c (and/c real? (not/c negative?))]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the side length a, angle B, and, side length c given by @racket[side-length-a],
 @racket[angle-b], and, @racket[side-length-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/sas 60  10 100 "solid" "seagreen")
                 (triangle/sas 60  90 100 "solid" "aquamarine")
                 (triangle/sas 60 130 100 "solid" "lightseagreen")]
}

@defproc*[([(triangle/ssa [side-length-a (and/c real? (not/c negative?))] 
                          [side-length-b (and/c real? (not/c negative?))] 
                          [angle-c angle?] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/ssa [side-length-a (and/c real? (not/c negative?))]
                          [side-length-b (and/c real? (not/c negative?))]
                          [angle-c angle?]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the side length a, side length b, and, angle c given by @racket[side-length-a],
 @racket[side-length-b], and, @racket[angle-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/ssa 60 100  10 "solid" "seagreen")
                 (triangle/ssa 60 100  90 "solid" "aquamarine")
                 (triangle/ssa 60 100 130 "solid" "lightseagreen")]
}
@defproc*[([(triangle/aas [angle-a angle?] 
                          [angle-b angle?] 
                          [side-length-c (and/c real? (not/c negative?))] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/aas [angle-a angle?]
                          [angle-b angle?]
                          [side-length-c (and/c real? (not/c negative?))]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the angle A, angle B, and, side length c given by @racket[angle-a],
 @racket[angle-b], and, @racket[side-length-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/aas  10 40 200 "solid" "seagreen")
                 (triangle/aas  90 40 200 "solid" "aquamarine")
                 (triangle/aas 130 40 40  "solid" "lightseagreen")]
}

@defproc*[([(triangle/asa [angle-a angle?] 
                          [side-length-b (and/c real? (not/c negative?))] 
                          [angle-c angle?] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/asa [angle-a angle?]
                          [side-length-b (and/c real? (not/c negative?))]
                          [angle-c angle?]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the angle A, side length b, and, angle C given by @racket[angle-a],
 @racket[side-length-b], and, @racket[angle-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/asa  10 200 40 "solid" "seagreen")
                 (triangle/asa  90 200 40 "solid" "aquamarine")
                 (triangle/asa 130 40  40 "solid" "lightseagreen")]
}

@defproc*[([(triangle/saa [side-length-a (and/c real? (not/c negative?))] 
                          [angle-b angle?] 
                          [angle-c angle?] 
                          [mode mode?]
                          [color image-color?])
            image?]
           [(triangle/saa [side-length-a (and/c real? (not/c negative?))]
                          [angle-b angle?]
                          [angle-c angle?]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 Creates a triangle where the side length a, angle B, and, angle C given by @racket[side-length-a],
 @racket[angle-b], and, @racket[angle-c] respectively.
 See above for a diagram showing where which sides and which angles are which.
 
 @mode/color-and-nitty-text
 
 @image-examples[(triangle/saa 200  10 40 "solid" "seagreen")
                 (triangle/saa 200  90 40 "solid" "aquamarine")
                 (triangle/saa 40  130 40 "solid" "lightseagreen")]
}


@defproc*[([(square [side-len (and/c real? (not/c negative?))]
                    [mode mode?]
                    [color image-color?])
            image?]
           [(square [side-len (and/c real? (not/c negative?))]
                    [outline-mode (or/c 'outline "outline")]
                    [pen-or-color (or/c pen? image-color?)])
            image?])]{

 Constructs a square.
 
 @mode/color-and-nitty-text
 
 @image-examples[(square 40 "solid" "slateblue")
                 (square 50 "outline" "darkmagenta")]

}

@defproc*[([(rectangle [width real?]
                       [height real?]
                       [mode mode?]
                       [color image-color?])
            image?]
           [(rectangle [width real?] 
                       [height real?] 
                       [outline-mode (or/c 'outline "outline")] 
                       [pen-or-color (or/c pen? image-color?)])
            image?])]{
  Constructs a rectangle with the given width, height, mode, and color.
  
  @mode/color-and-nitty-text
  
  @image-examples[(rectangle 40 20 "outline" "black")
                  (rectangle 20 40 "solid" "blue")]
}

@defproc*[([(rhombus [side-length (and/c real? (not/c negative?))]
                     [angle angle?]
                     [mode mode?]
                     [color image-color?])
            image?]
           [(rhombus [side-length (and/c real? (not/c negative?))]
                     [angle angle?]
                     [outline-mode (or/c 'outline "outline")]
                     [pen-or-color (or/c pen? image-color?)])
            image?])]{
                 
Constructs a four sided polygon with all equal sides and thus where opposite angles are equal to each
other. The top and bottom pair of angles is @racket[angle] and the left and right are @racket[(- 180 angle)].

@mode/color-and-nitty-text

@image-examples[(rhombus 40 45 "solid" "magenta")
                (rhombus 80 150 "solid" "mediumpurple")]
}

@defproc*[([(star [side-length (and/c real? (not/c negative?))] 
                  [mode mode?]
                  [color image-color?])
            image?]
           [(star [side-length (and/c real? (not/c negative?))] 
                  [outline-mode (or/c 'outline "outline")]
                  [color (or/c pen? image-color?)])
            image?])]{
  Constructs a star with five points. The @racket[side-length] argument 
  determines the side length of the enclosing pentagon.

  @mode/color-and-nitty-text

  @image-examples[(star 40 "solid" "gray")]
  
}

@defproc*[([(star-polygon [side-length (and/c real? (not/c negative?))]
                          [side-count side-count?]
                          [step-count step-count?]
                          [mode mode?]
                          [color image-color?])
            image?]
           [(star-polygon [side-length (and/c real? (not/c negative?))]
                          [side-count side-count?]
                          [step-count step-count?]
                          [outline-mode (or/c 'outline "outline")]
                          [pen-or-color (or/c pen? image-color?)])
            image?])]{
 
  Constructs an arbitrary regular star polygon (a generalization of the regular polygons). 
  The polygon is enclosed by a regular polygon with @racket[side-count] sides each
  @racket[side-length] long. The polygon is actually constructed by going from vertex to
  vertex around the regular polgon, but connecting every @racket[step-count]-th vertex
  (i.e., skipping every @racket[(- step-count 1)] verticies).
  
  For example, if @racket[side-count] is @racket[5] and @racket[step-count] is @racket[2],
  then this function produces a shape just like @racket[star].
  
  @mode/color-and-nitty-text

  @image-examples[(star-polygon 40 5 2 "solid" "seagreen")
                  (star-polygon 40 7 3 "outline" "darkred")
                  (star-polygon 20 10 3 "solid" "cornflowerblue")]
 
}
                
@defproc*[([(radial-star [point-count (and/c integer? (>=/c 2))]
                         [inner-radius (and/c real? (not/c negative?))]
                         [outer-radius (and/c real? (not/c negative?))]
                         [mode mode?]
                         [color image-color?])
            image?]
           [(radial-star [point-count (and/c integer? (>=/c 2))]
                         [inner-radius (and/c real? (not/c negative?))]
                         [outer-radius (and/c real? (not/c negative?))]
                         [outline-mode (or/c 'outline "outline")]
                         [pen-or-color (or/c pen? image-color?)])
            image?])]{

Constructs a star-like polygon where the star is specified by two radii and a number of points. 
The first radius determines where the points begin, the second determines where they end, and
the @racket[point-count] argument determines how many points the star has.

@image-examples[(radial-star 8 8 64 "solid" "darkslategray")
                (radial-star 32 30 40 "outline" "black")]

}

@defproc*[([(regular-polygon [side-length (and/c real? (not/c negative?))] 
                             [side-count side-count?]
                             [mode mode?]
                             [color image-color?])
            image?]
           [(regular-polygon [side-length (and/c real? (not/c negative?))] 
                             [side-count side-count?]
                             [outline-mode (or/c 'outline "outline")]
                             [pen-or-color (or/c pen? image-color?)])
            image?])]{
  Constructs a regular polygon with @racket[side-count] sides.

  @mode/color-and-nitty-text

  @image-examples[(regular-polygon 50 3 "outline" "red")
                  (regular-polygon 40 4 "outline" "blue")
                  (regular-polygon 20 8 "solid" "red")]
}


@defproc*[([(polygon [vertices (listof real-valued-posn?)] 
                     [mode mode?]
                     [color image-color?])
            image?]
           [(polygon [vertices (listof real-valued-posn?)] 
                     [outline-mode (or/c 'outline "outline")]
                     [pen-or-color (or/c pen? image-color?)])
            image?])]{
  Constructs a polygon connecting the given vertices.
  
  @mode/color-and-nitty-text
  
  @image-examples[(polygon (list (make-posn 0 0)
                                 (make-posn -10 20)
                                 (make-posn 60 0)
                                 (make-posn -10 -20))
                           "solid" 
                           "burlywood")
                  (polygon (list (make-posn 0 0)
                                 (make-posn 0 40)
                                 (make-posn 20 40)
                                 (make-posn 20 60)
                                 (make-posn 40 60)
                                 (make-posn 40 20)
                                 (make-posn 20 20)
                                 (make-posn 20 0))
                           "solid" 
                           "plum")
                  (underlay
                   (rectangle 80 80 "solid" "mediumseagreen")
                   (polygon
                    (list (make-posn 0 0)
                          (make-posn 50 0)
                          (make-posn 0 50)
                          (make-posn 50 50))
                    "outline"
                    (make-pen "darkslategray" 10 "solid" "round" "round")))
                  
                  (underlay
                   (rectangle 90 80 "solid" "mediumseagreen")
                   (polygon 
                    (list (make-posn 0 0)
                          (make-posn 50 0)
                          (make-posn 0 50)
                          (make-posn 50 50))
                    "outline"
                    (make-pen "darkslategray" 10 "solid" "projecting" "miter")))]
}

@section{Overlaying Images}

@defproc[(overlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its arguments building a single image. The first argument goes
  on top of the second argument, which goes on top of the third argument, etc.
  The images are all lined up on their centers.

  @image-examples[(overlay (rectangle 30 60 "solid" "orange")
                           (ellipse 60 30 "solid" "purple"))
                  (overlay (ellipse 10 10 "solid" "red")
                           (ellipse 20 20 "solid" "black")
                           (ellipse 30 30 "solid" "red")
                           (ellipse 40 40 "solid" "black")
                           (ellipse 50 50 "solid" "red")
                           (ellipse 60 60 "solid" "black"))
                  (overlay (regular-polygon 20 5 "solid" (make-color  50  50 255))
                           (regular-polygon 26 5 "solid" (make-color 100 100 255))
                           (regular-polygon 32 5 "solid" (make-color 150 150 255))
                           (regular-polygon 38 5 "solid" (make-color 200 200 255))
                           (regular-polygon 44 5 "solid" (make-color 250 250 255)))]
  
  }

@defproc[(overlay/align [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its image arguments, much like the @racket[overlay] function, but using
  @racket[x-place] and @racket[y-place] to determine where the images are lined up. For example, if
  @racket[x-place] and @racket[y-place] are both @racket["middle"], then the images are lined up
  on their centers.

  @image-examples[(overlay/align "left" "middle"
                                 (rectangle 30 60 "solid" "orange")
                                 (ellipse 60 30 "solid" "purple"))
                  (overlay/align "right" "bottom"
                                 (rectangle 20 20 "solid" "silver")
                                 (rectangle 30 30 "solid" "seagreen")
                                 (rectangle 40 40 "solid" "silver")
                                 (rectangle 50 50 "solid" "seagreen"))]

  }

@defproc[(overlay/offset [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Just like @racket[overlay], this function lines up its image arguments on top of
  each other. Unlike @racket[overlay], it moves @racket[i2] by @racket[x] pixels to
  the right and @racket[y] down before overlaying them.
  
  @image-examples[(overlay/offset (circle 40 "solid" "red")
                                  10 10
                                  (circle 40 "solid" "blue"))
                  
                  (overlay/offset (overlay/offset (rectangle 60 20 "solid" "black")
                                                  -50 0
                                                  (circle 20 "solid" "darkorange"))
                                  70 0
                                  (circle 20 "solid" "darkorange"))
                  (overlay/offset
                   (overlay/offset (circle 30 'solid (color 0 150 0 127))
                                   26 0
                                   (circle 30 'solid (color 0 0 255 127)))
                   0 26
                   (circle 30 'solid (color 200 0 0 127)))]
}

@defproc[(overlay/align/offset [x-place x-place?] [y-place y-place?] [i1 image?] [x real?] [y real?] [i2 image?])
         image?]{
  Overlays image @racket[i1] on top of @racket[i2], using @racket[x-place] and @racket[y-place] as the 
  starting points for the overlaying, and then adjusts @racket[i2] by @racket[x] to the right and
  @racket[y] pixels down. 
  
  This function combines the capabilities of @racket[overlay/align] and @racket[overlay/offset].
  
  @image-examples[(overlay/align/offset
                   "right" "bottom"
                   (star-polygon 20 20 3 "solid" "navy")
                   10 10
                   (circle 30 "solid" "cornflowerblue"))
                  (overlay/align/offset
                   "left" "bottom"
                   (star-polygon 20 20 3 "solid" "navy")
                   -10 10
                   (circle 30 "solid" "cornflowerblue"))]
  
}

@defproc[(overlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by overlaying @racket[i1] on top of @racket[i2].
  The images are initially lined up on their upper-left corners and 
  then @racket[i2] is shifted to the right 
  by @racket[x] pixels to and down by @racket[y] pixels.
  
  This is the same as @racket[(underlay/xy i2 (- x) (- y) i1)].

  See also @racket[overlay/offset] and @racket[underlay/offset].

  @image-examples[(overlay/xy (rectangle 20 20 "outline" "black")
                              20 0
                              (rectangle 20 20 "outline" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              10 10
                              (rectangle 20 20 "solid" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              -10 -10
                              (rectangle 20 20 "solid" "black"))
                  (overlay/xy 
                   (overlay/xy (ellipse 40 40 "outline" "black")
                               10
                               15
                               (ellipse 10 10 "solid" "forestgreen"))
                   20
                   15
                   (ellipse 10 10 "solid" "forestgreen"))]
}

@defproc[(underlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Underlays all of its arguments building a single image.
  
  It behaves like @racket[overlay], but with the arguments in the reverse order.
  That is, the first argument goes
  underneath of the second argument, which goes underneath the third argument, etc.
  The images are all lined up on their centers.

  @image-examples[(underlay (rectangle 30 60 "solid" "orange")
                            (ellipse 60 30 "solid" "purple"))
                  (underlay (ellipse 10 60 "solid" "red")
                            (ellipse 20 50 "solid" "black")
                            (ellipse 30 40 "solid" "red")
                            (ellipse 40 30 "solid" "black")
                            (ellipse 50 20 "solid" "red")
                            (ellipse 60 10 "solid" "black"))
                  (underlay (ellipse 10 60 40 "red")
                            (ellipse 20 50 40 "red")
                            (ellipse 30 40 40 "red")
                            (ellipse 40 30 40 "red")
                            (ellipse 50 20 40 "red")
                            (ellipse 60 10 40 "red"))]
  
  }

@defproc[(underlay/align [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Underlays all of its image arguments, much like the @racket[underlay] function, but using
  @racket[x-place] and @racket[y-place] to determine where the images are lined up. For example, if
  @racket[x-place] and @racket[y-place] are both @racket["middle"], then the images are lined up
  on their centers.

  @image-examples[(underlay/align "left" "middle"
                                  (rectangle 30 60 "solid" "orange")
                                  (ellipse 60 30 "solid" "purple"))
                  (underlay/align "right" "top"
                                  (rectangle 50 50 "solid" "seagreen")
                                  (rectangle 40 40 "solid" "silver")
                                  (rectangle 30 30 "solid" "seagreen")
                                  (rectangle 20 20 "solid" "silver"))
                  (underlay/align "left" "middle"
                                  (rectangle 50 50 50 "seagreen")
                                  (rectangle 40 40 50 "seagreen")
                                  (rectangle 30 30 50 "seagreen")
                                  (rectangle 20 20 50 "seagreen"))]

  }


@defproc[(underlay/offset [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Just like @racket[underlay], this function lines up its first image argument
  underneath the second. Unlike @racket[underlay], it moves @racket[i2] by
  @racket[x] pixels to the right and @racket[y] down before underlaying them.
  
  @image-examples[(underlay/offset (circle 40 "solid" "red")
                                  10 10
                                  (circle 40 "solid" "blue"))
                  
                  (underlay/offset (circle 40 "solid" "gray")
                                   0 -10
                                   (underlay/offset (circle 10 "solid" "navy")
                                                   -30 0
                                                   (circle 10 "solid" "navy")))]
}

@defproc[(underlay/align/offset [x-place x-place?] [y-place y-place?] [i1 image?] [x real?] [y real?] [i2 image?])
         image?]{
  Underlays image @racket[i1] underneath @racket[i2], using @racket[x-place] and @racket[y-place] as the 
  starting points for the combination, and then adjusts @racket[i2] by @racket[x] to the right and
  @racket[y] pixels down. 
  
  This function combines the capabilities of @racket[underlay/align] and @racket[underlay/offset].
  
  @image-examples[(underlay/align/offset
                   "right" "bottom"
                   (star-polygon 20 20 3 "solid" "navy")
                   10 10
                   (circle 30 "solid" "cornflowerblue"))
                  (underlay/align/offset
                   "right" "bottom"
                   (underlay/align/offset
                    "left" "bottom"
                    (underlay/align/offset
                     "right" "top"
                     (underlay/align/offset
                      "left" "top"
                      (rhombus 120 90 "solid" "navy")
                      16 16
                      (star-polygon 20 11 3 "solid" "cornflowerblue"))
                     -16 16
                     (star-polygon 20 11 3 "solid" "cornflowerblue"))
                    16 -16
                    (star-polygon 20 11 3 "solid" "cornflowerblue"))
                   -16 -16
                   (star-polygon 20 11 3 "solid" "cornflowerblue"))]
  
}

@defproc[(underlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by underlaying @racket[i1] underneath @racket[i2].
  The images are initially lined up on their upper-left corners and 
  then @racket[i2] is shifted to the right 
  by @racket[x] pixels to and down by @racket[y] pixels.
  
  This is the same as @racket[(overlay/xy i2 (- x) (- y) i1)].
  
  See also @racket[underlay/offset] and @racket[overlay/offset].

  @image-examples[(underlay/xy (rectangle 20 20 "outline" "black")
                               20 0
                               (rectangle 20 20 "outline" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               10 10
                               (rectangle 20 20 "solid" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               -10 -10
                               (rectangle 20 20 "solid" "black"))
                  (underlay/xy 
                   (underlay/xy (ellipse 40 40 "solid" "gray")
                                10
                                15
                                (ellipse 10 10 "solid" "forestgreen"))
                   20
                   15
                   (ellipse 10 10 "solid" "forestgreen"))]
}


@defproc[(beside [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  horizontal row, aligned along their centers.

  @image-examples[(beside (ellipse 20 70 "solid" "gray")
                          (ellipse 20 50 "solid" "darkgray")
                          (ellipse 20 30 "solid" "dimgray")
                          (ellipse 20 10 "solid" "black"))]

  }

@defproc[(beside/align [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a horizontal row, lined
  up as indicated by the @racket[y-place] argument. For example, if @racket[y-place]
  is @racket["middle"], then the images are placed side by side with their centers 
  lined up with each other.

  @image-examples[(beside/align "bottom"
                                (ellipse 20 70 "solid" "lightsteelblue")
                                (ellipse 20 50 "solid" "mediumslateblue")
                                (ellipse 20 30 "solid" "slateblue")
                                (ellipse 20 10 "solid" "navy"))

                  (beside/align "top"
                                (ellipse 20 70 "solid" "mediumorchid")
                                (ellipse 20 50 "solid" "darkorchid")
                                (ellipse 20 30 "solid" "purple")
                                (ellipse 20 10 "solid" "indigo"))

                  (beside/align "baseline"
                                (text "ijy" 18 "black")
                                (text "ijy" 24 "black"))]

  }


@defproc[(above [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  vertical row, aligned along their centers.

  @image-examples[(above (ellipse 70 20 "solid" "gray")
                         (ellipse 50 20 "solid" "darkgray")
                         (ellipse 30 20 "solid" "dimgray")
                         (ellipse 10 20 "solid" "black"))]

  }

@defproc[(above/align [x-place x-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a vertical row, lined
  up as indicated by the @racket[x-place] argument. For example, if @racket[x-place]
  is @racket["middle"], then the images are placed above each other with their centers 
  lined up.

  @image-examples[(above/align "right"
                               (ellipse 70 20 "solid" "gold")
                               (ellipse 50 20 "solid" "goldenrod")
                               (ellipse 30 20 "solid" "darkgoldenrod")
                               (ellipse 10 20 "solid" "sienna"))

                  (above/align "left"
                               (ellipse 70 20 "solid" "yellowgreen")
                               (ellipse 50 20 "solid" "olivedrab")
                               (ellipse 30 20 "solid" "darkolivegreen")
                               (ellipse 10 20 "solid" "darkgreen"))]

  }

@section{Placing Images & Scenes}

Placing images into scenes is particularly useful when building worlds
and universes using @racket[2htdp/universe].

@defproc*[([(empty-scene [width (and/c real? (not/c negative?))]
                         [height (and/c real? (not/c negative?))])
            image?]
            [(empty-scene [width (and/c real? (not/c negative?))]
                          [height (and/c real? (not/c negative?))]
                          [color image-color?])
            image?])]{

Creates an empty scene, i.e., a white rectangle with a black outline.

@image-examples[(empty-scene 160 90)]

The three-argument version creates a rectangle of the specified color with
a black outline. 
}

@defproc[(place-image [image image?] [x real?] [y real?] [scene image?]) image?]{

 Places @racket[image] onto @racket[scene] with its center at the coordinates 
 (@racket[x],@racket[y]) and crops the resulting image so that it has the 
 same size as @racket[scene]. The coordinates are relative to the top-left
 of @racket[scene].
  
 @crop-warning
 
 @image-examples[(place-image 
                  (triangle 32 "solid" "red")
                  24 24
                  (rectangle 48 48 "solid" "gray"))
                 
                 (place-image 
                  (triangle 64 "solid" "red")
                  24 24
                  (rectangle 48 48 "solid" "gray"))
                 
                 (place-image
                  (circle 4 "solid" "white")
                  18 20
                  (place-image
                   (circle 4 "solid" "white")
                   0 6
                   (place-image
                    (circle 4 "solid" "white")
                    14 2
                    (place-image
                     (circle 4 "solid" "white")
                     8 14
                     (rectangle 24 24 "solid" "goldenrod")))))]
}
@defproc[(place-image/align [image image?] [x real?] [y real?] [x-place x-place?] [y-place y-place?][scene image?])
         image?]{

 Like @racket[place-image], but uses @racket[image]'s @racket[x-place] and
 @racket[y-place] to anchor the image. Also, like
 @racket[place-image], @racket[place-image/align]
 crops the resulting image so that it has the 
 same size as @racket[scene].
  
 @crop-warning
 
 @image-examples[(place-image/align (triangle 48 "solid" "yellowgreen")
                                    64 64 "right" "bottom"
                                    (rectangle 64 64 "solid" "mediumgoldenrod"))
                 (beside 
                  (place-image/align (circle 8 "solid" "tomato")
                                     0 0 "center" "center"
                                     (rectangle 32 32 "outline" "black"))
                  (place-image/align (circle 8 "solid" "tomato")
                                     8 8 "center" "center"
                                     (rectangle 32 32 "outline" "black"))
                  (place-image/align (circle 8 "solid" "tomato")
                                     16 16 "center" "center"
                                     (rectangle 32 32 "outline" "black"))
                  (place-image/align (circle 8 "solid" "tomato")
                                     24 24 "center" "center"
                                     (rectangle 32 32 "outline" "black"))
                  (place-image/align (circle 8 "solid" "tomato")
                                     32 32 "center" "center"
                                     (rectangle 32 32 "outline" "black")))]
}

@defproc[(scene+line [image image?]
                     [x1 real?] [y1 real?]
                     [x2 real?] [y2 real?]
                     [pen-or-color (or/c pen? image-color?)])
         image?]{

  Adds a line to the image @racket[scene], starting from the point (@racket[x1],@racket[y1])
  and going to the point (@racket[x2],@racket[y2]); unlike
  @racket[add-line], this function crops the resulting image to the size of @racket[scene].
  
  @crop-warning
  
  @image-examples[(scene+line (ellipse 40 40 "outline" "maroon")
                              0 40 40 0 "maroon")
                  (scene+line (rectangle 40 40 "solid" "gray")
                              -10 50 50 -10 "maroon")
                  (scene+line
                   (rectangle 100 100 "solid" "darkolivegreen")
                   25 25 100 100 
                   (make-pen "goldenrod" 30 "solid" "round" "round"))]
}

@defproc[(scene+curve [scene image?] 
                      [x1 real?] [y1 real?] [angle1 angle?] [pull1 real?]
                      [x2 real?] [y2 real?] [angle2 angle?] [pull2 real?]
                      [color image-color?])
         image?]{

Adds a curve to @racket[scene], starting at the point
(@racket[x1],@racket[y1]), and ending at the point
(@racket[x2],@racket[y2]).

The @racket[angle1] and @racket[angle2] arguments specify the 
angle that the curve has as it leaves the initial point and
as it reaches the final point, respectively. 

The @racket[pull1] and @racket[pull2] arguments control how
long the curve tries to stay with that angle. Larger numbers
mean that the curve stays with the angle longer.

Unlike @racket[add-curve], this function crops the curve, only showing
the parts that fit onto @racket[scene].

@crop-warning

@image-examples[(scene+curve (rectangle 100 100 "solid" "black")
                             20 20 0 1/3
                             80 80 0 1/3
                             "white")
                (scene+curve (rectangle 100 100 "solid" "black")
                             20 20 0 1 
                             80 80 0 1
                             "white")
                (scene+curve 
                 (add-curve 
                  (rectangle 40 100 "solid" "black")
                  20 10 180 1/2
                  20 90 180 1/2
                  "white")
                 20 10 0 1/2
                 20 90 0 1/2
                 "white")

                (scene+curve (rectangle 100 100 "solid" "black")
                             -20 -20 0 1 
                             120 120 0 1
                             "red")]
}


@section{Rotating, Scaling, Flipping, Cropping, and Framing Images}

@defproc[(rotate [angle angle?] [image image?]) image?]{
  Rotates @racket[image] by @racket[angle] degrees in a counter-clockwise direction.

          @image-examples[(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
                          (rotate 5 (rectangle 50 50 "outline" "black"))
                          (rotate 45
                                  (beside/align
                                   "center"
                                   (rectangle 40 20 "solid" "darkseagreen")
                                   (rectangle 20 100 "solid" "darkseagreen")))]
          
}

@defproc[(scale [factor (and/c real? positive?)] [image image?]) image?]{

  Scales @racket[image] by @racket[factor]. 
  
  The pen sizes are also scaled and thus draw thicker (or thinner)
  lines than the original image, unless the pen was size 
  @racket[0]. That pen size is treated specially to mean ``the
  smallest available line'' and thus it always draws a one pixel
  wide line; this is also the case for @racket['outline] and @racket["outline"]
  shapes that are drawn with an @racket[image-color?] instead of
  a @racket[pen].
  
         
  @image-examples[(scale 2 (ellipse 20 30 "solid" "blue"))
                   (ellipse 40 60 "solid" "blue")]
  
  
  
}

@defproc[(scale/xy [x-factor (and/c real? positive?)] [y-factor (and/c real? positive?)] [image image?]) image?]{
  Scales @racket[image] by @racket[x-factor] horizontally and by 
  @racket[y-factor] vertically. 
  
  @image-examples[(scale/xy 3 
                            2 
                            (ellipse 20 30 "solid" "blue")) 
                  (ellipse 60 60 "solid" "blue")]
}

@defproc[(flip-horizontal [image image?]) image?]{
   Flips @racket[image] left to right.
         
         Flipping images with text is not supported (so passing @racket[flip-horizontal] an image
         that contains a @racket[text] or @racket[text/font] image inside somewhere signals an error).
         
         @image-examples[(beside
                          (rotate 30 (square 50 "solid" "red"))
                          (flip-horizontal
                           (rotate 30 (square 50 "solid" "blue"))))]
}

@defproc[(flip-vertical [image image?]) image?]{
   Flips @racket[image] top to bottom.
         
         Flipping images with text is not supported (so passing @racket[flip-vertical] an image
         that contains a @racket[text] or @racket[text/font] image inside somewhere signals an error).

         @image-examples[(above 
                          (star 40 "solid" "firebrick")
                          (scale/xy 1 1/2 (flip-vertical (star 40 "solid" "gray"))))]
}

@defproc[(crop [x real?]
               [y real?] 
               [width (and/c real? (not/c negative?))]
               [height (and/c real? (not/c negative?))]
               [image image?])
         image?]{

 Crops @racket[image] to the rectangle with the upper left at the point (@racket[x],@racket[y])
 and with @racket[width] and @racket[height]. 
 
 @crop-warning
 
 @image-examples[(crop 0 0 40 40 (circle 40 "solid" "chocolate"))
                 (crop 40 60 40 60 (ellipse 80 120 "solid" "dodgerblue"))
                 (above
                  (beside (crop 40 40 40 40 (circle 40 "solid" "palevioletred"))
                          (crop 0 40 40 40 (circle 40 "solid" "lightcoral")))
                  (beside (crop 40 0 40 40 (circle 40 "solid" "lightcoral"))
                          (crop 0 0 40 40 (circle 40 "solid" "palevioletred"))))]
                 
}

@defproc[(frame [image image?]) image?]{
  Returns an image just like @racket[image], except
  with a black, single pixel frame drawn around the 
  bounding box of the image.
  
  @image-examples[(frame (ellipse 40 40 "solid" "gray"))]
  
  Generally speaking, this function is useful to 
  debug image constructions, i.e., to see where
  certain sub-images appear within some larger image.
  
  @image-examples[(beside
                   (ellipse 20 70 "solid" "lightsteelblue")
                   (frame (ellipse 20 50 "solid" "mediumslateblue"))
                   (ellipse 20 30 "solid" "slateblue")
                   (ellipse 20 10 "solid" "navy"))]
}

@section{Bitmaps}

DrRacket's @seclink["images" #:doc '(lib "scribblings/drracket/drracket.scrbl")]{Insert Image ...} 
menu item allows you to insert images into your program text, and those images are treated 
as images for this library. 

Unlike all of the other images in this library, those images (and the other images created
by functions in this section of the documentation)
are represented as bitmaps, i.e., an array of colors (that can be quite large in some cases).
This means that scaling and rotating them loses fidelity in the image and is significantly
more expensive than with the other shapes.

See also the @racketmodname[2htdp/planetcute] library.

@defform/subs[(bitmap bitmap-spec)
              ([bitmap-spec rel-string
                            id])]{

  Loads the bitmap specified by @racket[bitmap-spec]. If
  @racket[bitmap-spec] is a string, it is treated as a relative path.
  If it is an identifier, it is treated like a require spec and used to
  refer to a file in a collection.

  @image-examples[(bitmap icons/stop-16x16.png)
                  (bitmap icons/b-run.png)]
}

@defproc[(bitmap/url [url string?]) image?]{
  Goes out on the web and downloads the image at @racket[url].

  Downloading the image happens each time this function is called, so
  you may find it simpler to download the image once with a browser 
  and then paste it into your program or download it and use @racket[bitmap].
}

@defproc[(bitmap/file [ps path-string?]) image?]{
  Loads the image from @racket[ps].

  If @racket[ps] is a relative path, the file is relative to
  the current directory. (When running in DrRacket, the current
  directory is set to the place where the definitions window is
  saved, but in general this can be an arbitrary directory.)
}


@defproc[(image->color-list [image image?]) (listof color?)]{
  Returns a list of colors that correspond to the colors in the
  image, reading from left to right, top to bottom.
  
  @image-examples[(image->color-list (rectangle 2 2 "solid" "black"))
                  (image->color-list
                   (above (beside (rectangle 1 1 "solid" (make-color 1 1 1))
                                  (rectangle 1 1 "solid" (make-color 2 2 2)))
                          (beside (rectangle 1 1 "solid" (make-color 3 3 3))
                                  (rectangle 1 1 "solid" (make-color 4 4 4)))))]
  
}

@defproc[(color-list->bitmap [colors (listof image-color?)] 
                             [width (and/c real? (not/c negative?))]
                             [height (and/c real? (not/c negative?))])
         image?]{
  Constructs a bitmap from the given @racket[colors], with the given @racket[width] and @racket[height].

  @image-examples[(scale
                   40
                   (color-list->bitmap
                    (list "red" "green" "blue")
                    3 1))]
  
  }

@defproc*[([(freeze [image image?]) image?]
           [(freeze [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))]
                    [image image?]) image?]
           [(freeze [x real?]
                    [y real?]
                    [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))]
                    [image image?]) image?])]{
  Freezing an image internally builds a bitmap, crops the image, draws the cropped image
  into the bitmap and then
  uses the bitmap to draw that image afterwards. Typically this is used as a performance
  hint. When an image both contains many sub-images and is going to be drawn many times
  (but not scaled or rotated),
  using freeze on the image can substantially improve performance without changing how
  the image draws (assuming it draws only inside its bounding box; see also @secref["nitty-gritty"]).
  
  If @racket[freeze] is passed only the image argument, then it crops the image to its bounding
  box. If it is given three arguments, the two numbers are used as the width and height and
  the five argument version fully specifies where to crop the image.
}

@section{Image Properties}

@defproc[(image-width [i image?]) (and/c integer? (not/c negative?) exact?)]{
  Returns the width of @racket[i].

  @image-examples[(image-width (ellipse 30 40 "solid" "orange"))
                  (image-width (circle 30 "solid" "orange"))
                  (image-width (beside (circle 20 "solid" "orange")
                                       (circle 20 "solid" "purple")))
                  (image-width (rectangle 0 10 "solid" "purple"))]
}

@defproc[(image-height [i image?]) (and/c integer? (not/c negative?) exact?)]{
  Returns the height of @racket[i].
  
  @image-examples[(image-height (ellipse 30 40 "solid" "orange"))
                  (image-height (circle 30 "solid" "orange"))
                  (image-height (overlay (circle 20 "solid" "orange")
                                         (circle 30 "solid" "purple")))
                  (image-height (rectangle 10 0 "solid" "purple"))]
  }

@defproc[(image-baseline [i image?]) (and/c integer? (not/c negative?) exact?)]{
  Returns the distance from the top of the image to its baseline. 
  The baseline of an image is the place where the bottoms any letters line up, 
  but without counting the descenders, e.g. the tail on ``y'' or ``g'' or ``j''.
  
  Unless the image was constructed with @racket[text], @racket[text/font] 
  or, in some cases, @racket[crop], this will be the same as its height.
  
  @image-examples[(image-baseline (text "Hello" 24 "black"))
                  (image-height (text "Hello" 24 "black"))
                  (image-baseline (rectangle 100 100 "solid" "black"))
                  (image-height (rectangle 100 100 "solid" "black"))]

  A @racket[crop]ped image's baseline is the same as the image's baseline, if the
  cropping stays within the original image's bounding box. But if the cropping actually
  enlarges the image, then the baseline can end up being smaller.
  
  @image-examples[(image-height (rectangle 20 20 "solid" "black"))
                  (image-baseline (rectangle 20 20 "solid" "black"))
                  
                  (image-height (crop 10 10 5 5 (rectangle 20 20 "solid" "black")))
                  (image-baseline (crop 10 10 5 5 (rectangle 20 20 "solid" "black")))
                  
                  (image-height (crop 10 10 30 30 (rectangle 20 20 "solid" "black")))
                  (image-baseline (crop 10 10 30 30 (rectangle 20 20 "solid" "black")))]
                  
}

@section{Image Predicates}

This section lists predicates for the basic structures provided by the image library.

@defproc[(image? [x any/c]) boolean?]{
 Determines if @racket[x] is an image. Images are returned by functions
 like @racket[ellipse] and @racket[rectangle] and
 accepted by functions like @racket[overlay] and @racket[beside].

 Additionally, images inserted into a DrRacket window are treated as
 bitmap images, as are instances of @racket[image-snip%] and @racket[bitmap%].
 }

@defproc[(mode? [x any/c]) boolean?]{
 Determines if @racket[x] is a mode suitable for
 constructing images. 
 
 It can be one of
 @racket['solid], @racket["solid"], @racket['outline],
 or @racket["outline"], indicating if the shape is
 filled in or not.
 
 It can also be an integer between @racket[0] and @racket[255] (inclusive)
 indicating the transparency of the image. The integer @racket[255] is
 fully opaque, and is the same as @racket["solid"] (or @racket['solid]).
 The integer @racket[0] means fully transparent.
}

@defproc[(image-color? [x any/c]) boolean?]{

  Determines if @racket[x] represents a color. Strings, symbols,
  and @racket[color] structs are allowed as colors.

  For example,
  @racket["magenta"], @racket["black"], @racket['orange], and @racket['purple]
  are allowed. Colors are not case-sensitive, so 
  @racket["Magenta"], @racket["Black"], @racket['Orange], and @racket['Purple]
  are also allowed, and are the same colors as in the previous sentence.
  If a string or symbol color name is not recognized, black is used in its place.
  
  The complete list of colors is available in the documentation for
  @racket[color-database<%>].

}

@defstruct[color ([red (and/c natural-number/c (<=/c 255))]
                  [green (and/c natural-number/c (<=/c 255))]
                  [blue (and/c natural-number/c (<=/c 255))]
                  [alpha (and/c natural-number/c (<=/c 255))])]{
  The @racket[color] struct defines a color with @racket[red], 
      @racket[green], @racket[blue], and @racket[alpha] components
      that range from @racket[0] to @racket[255]. 
      
    The @racket[red], @racket[green], and @racket[blue] fields
      combine to make a color, with the higher values meaning more of the given color. 
      For example, @racket[(make-color 255 0 0)] makes a
      bright red color and @racket[(make-color 255 0 255)] makes a bright purple.
    
      The @racket[alpha] field controls the transparency of the color. A value of @racket[255] means
      that the color is opaque and @racket[0] means the color is fully transparent.
      
  The constructor, @racket[make-color], also accepts only three arguments, in which case
  the three arguments are used for the @racket[red], @racket[green], and @racket[blue] fields, and the
  @racket[alpha] field defaults to @racket[255].
}

@defproc[(y-place? [x any/c]) boolean?]{
  Determines if @racket[x] is a placement option
  for the vertical direction. It can be one
  of
@racket["top"],
@racket['top], 
@racket["bottom"],
@racket['bottom],
@racket["middle"],
@racket['middle],
@racket["center"],
@racket['center],
@racket["baseline"],
@racket['baseline],
@racket["pinhole"], or
@racket['pinhole].

Using @racket["pinhole"] or @racket['pinhole] is only allowed when all of the image arguments have @seclink["pinholes"]{pinholes}.

See also @racket[image-baseline] for more discussion of baselines.

}

@defproc[(x-place? [x any/c]) boolean?]{
  Determines if @racket[x] is a placement option
  for the horizontal direction. It can be one
  of @racket["left"],
  @racket['left], 
  @racket["right"],
  @racket['right],
  @racket["middle"],
  @racket['middle],
  @racket["center"],
  @racket['center],
  @racket["pinhole"], or
  @racket['pinhole].

  Using @racket["pinhole"] or @racket['pinhole] is only allowed when all of the image arguments have @seclink["pinholes"]{pinholes}.

}

@defproc[(angle? [x any/c]) boolean?]{
  Determines if @racket[x] is an angle, namely
  a real number between @racket[0] (inclusive)
  and @racket[360] (exclusive).
}

@defproc[(side-count? [x any/c]) boolean?]{
  Determines if @racket[x] is an integer 
  greater than or equal to @racket[3].
}

@defproc[(step-count? [x any/c]) boolean?]{
  Determines if @racket[x] is an integer greater than or equal to @racket[1].
}

@defproc[(real-valued-posn? [x any/c]) boolean?]{
  Determines if @racket[x] is a @racket[posn] whose @racket[_x] and @racket[_y]
  fields are both @racket[real?] numbers.
}

@defstruct[pen ([color image-color?]
                [width (and/c real? (<=/c 0 255))]
                [style pen-style?]
                [cap pen-cap?]
                [join pen-join?])]{
  The @racket[pen] struct specifies how the drawing library draws lines. 
      
      
      A good default for @racket[style] is @racket["solid"], and
      good default values for the @racket[cap] and @racket[join] fields
      are @racket["round"].
      
      Using @racket[0] as a width is special; it means to always draw the 
      smallest possible, but visible, pen. This means that the pen will always
      be one pixel in size, no matter how the image is scaled.
}

@defproc[(pen-style? [x any/c]) boolean?]{
  Determines if @racket[x] is a valid pen style.
  It can be one of
  @racket["solid"], @racket['solid], 
  @racket["dot"], @racket['dot], 
  @racket["long-dash"], @racket['long-dash], 
  @racket["short-dash"], @racket['short-dash], 
  @racket["dot-dash"], or @racket['dot-dash].
}

@defproc[(pen-cap? [x any/c]) boolean?]{
  Determines if @racket[x] is a valid pen cap.
  It can be one of
  @racket["round"], @racket['round], 
  @racket["projecting"], @racket['projecting], 
  @racket["butt"], or @racket['butt].
}

@defproc[(pen-join? [x any/c]) boolean?]{
  Determines if @racket[x] is a valid pen join.
  It can be one of
  @racket["round"], @racket['round], 
  @racket["bevel"], @racket['bevel], 
  @racket["miter"], or @racket['miter].
}

@section{Equality Testing of Images}

Two images are @racket[equal?] if they draw exactly the same way at their current size
(not necessarily at all sizes) and, if there are pinholes, the pinholes are
in the same place.

@section[#:tag "pinholes"]{Pinholes}

A pinhole is an optional property of an image that identifies a point somewhere
in the image. The pinhole can then be used to facilitate overlaying images by
lining them up on the their pinholes. 

When an image has a pinhole, the pinhole
is drawn with crosshairs on the image.
The crosshairs are drawn with a two one pixel wide black lines (one horizontal and one vertical)
and two one pixel wide white lines,
where the black lines is drawn .5 pixels to the left and above the pinhole, and the
white lines are drawn .5 pixels to the right and below the pinhole. 
Accordingly, when the pixel is on an integral coordinate, then black and white lines all 
take up a single pixel and in the center of their intersections is the actual pinholes.
See @secref["nitty-gritty"] for more details about pixels.

When images are @racket[overlay]'d, @racket[underlay]'d (or the variants of those functions),
placed @racket[beside], or @racket[above] each other, 
the pinhole of the resulting image is the pinhole of the first image argument passed to the combining
operation. When images are combined with @racket[place-image] (or the variants of @racket[place-image]), 
then the scene argument's pinhole is preserved.

@defproc[(center-pinhole [image image?]) image?]{
  Creates a pinhole in @racket[image] at its center.
  @image-examples[(center-pinhole (rectangle 40 20 "solid" "red"))
                  (rotate 30 (center-pinhole (rectangle 40 20 "solid" "orange")))]
}
@defproc[(put-pinhole [x integer?] [y integer?] [image image?]) image?]{
  Creates a pinhole in @racket[image] at the point (@racket[x],@racket[y]).
  @image-examples[(put-pinhole 2 18 (rectangle 40 20 "solid" "forestgreen"))]
}
@defproc[(pinhole-x [image image?]) (or/c integer? #f)]{
  Returns the x coordinate of @racket[image]'s pinhole.
  @image-examples[(pinhole-x (center-pinhole (rectangle 10 10 "solid" "red")))]
}
@defproc[(pinhole-y [image image?]) (or/c integer? #f)]{
  Returns the y coordinate of @racket[image]'s pinhole.
  @image-examples[(pinhole-y (center-pinhole (rectangle 10 10 "solid" "red")))]
}
@defproc[(clear-pinhole [image image?]) image?]{
  Removes a pinhole from @racket[image] (if the image has a pinhole).
}

@defproc[(overlay/pinhole [i1 image?] [i2 image?] [is image?] ...) image?]{
  
  Overlays all of the image arguments on their pinholes. If any of the
  arguments do not have pinholes, then the center of the image is used instead.
  
  @image-examples[(overlay/pinhole
                   (put-pinhole 25 10 (ellipse 100 50 "solid" "red"))
                   (put-pinhole 75 40 (ellipse 100 50 "solid" "blue")))
                  (let ([petal (put-pinhole 
                                20 20
                                (ellipse 100 40 "solid" "purple"))])
                    (clear-pinhole
                     (overlay/pinhole
                      (circle 30 "solid" "yellow")
                      (rotate (* 60 0) petal)
                      (rotate (* 60 1) petal)
                      (rotate (* 60 2) petal)
                      (rotate (* 60 3) petal)
                      (rotate (* 60 4) petal)
                      (rotate (* 60 5) petal))))]
}

@defproc[(underlay/pinhole [i1 image?] [i2 image?] [is image?] ...) image?]{
  
  Underlays all of the image arguments on their pinholes. If any of the
  arguments do not have pinholes, then the center of the image is used instead.
  
  @image-examples[(underlay/pinhole
                   (put-pinhole 25 10 (ellipse 100 50 "solid" "red"))
                   (put-pinhole 75 40 (ellipse 100 50 "solid" "blue")))
                  (let* ([t (triangle 40 "solid" "orange")]
                         [w (image-width t)]
                         [h (image-height t)])
                    (clear-pinhole
                     (overlay/pinhole
                      (put-pinhole (/ w 2) 0 t)
                      (put-pinhole w h t)
                      (put-pinhole 0 h t))))]
}

@;-----------------------------------------------------------------------------
@section{Exporting Images to Disk}

In order to use an image as an input to another program (e.g., Photoshop or 
a web browser), it is necessary to represent it in a format that these programs
can understand. 

The @racket[save-image] function provides this functionality, 
writing an image to disk using the @tt{PNG} format. Since this
format represents an image using a set of pixel values, an image written to disk
generally contains less information than the image that was written, and cannot be scaled
or manipulated as cleanly (by any image program).

The @racket[save-svg-image] function writes an @tt{SVG} file format
representation of the file to the disk that, unlike @racket[save-image] produces
an image that can still be scaled arbitrarily look as good as scaling the
image directly via @racket[scale].

@defproc[(save-image [image image?]
                     [filename path-string?]
                     [width 
                      (and/c real? (not/c negative?))
                      (image-width image)]
                     [height 
                      (and/c real? (not/c negative?))
                      (image-height image)])
         boolean?]{
 Writes an image to the path specified by @racket[filename], using the
 @tt{PNG} format.
 
 The last two arguments are optional. If present, they determine the width
 and height of the save image file. If absent, the width and height of the image is used.
 
 }

@defproc[(save-svg-image [image image?]
                         [filename path-string?]
                         [width 
                          (and/c real? (not/c negative?))
                          (image-width image)]
                         [height 
                          (and/c real? (not/c negative?))
                          (image-height image)])
         void?]{
 Writes an image to the path specified by @racket[filename], using the
 @tt{SVG} format.
 
 The last two arguments are optional. If present, they determine the width
 and height of the save image file. If absent, the width and height of the image is used.
 }

@(close-eval img-eval)
