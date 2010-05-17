#lang scribble/doc

@(require (for-label (only-in scheme/contract and/c or/c any/c not/c)
                     2htdp/image
                     (except-in lang/htdp-beginner make-posn posn? posn-x posn-y image?)
                     lang/posn
                     scheme/gui/base
                     (only-in scheme/base path-string?))
          lang/posn
          "shared.ss"
          "image-util.ss"
          scribble/decode
          scribble/manual)

@teachpack["image"]{Images}

@(define mode/color-text
   (make-splice
    @list{If the @scheme[mode] is @scheme['outline] or @scheme["outline"], then the last
     argument can be a @scheme[pen] struct or an @scheme[image-color?], but if the @scheme[mode]
     is @scheme['solid] or @scheme["solid"], then the last argument must be an
     @scheme[image-color?].}))

@defmodule[#:require-form beginner-require 2htdp/image]

The image teachpack provides a number of basic image construction functions, along with
combinators for building more complex images out of existing images. Basic images include
various polygons, ellipses and circles, and text, as well as bitmaps (typically bitmaps 
come about via the @onscreen{Insert Image...} menu item in DrRacket).
Existing images can be rotated, scaled, and overlaid on top of each other.

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
  
  @mode/color-text
  
   @image-examples[(circle 30 "outline" "red")
                   (circle 20 "solid" "blue")]

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
  Constructs an ellipsis with the given width, height, mode, and color.

  @mode/color-text
  
  @image-examples[(ellipse 40 20 "outline" "black")
                  (ellipse 20 40 "solid" "blue")] 
}

@defproc*[([(triangle [side-length (and/c real? (not/c negative?))] 
                      [mode mode?]
                      [color image-color?])
            image?]
           [(triangle [side-length (and/c real? (not/c negative?))] 
                      [outline-mode (or/c 'outline "outline")]
                      [pen-or-color (or/c pen? image-color?)])
            image?])]{

   Constructs a upward-pointing equilateral triangle. 
  The @scheme[side-length] argument 
  determines the 
  length of the side of the triangle.

    @mode/color-text
  
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
  to the right angle have lengths @scheme[side-length1] and @scheme[side-length2].

  @mode/color-text
  
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

 Creates a triangle with two equal-length sides, of length @scheme[side-length]
 where the angle between those sides is @scheme[angle]. The third
 leg is straight, horizontally. If the angle is less than 
 @scheme[180], then the triangle will point up and if the @scheme[angle]
 is more, then the triangle will point down. 
 
 @mode/color-text
 
 @image-examples[(isosceles-triangle 200 170 "solid" "seagreen")
                 (isosceles-triangle 60 30 "solid" "aquamarine")
                 (isosceles-triangle 60 330 "solid" "lightseagreen")]
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
 
 @mode/color-text
 
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
  
  @mode/color-text
  
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
other. The top and bottom pair of angles is @scheme[angle] and the left and right are @scheme[(- 180 angle)].

@mode/color-text

@image-examples[(rhombus 40 45 "solid" "magenta")
                (rhombus 80 150 "solid" "mediumpurple")]
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
  Constructs a regular polygon with @scheme[side-count] sides.

  @mode/color-text

  @image-examples[(regular-polygon 50 3 "outline" "red")
                  (regular-polygon 40 4 "outline" "blue")
                  (regular-polygon 20 8 "solid" "red")]
}

@defproc*[([(star [side-length (and/c real? (not/c negative?))] 
                  [mode mode?]
                  [color image-color?])
            image?]
           [(star [side-length (and/c real? (not/c negative?))] 
                  [outline-mode (or/c 'outline "outline")]
                  [color (or/c pen? image-color?)])
            image?])]{
  Constructs a star with five points. The @scheme[side-length] argument 
  determines the side length of the enclosing pentagon.

  @mode/color-text

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
  The polygon is enclosed by a regular polygon with @scheme[side-count] sides each
  @scheme[side-length] long. The polygon is actually constructed by going from vertex to
  vertex around the regular polgon, but skipping over every @scheme[step-count] vertices.
  
  For examples, if @scheme[side-count] is @scheme[5] and @scheme[step-count] is @scheme[2],
  then this function produces a shape just like @scheme[star].
  
  @mode/color-text

  @image-examples[(star-polygon 40 5 2 "solid" "seagreen")
                  (star-polygon 40 7 3 "outline" "darkred")
                  (star-polygon 20 10 3 "solid" "cornflowerblue")]
 
}
                
@defproc*[([(polygon [vertices (listof posn?)] 
                     [mode mode?]
                     [color image-color?])
            image?]
           [(polygon [vertices (listof posn?)] 
                     [outline-mode (or/c 'outline "outline")]
                     [pen-or-color (or/c pen? image-color?)])
            image?])]{
  Constructs a polygon connecting the given vertices.
  
  @mode/color-text
  
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

@defproc[(line [x1 real?] [y1 real?] [color image-color?]) image?]{
  Constructs an image representing a line segment that connects the points
  (0,0) to (x1,y1).
  
  @image-examples[(line 30 30 "black")
                  (line -30 20 "red")
                  (line 30 -20 "red")]
}

@defproc[(add-line [image image?]
                   [x1 real?] [y1 real?]
                   [x2 real?] [y2 real?]
                   [color image-color?])
         image?]{

  Adds a line to the image @scheme[image], starting from the point (@scheme[x1],@scheme[y1])
  and going to the point (@scheme[x2],@scheme[y2]).
  Unlike @scheme[scene+line], if the line passes outside of @scheme[image], the image
  gets larger to accomodate the line.
  
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
                    [color image-color?])
         image?]{

Adds a curve to @scheme[image], starting at the point
(@scheme[x1],@scheme[y1]), and ending at the point
(@scheme[x2],@scheme[y2]).

The @scheme[angle1] and @scheme[angle2] arguments specify the 
angle that the curve has as it leaves the initial point and
as it reaches the final point, respectively. 

The @scheme[pull1] and @scheme[pull2] arguments control how
long the curve tries to stay with that angle. Larger numbers
mean that the curve stays with the angle longer.

Unlike @scheme[scene+curve], if the line passes outside of @scheme[image], the image
  gets larger to accomodate the curve.


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
  
  The @scheme[face] and the @scheme[family] combine to give the complete typeface. If 
  @scheme[face] is available on the system, it is used, but if not then a default typeface
  based on the @scheme[family] is chosen. The @scheme[style] controls if the face is italic
  or not (under Windows and Mac OS X, @scheme['slant] and @scheme['italic] are the same),
  the @scheme[weight] controls if it is boldface (or light), and @scheme[underline?]
  determines if the face is underlined. For more details on these arguments, see @scheme[font%],
  which ultimately is what this code uses to draw the font.
                 
  @image-examples[(text/font "Hello" 24 "olive"
                             "Gill Sans" 'swiss 'normal 'bold #f)
                  (text/font "Goodbye" 18 "indigo"
                             #f 'modern 'italic 'normal #f)
                  (text/font "not really a link" 18 "blue"
                             #f 'roman 'normal 'normal #t)]
}

@defform/subs[(bitmap bitmap-spec)
              ([bitmap-spec rel-string
                            id])]{
                                  
  Loads the bitmap specified by @scheme[bitmap-spec]. If @scheme[bitmap-spec] is a string, it is treated as a 
  relative path. If it is an identifier, it is treated like a require spec and used to refer to a file
  in a collection.
  
  @image-examples[(bitmap icons/stop-16x16.png)
                  (bitmap icons/b-run.png)]
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
  Overlays all of its image arguments, much like the @scheme[overlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
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

@defproc[(overlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by overlaying @scheme[i1] on top of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
  @image-examples[(overlay/xy (rectangle 20 20 "outline" "black")
                              20 0
                              (rectangle 20 20 "outline" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              20 20
                              (rectangle 20 20 "solid" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              -20 -20
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
  
  It behaves like @scheme[overlay], but with the arguments in the reverse order.
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
                            (ellipse 60 10 "solid" "black"))]
  
  }

@defproc[(underlay/align [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Underlays all of its image arguments, much like the @scheme[underlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
  on their centers.

  @image-examples[(underlay/align "left" "middle"
                                  (rectangle 30 60 "solid" "orange")
                                  (ellipse 60 30 "solid" "purple"))
                  (underlay/align "right" "top"
                                  (rectangle 50 50 "solid" "seagreen")
                                  (rectangle 40 40 "solid" "silver")
                                  (rectangle 30 30 "solid" "seagreen")
                                  (rectangle 20 20 "solid" "silver"))]
                                  
  
  }

@defproc[(underlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by underlaying @scheme[i1] underneath of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
  
  This is the same as @scheme[(overlay/xy i2 (- x) (- y) i1)].
  
  @image-examples[(underlay/xy (rectangle 20 20 "outline" "black")
                               20 0
                               (rectangle 20 20 "outline" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               20 20
                               (rectangle 20 20 "solid" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               -20 -20
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
  up as indicated by the @scheme[y-place] argument. For example, if @scheme[y-place]
  is @scheme["middle"], then the images are placed side by side with their centers 
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
  up as indicated by the @scheme[x-place] argument. For example, if @scheme[x-place]
  is @scheme["middle"], then the images are placed above each other with their centers 
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
and universes using @scheme[2htdp/universe].

@defproc[(empty-scene [width (and/c real? (not/c negative?))]
                      [height (and/c real? (not/c negative?))])
         image?]{

Creates an empty scene, i.e., a rectangle with a black outline.

@image-examples[(empty-scene 160 90)]
                                                                 
}

@defproc[(place-image [image image?] [x real?] [y real?] [scene image?]) image?]{

 Places @scheme[image] onto @scheme[scene] with its center at the coordinates 
 (@scheme[x],@scheme[y]) and crops the resulting image so that it has the 
 same size as @scheme[scene]. The coordinates are relative to the top-left
 of @scheme[scene].
  
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

 Like @scheme[place-image], but uses @scheme[image]'s @scheme[x-place] and
 @scheme[y-place] to anchor the image. Also, like
 @scheme[place-image], @scheme[place-image/align]
 crops the resulting image so that it has the 
 same size as @scheme[scene].
  
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
                     [color image-color?])
         image?]{

  Adds a line to the image @scheme[scene], starting from the point (@scheme[x1],@scheme[y1])
  and going to the point (@scheme[x2],@scheme[y2]); unlike
  @scheme[add-line], this function crops the resulting image to the size of @scheme[scene].
  
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

Adds a curve to @scheme[scene], starting at the point
(@scheme[x1],@scheme[y1]), and ending at the point
(@scheme[x2],@scheme[y2]).

The @scheme[angle1] and @scheme[angle2] arguments specify the 
angle that the curve has as it leaves the initial point and
as it reaches the final point, respectively. 

The @scheme[pull1] and @scheme[pull2] arguments control how
long the curve tries to stay with that angle. Larger numbers
mean that the curve stays with the angle longer.

Unlike @scheme[add-curve], this function crops the curve, only showing
the parts that fit onto @scheme[scene].

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

                
@section{Rotating, Scaling, Cropping, and Framing Images}

@defproc[(rotate [angle angle?] [image image?]) image?]{
  Rotates @scheme[image] by @scheme[angle] degrees in a counter-clockwise direction.

          @image-examples[(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
                          (rotate 5 (rectangle 50 50 "outline" "black"))
                          (rotate 45
                                  (beside/align
                                   "center"
                                   (rectangle 40 20 "solid" "darkseagreen")
                                   (rectangle 20 100 "solid" "darkseagreen")))]
          
}

@defproc[(scale [factor (and/c real? positive?)] [image image?]) image?]{

  Scales @scheme[image] by @scheme[factor]. 
  
  The pen sizes are also scaled and thus draw thicker (or thinner)
  lines than the original image, unless the pen was size 
  @scheme[0]. That pen size is treated specially to mean ``the
  smallest available line'' and thus it always draws a one pixel
  wide line; this is also the case for @scheme['outline] and @scheme["outline"]
  shapes that are drawn with an @scheme[image-color?] instead of
  a @scheme[pen].
  
         
  @image-examples[(scale 2 (ellipse 20 30 "solid" "blue"))
                   (ellipse 40 60 "solid" "blue")]
  
  
  
}

@defproc[(scale/xy [x-factor (and/c real? positive?)] [y-factor (and/c real? positive?)] [image image?]) image?]{
  Scales @scheme[image] by @scheme[x-factor] horizontally and by 
  @scheme[y-factor] vertically. 
  
  @image-examples[(scale/xy 3 
                            2 
                            (ellipse 20 30 "solid" "blue")) 
                  (ellipse 60 60 "solid" "blue")]
}

@defproc[(crop [x real?] [y real?] 
               [width (and/c real? (not/c negative?))]
               [height (and/c real? (not/c negative?))]
               [image image?])
         image?]{

 Crops @scheme[image] to the rectangle with the upper left at the point (@scheme[x],@scheme[y])
 and with @scheme[width] and @scheme[height]. 
 
 @image-examples[(crop 0 0 40 40 (circle 40 "solid" "chocolate"))
                 (crop 40 60 40 60 (ellipse 80 120 "solid" "dodgerblue"))
                 (above
                  (beside (crop 40 40 40 40 (circle 40 "solid" "palevioletred"))
                          (crop 0 40 40 40 (circle 40 "solid" "lightcoral")))
                  (beside (crop 40 0 40 40 (circle 40 "solid" "lightcoral"))
                          (crop 0 0 40 40 (circle 40 "solid" "palevioletred"))))]
                 
}

@defproc[(frame [image image?]) image?]{
  Returns an image just like @scheme[image], except
  with a black, single pixel frame drawn around the 
  bounding box of the image.
  
  @image-examples[(frame (ellipse 20 20 "outline" "black"))]
  
  Generally speaking, this function is useful to 
  debug image constructions, i.e., to see where
  certain sub-images appear within some larger image.
  
  @image-examples[(beside
                   (ellipse 20 70 "solid" "lightsteelblue")
                   (frame (ellipse 20 50 "solid" "mediumslateblue"))
                   (ellipse 20 30 "solid" "slateblue")
                   (ellipse 20 10 "solid" "navy"))]
}

@section{Image Properties}

@defproc[(image-width [i image?]) (and/c integer? positive? exact?)]{
  Returns the width of @scheme[i].
                       
  @image-examples[(image-width (ellipse 30 40 "solid" "orange"))
                  (image-width (circle 30 "solid" "orange"))
                  (image-width (beside (circle 20 "solid" "orange")
                                       (circle 20 "solid" "purple")))]
}

@defproc[(image-height [i image?]) (and/c integer? positive? exact?)]{
  Returns the height of @scheme[i].
  
  @image-examples[(image-height (ellipse 30 40 "solid" "orange"))
                  (image-height (circle 30 "solid" "orange"))
                  (image-height (overlay (circle 20 "solid" "orange")
                                         (circle 30 "solid" "purple")))]
  }

@defproc[(image-baseline [i image?]) (and/c integer? positive? exact?)]{
  Returns the distance from the top of the image to its baseline. 
  Unless the image was constructed with @scheme[text] or @scheme[text/font],
  this will be the same as its height.
  
  @image-examples[(image-baseline (text "Hello" 24 "black"))
                  (image-height (text "Hello" 24 "black"))
                  (image-baseline (rectangle 100 100 "solid" "black"))
                  (image-height (rectangle 100 100 "solid" "black"))]
}

@section{Image Predicates}

This section lists predicates for the basic structures provided by the image library.

@defproc[(image? [x any/c]) boolean?]{
 Determines if @scheme[x] is an image. Images are returned by functions
 like @scheme[ellipse] and @scheme[rectangle] and
 accepted by functions like @scheme[overlay] and @scheme[beside].

 Additionally, images inserted into a DrRacket window are treated as
 bitmap images, as are instances of @scheme[image-snip%] and @scheme[bitmap%].
 }

@defproc[(mode? [x any/c]) boolean?]{
 Determines if @scheme[x] is a mode suitable for
 constructing images. It can be one of
 @scheme['solid], @scheme["solid"], @scheme['outline],
 or @scheme["outline"], indicating if the shape is
 filled in or not.
}

@defproc[(image-color? [x any/c]) boolean?]{

  Determines if @scheme[x] represents a color. Strings, symbols,
  and @scheme[color] structs are allowed as colors.

  For example,
  @scheme["magenta"], @scheme["black"], @scheme['orange], and @scheme['purple]
  are allowed. Colors are not case-sensitive, so 
  @scheme["Magenta"], @scheme["Black"], @scheme['Orange], and @scheme['Purple]
  are also allowed, and are the same colors as in the previous sentence.
  If a string or symbol color name is not recognized, black is used in its place.
  
  The complete list of colors is available in the documentation for
  @scheme[color-database<%>].
                                      
}

@defstruct[color ([red (and/c natural-number/c (<=/c 255))]
                  [green (and/c natural-number/c (<=/c 255))]
                  [blue (and/c natural-number/c (<=/c 255))])]{
  The @scheme[color] struct defines a color with red, green, and blue components
      that range from @scheme[0] to @scheme[255].
}

@defproc[(y-place? [x any/c]) boolean?]{
  Determines if @scheme[x] is a placement option
  for the vertical direction. It can be one
  of
@scheme["top"],
@scheme['top], 
@scheme["bottom"],
@scheme['bottom],
@scheme["middle"],
@scheme['middle],
@scheme["center"],
@scheme['center],
@scheme["baseline"], or
@scheme['baseline].

The baseline of an image is the place where the bottoms any letters line up, not counting descenders, e.g. the tail on ``y'' or ``g'' or ``j''.


}

@defproc[(x-place? [x any/c]) boolean?]{
  Determines if @scheme[x] is a placement option
  for the horizontal direction. It can be one
  of @scheme["left"],
  @scheme['left], 
  @scheme["right"],
  @scheme['right],
  @scheme["middle"],
  @scheme['middle],
  @scheme["center"], or
  @scheme['center].
}

@defproc[(angle? [x any/c]) boolean?]{
  Determines if @scheme[x] is an angle, namely
  a real number between @scheme[0] (inclusive)
  and @scheme[360] (exclusive).
}

@defproc[(side-count? [x any/c]) boolean?]{
  Determines if @scheme[x] is an integer 
  greater than or equal to @scheme[3].
}

@defstruct[pen ([color image-color?]
                [width (and/c real? (<=/c 0 255))]
                [style pen-style?]
                [cap pen-cap?]
                [join pen-join?])]{
  The @scheme[pen] struct specifies how the drawing library draws lines. 
      
      
      A good default for @scheme[style] is @scheme["solid"], and
      good default values for the @scheme[cap] and @scheme[join] fields
      are @scheme["round"].
      
      Using @scheme[0] as a width is special; it means to always draw the 
      smallest possible, but visible, pen. This means that the pen will always
      be one pixel in size, no matter how the image is scaled.
}

@defproc[(pen-style? [x any/c]) boolean?]{
  Determines if @scheme[x] is a valid pen style.
  It can be one of
  @scheme["solid"], @scheme['solid], 
  @scheme["dot"], @scheme['dot], 
  @scheme["long-dash"], @scheme['long-dash], 
  @scheme["short-dash"], @scheme['short-dash], 
  @scheme["dot-dash"], or @scheme['dot-dash].
}

@defproc[(pen-cap? [x any/c]) boolean?]{
  Determines if @scheme[x] is a valid pen cap.
  It can be one of
  @scheme["round"], @scheme['round], 
  @scheme["projecting"], @scheme['projecting], 
  @scheme["butt"], or @scheme['butt].
}

@defproc[(pen-join? [x any/c]) boolean?]{
  Determines if @scheme[x] is a valid pen join.
  It can be one of
  @scheme["round"], @scheme['round], 
  @scheme["bevel"], @scheme['bevel], 
  @scheme["miter"], or @scheme['miter].
}

@section{Equality Testing of Images}

Two images are equal if they draw exactly the same way, at their current size
(not neccessarily at all sizes).

@section[#:tag "nitty-gritty"]{The nitty gritty of pixels, pens, and lines}

The image library treats coordinates as if they are in the upper-left corner 
of each pixel, and infinitesimally small.

Thus, when drawing a solid @scheme[square] of whose side-length is 10, the image library
colors in all of the pixels enclosed by the @scheme[square] starting at the upper
left corner of (0,0) and going down to the upper left corner of (10,10),
so the pixel whose upper left at (9,9) is colored in, but the pixel
at (10,10) is not. All told, 100 pixels get colored in, just as expected for
a @scheme[square] with a side length of 10.

When drawing lines, however, things get a bit more complex. Specifically, 
imagine drawing the outline of that rectangle. Since the border is
between the pixels, there really isn't a natural pixel to draw to indicate
the border. Accordingly, when drawing an outline @scheme[square] (without a 
@scheme[pen] specification, but just a color as the last argument), 
the image library uses a pen whose width is 1 pixel, but draws a line
centered at the point (0.5,0.5) that goes down and around to the point (10.5,10.5).
This means that the outline slightly exceeds the bounding box of the shape.
Specifically, the upper and left-hand lines around the square are within
the bounding box, but the lower and right-hand lines are just outside.

The special case of adding 0.5 to each coordinate when drawing the square
applies to all polygon-based shapes, but does not apply when a @scheme[pen]
is passed as the last argument to create the shape.
In that case, not adjustment of the pixels is performed and using a one
pixel wide pen draws the pixels above and below the line, but each with
a color that is half of the intensity of the given color. Using a
@scheme[pen] with with two, colors the pixels above and below the line
with the full intensity. 


@;-----------------------------------------------------------------------------
@section{Exporting Images to Disk}

In order to use an image as an input to another program (Photoshop, e.g., or 
a web browser), it is necessary to represent it in a format that these programs
can understand. The @scheme[save-image] function provides this functionality, 
writing an image to disk using the @tt{PNG} format. Since this
format represents an image using a set of pixel values, an image written to disk
generally contains less information than the image that was written, and cannot be scaled
or manipulated as cleanly (by any image program).

@defproc[(save-image [image image?] [filename path-string?]) boolean?]{
 writes an image to the path specified by @scheme[filename], using the
 @tt{PNG} format.}


