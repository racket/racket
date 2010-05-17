#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme teachpack/htdp/image))

@teachpack["image"]{Manipulating Images}

@defmodule[#:require-form beginner-require htdp/image]

The teachpack provides primitives for constructing and manipulating
images. Basic, colored images are created as outlines or solid
shapes. Additional primitives allow for the composition of images.

@;-----------------------------------------------------------------------------
@section{Images}

@defproc[(image? [x any/c]) boolean?]{Is @scheme[x] an image?}

@defproc[(image=? [x image?] [y image?]) boolean?]{Are @scheme[x] and
@scheme[y] the same image?}

@;-----------------------------------------------------------------------------
@section[#:tag "modes-colors"]{Modes and Colors}

@deftech{Mode} @scheme[(one-of/c 'solid 'outline "solid" "outline")]

A @tech{Mode} is used to specify whether painting a shape fills or
outlines the form.

@defstruct[color [(red (and/c natural-number/c (<=/c 255)))
                  (green (and/c natural-number/c (<=/c 255)))
                  (blue (and/c natural-number/c (<=/c 255)))]]

@deftech{RGB} @scheme[color?]

A @tech{RGB} describes a color via a shade of red, blue, and green colors
 (e.g., @scheme[(make-color 100 200 30)]).

@deftech{Color} @scheme[(or/c symbol? string? color?)] 

A @tech{Color} is a color-symbol (e.g., @scheme['blue]) or a color-string
 (e.g., @scheme["blue"]) or an @tech{RGB} structure.

@defproc[(image-color? [x any]) boolean?]{ Determines if the input is a
 valid image @tech{Color}.}

@;-----------------------------------------------------------------------------
@section[#:tag "creational"]{Creating Basic Shapes}

In DrRacket, you can insert images from your file system. Use PNG images
instead whenever possible for insertions. In addition, you can create basic
shapes with the following functions.

@defproc[(rectangle [w (and/c number? (or/c zero? positive?))] [h (and/c number? (or/c zero? positive?))] [m (unsyntax @tech{Mode})] [c (unsyntax @tech{Color})]) image?]{
 Creates a @scheme[w] by @scheme[h] rectangle, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(circle [r (and/c number? (or/c zero? positive?))] [m (unsyntax @tech{Mode})] [c (unsyntax @tech{Color})]) image?]{
 Creates a circle or disk of radius @scheme[r], filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(ellipse [w (and/c number? (or/c zero? positive?))] [h (and/c number? (or/c zero? positive?))] [m (unsyntax @tech{Mode})] [c (unsyntax @tech{Color})]) image?]{
 Creates a @scheme[w] by @scheme[h] ellipse, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(triangle [s number?] [m (unsyntax @tech{Mode})] [c (unsyntax @tech{Color})]) image?]{ 
 Creates an upward pointing equilateral
 triangle whose side is @scheme[s] pixels long, filled in according to
 @scheme[m] and painted in color @scheme[c]}

@defproc[(star [n (and/c number? (>=/c 2))]
               [outer (and/c number? (>=/c 1))]
               [inner (and/c number? (>=/c 1))]
               [m (unsyntax @tech{Mode})]
               [c (unsyntax @tech{Color})]) image?]{
 Creates a multi-pointed star with @scheme[n] points, an @scheme[outer]
 radius for the max distance of the points to the center, and
 an @scheme[inner] radius for the min distance to the center. }

@defproc[(regular-polygon [s side] [r number?] [m (unsyntax @tech{Mode})] [c (unsyntax @tech{Color})] [angle real? 0]) image?]{
Creates a regular polygon with @scheme[s] sides inscribed in
a circle of radius @scheme[r], using mode @scheme[m] and
color @scheme[c]. If an angle is specified, the polygon is rotated by that
angle.
}

@defproc[(line [x number?][y number?] [c (unsyntax @tech{Color})]) image?]{
 Creates a line colored @scheme[c] from (0,0) to @scheme[(x,y)].
 See @scheme[add-line] below.
}

@defproc[(text [s string?] [f (and/c number? positive?)] [c (unsyntax @tech{Color})]) Image]{
 Creates an image of the text @scheme[s] at point size @scheme[f] 
 and painted in color @scheme[c].}

@;-----------------------------------------------------------------------------
@section[#:tag "properties"]{Basic Image Properties}

To understand how images are manipulated, you need to understand the
basic properties of images. 

@defproc[(image-width [i image?]) integer?]{
 Obtain @scheme[i]'s width in pixels}

@defproc[(image-height [i image?]) integer?]{
 Obtain @scheme[i]'s height in pixels}

For the composition of images, you must know about @emph{pinholes}. Each
image, including primitive ones, come with a pinhole. For images created
with the above primitives, the pinhole is at the center of the shape except
for those created from @scheme[line] and @scheme[text].
The @scheme[text] function puts the pinhole at the upper left corner of
the image, and @scheme[line] puts the pinhole at the beginning of the line
(meaning that if the first two arguments to @scheme[line] are positive,
the pinhole is also in the upper left corner).
The pinhole can be moved, of course, and compositions
locate pinholes according to their own rules. When in doubt you can always
find out where the pinhole is and place it where convenient.

@defproc[(pinhole-x [i image?]) integer?]{Determines the @scheme[x]
 coordinate of the pinhole, measuring from the left of the image.}

@defproc[(pinhole-y [i image?]) integer?]{Determines the @scheme[y]
 coordinate of the pinhole, measuring from the top (down) of the image.}

@defproc[(put-pinhole [i image?] [x number?] [y number?]) image?]{
 Creates a new image with the pinhole in the location specified by
 @scheme[x] and @scheme[y], counting from the left and top (down),
 respectively.}

@defproc[(move-pinhole [i image?] [delta-x number?] [delta-y number?]) image?]{
 Creates a new image with the pinhole moved down and right by
 @scheme[delta-x] and @scheme[delta-y] with respect to its current
 location. Use negative numbers to move it up or left.}

@;-----------------------------------------------------------------------------
@section[#:tag "composition"]{Composing Images}

Images can be composed, and images can be found within compositions. 

@defproc[(add-line [i image?] 
                   [x1 number?]
                   [y1 number?]
                   [x2 number?]
                   [y2 number?]
                   [c (unsyntax @tech{Color})]) image?]{
Creates an image by adding a line (colored @scheme[c]) from 
(@scheme[x1],@scheme[y1]) to
(@scheme[x2],@scheme[y2])
to image @scheme[i].}

@defproc[(overlay [img image?] [img2 image?] [img* image?] ...) image?]{
Creates an image by overlaying all images on their pinholes.
The pinhole of the resulting image is the same place as the pinhole in the
first image. 
}

@defproc[(overlay/xy [img image?] [delta-x number?] [delta-y number?] [other image?]) image?]{
Creates an image by adding the pixels of @scheme[other] to
@scheme[img]. 

Instead of lining the two images up on their pinholes,
@scheme[other]'s pinhole is lined up on the point:
@schemeblock[
(make-posn (+ (pinhole-x img) delta-x)
           (+ (pinhole-y img) delta-y))
]

The pinhole of the resulting image is the same
place as the pinhole in the first image.

The same effect can be had by combining
@scheme[move-pinhole] and @scheme[overlay],
@schemeblock[
(overlay img 
         (move-pinhole other
                       (- delta-x)
                       (- delta-y)))]

}

@defproc[(image-inside? [img image?] [other image?]) boolean?]{
 Determines whether the pixels of the second image appear in the first.

Be careful when using this function with jpeg images. If you use an
image-editing program to crop a jpeg image and then save it,
@scheme[image-inside?] does not recognize the cropped image, due to
standard compression applied to JPEG images.}

@defproc[(find-image [img image?] [other image?]) posn?]{
 Determines where the pixels of the second image appear in the first, with
 respect to the pinhole of the first image. If @scheme[(image-inside? img
 other)] isn't true, @scheme[find-image] signals an error.}

@;-----------------------------------------------------------------------------
@section[#:tag "manipulation"]{Manipulating Images}

Images can also be shrunk. These ``shrink'' functions trim an image by
eliminating extraneous pixels. 

@defproc[(shrink-tl [img image?][width number?][height number?]) image?]{
Shrinks the image to a @scheme[width] by @scheme[height] image, starting
from the @emph{top-left} corner. The pinhole of the resulting image is in
the center of the image.}

@defproc[(shrink-tr [img image?][width number?][height number?]) image?]{
Shrinks the image to a @scheme[width] by @scheme[height] image, starting
from the @emph{top-right} corner. The pinhole of the resulting image is in
the center of the image.}

@defproc[(shrink-bl [img image?][width number?][height number?]) image?]{
Shrinks the image to a @scheme[width] by @scheme[height] image, starting
from the @emph{bottom-left} corner. The pinhole of the resulting image is in
the center of the image.}

@defproc[(shrink-br [img image?][width number?][height number?]) image?]{
Shrinks the image to a @scheme[width] by @scheme[height] image, starting
from the @emph{bottom-right} corner. The pinhole of the resulting image is in
the center of the image.}

@defproc[(shrink [img image?][left number?][above number?][right number?][below number?]) image?]{
Shrinks an image around its pinhole. The numbers are the pixels to save to
left, above, to the right, and below the pinhole, respectively. The pixel
directly on the pinhole is always saved.}

@;-----------------------------------------------------------------------------
@section[#:tag "scenes"]{Scenes}

A @deftech{scene} is an image, but with the pinhole in the upper-left corner, i.e. 
an image where @scheme[pinhole-x] and @scheme[pinhole-y] both return
@scheme[0].

Scenes are particularly useful with the
@schememodname[2htdp/universe]
and
@schememodname[htdp/world]
teachpacks, since it displays only @tech{scene}s in its canvas. 

@defproc[(scene? [x any/c]) boolean?]{Is @scheme[x] an scene?}

@defproc[(empty-scene [width natural-number/c]
                      [height natural-number/c])
         scene?]{
 creates a plain white, @scheme[width] x @scheme[height] @tech{scene}.}

@defproc[(place-image [img image?] [x number?] [y number?]
                      [s scene?])
         scene?]{
 creates a scene by placing @scheme[img] at
 @math{(@scheme[x], @scheme[y])} into @scheme[s];
 @math{(@scheme[x], @scheme[y])} are computer graphics coordinates,
 i.e., they count right and down from the upper-left corner.}


@defproc[(nw:rectangle [width natural-number/c] [height natural-number/c] [solid-or-outline Mode] [c Color]) image?]{
   creates a @scheme[width] by @scheme[height] rectangle, solid or outlined as specified by
   @scheme[solid-or-outline] and colored according to @scheme[c], with a pinhole at the upper left
   corner.}
   
@defproc[(scene+line [s scene?][x0 number?][y0 number?][x1 number?][y1 number?][c Color]) scene?]{
   creates a scene by placing a line of color @scheme[c] from
   @math{(@scheme[x0], @scheme[y0])} to @math{(@scheme[x1],
   @scheme[y1])} using computer graphics coordinates.  In contrast to
   the @scheme[add-line] function, @scheme[scene+line] cuts off those
   portions of the line that go beyond the boundaries of the given
   @scheme[s].}

@;-----------------------------------------------------------------------------
@section[#:tag "pixel-lists"]{Miscellaneous Image Manipulation and Creation}
 
The last group of functions extracts the constituent colors from an image
and converts a list of colors into an image.

@defthing[List-of-color list?]{is one of:}
@(begin
#reader scribble/comment-reader
(schemeblock
;; -- @scheme[empty]
;; -- @scheme[(cons @#,tech{Color} List-of-color)]
;; Interpretation: represents a list of colors.
))

@defproc[(image->color-list [img image?]) List-of-color]{
 Converts an image to a list of colors.}

@defproc[(color-list->image [l List-of-color]
           [width natural-number/c]
           [height natural-number/c]
           [x natural-number/c]
           [y natural-number/c]) image?]{
 Converts a list of colors @scheme[l] to an image with the given
 @scheme[width] and @scheme[height] and pinhole (@scheme[x],@scheme[y])
 coordinates, specified with respect to the top-left of the image.}

The remaining functions provide alpha-channel information as well. Alpha
channels are a measure of transparency; 0 indicates fully opaque and 255
indicates fully transparent.


@defstruct[alpha-color [(alpha (and/c natural-number/c (<=/c 255)))
                        (red (and/c natural-number/c (<=/c 255)))
                        (green (and/c natural-number/c (<=/c 255)))
                        (blue (and/c natural-number/c (<=/c 255)))]]{
  A structure representing an alpha color.}

@defproc[(image->alpha-color-list [img image?]) (list-of alpha-color?)]{
 to convert an image to a list of alpha colors}

 @defproc[(alpha-color-list->image
            [l (list-of alpha-color?)]
            [width integer?]
            [height integer?]
            [x integer?]
            [y integer?]) image?]{
 Converts a list of @scheme[alpha-color]s @scheme[l] to an image with the given
 @scheme[width] and @scheme[height] and pinhole (@scheme[x],@scheme[y])
 coordinates, specified with respect to the top-left of the image.}
