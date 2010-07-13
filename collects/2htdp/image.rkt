#lang racket/base

#|

Need to test copy & paste. Also test that if the "if" 
expression in image-snipclass%'s read
method returns #f, then you get a black circle out.

---

improvments/changes wrt to htdp/image:

  - copying and pasting does not introduce jaggies
  - equal comparisions are more efficient
  - added rotation & scaling
  - got rid of pinholes (see the new overlay, beside, and above functions)
  - a bunch of new polygon functions

Equality change: equality is now based on the structure of the construction of the picture. 
This means that some equalities that were there before are no longer true. For example,
in the old library, these two images are the same:

  (overlay/xy (rectangle 100 10 'solid 'red)
               0
               10
               (rectangle 100 10 'solid 'red))

  (rectangle 100 20 'solid 'red)

... and why aren't they the same again....?!

todo: sort out wxme library support (loading in text mode).

------------

From Matthias: (to use to compare with this library)


You asked about exercises and code snippets for HtDP/2e yesterday. I actually do have a bunch of stuff in

 svn: 2HtDP/SampleCode/

and they all have good sample contracts. (It is amazing what we can do with kids who have just a few weeks of cs down; I would have never dared to write an editor after six weeks in Algol.)


|#


(require (except-in "../mrlib/image-core.ss" make-color make-pen)
         "private/image-more.ss"
         "private/img-err.ss"
         (only-in lang/prim provide-primitive provide-primitives define-primitive)
         htdp/error)

(provide-primitives
         overlay
         overlay/align
         overlay/xy
         underlay
         underlay/align
         underlay/xy
         
         beside
         beside/align

         above
         above/align
         
	 crop
         rotate
         flip-horizontal
         flip-vertical
         frame
         place-image
         place-image/align
         
         scale
         scale/xy
         
         circle
         ellipse
         rectangle
         empty-scene
         square
         rhombus
         regular-polygon
         polygon
         star
         star-polygon
         radial-star
         triangle
         isosceles-triangle
         right-triangle
         line
         add-line
         add-curve
         scene+line
         scene+curve
         text
         text/font
         
         x-place?
         y-place?
         image?
         mode?
         angle?
         side-count?
         image-color?
         pen-style? 
         pen-cap?
         pen-join?
         color-red color-blue color-green color? color
         pen-color pen-width pen-style pen-cap pen-join pen

         image-width
         image-height
         image-baseline
         
         make-color
         make-pen
         pen?
         step-count?
         save-image)

(provide bitmap)


(define-primitive make-color build-color)
(define-primitive make-pen build-pen)

#;
(provide (rename-out [build-color make-color])
         (rename-out [build-pen make-pen]))
