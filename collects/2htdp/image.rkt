#lang racket/base

#|

Need to test copy & paste. Also test that if the "if" 
expression in image-snipclass%'s read
method returns #f, then you get a black circle out.

---

improvments/changes wrt to htdp/image:

  - copying and pasting does not introduce jaggies
  - equal comparisons are more efficient
  - added rotation & scaling
  - got rid of pinholes (see the new overlay, beside, and above functions)
  - a bunch of new polygon functions

todo: sort out wxme library support (loading in text mode).

------------

From Matthias: (to use to compare with this library)


You asked about exercises and code snippets for HtDP/2e yesterday. I actually do have a bunch of stuff in

 svn: 2HtDP/SampleCode/

and they all have good sample contracts. (It is amazing what we can do with kids who have just a few weeks of cs down; I would have never dared to write an editor after six weeks in Algol.)


|#


(require (except-in "../mrlib/image-core.rkt" make-color color make-pen pen)
         "private/image-more.rkt"
         "private/img-err.rkt"
         (only-in lang/prim provide-primitive provide-primitives define-primitive)
         htdp/error)

(provide-primitives
         overlay
         overlay/align
         overlay/offset
         overlay/align/offset
         overlay/xy
         
         underlay
         underlay/align
         underlay/offset
         underlay/align/offset
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
         triangle/sss
         triangle/ssa
         triangle/sas
         triangle/ass
         triangle/aas
         triangle/asa
         triangle/saa
         isosceles-triangle
         right-triangle
         line
         add-line
         add-curve
         scene+line
         scene+curve
         text
         text/font
         
         image->color-list
         color-list->bitmap
         
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
         real-valued-posn?
         color-red color-blue color-green color-alpha color? color
         pen-color pen-width pen-style pen-cap pen-join 

         image-width
         image-height
         image-baseline

         put-pinhole
         clear-pinhole
         center-pinhole
         pinhole-x
         pinhole-y
         overlay/pinhole
         underlay/pinhole
         
         make-color
         make-pen pen
         pen?
         step-count?
         save-image
         save-svg-image
         
         freeze
         bitmap/url
         bitmap/file)

(provide bitmap
         empty-image)

(define-primitive make-color build-color/make-color)
(define-primitive color build-color/color)
(define-primitive make-pen build-pen/make-pen)
(define-primitive pen build-pen/pen)

#;
(provide (rename-out [build-color make-color])
         (rename-out [build-pen make-pen]))
