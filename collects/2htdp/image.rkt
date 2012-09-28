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
         htdp/error
         (for-syntax racket/base))

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
         make-pen
         step-count?
         save-image
         save-svg-image
         
         freeze
         bitmap/url
         bitmap/file)

(provide bitmap
         empty-image)

(define-primitive make-color build-color/make-color)
(define-primitive color build-color/color
  (list #f
        #'color #':color? 
        (list #':color-alpha  #':color-blue #':color-green #':color-red)
        (list #f #f #f #f)
        #f))
(define-primitive make-pen build-pen/make-pen)
(define-primitive pen build-pen/pen
  (list #f
        #'pen #':pen? 
        (list #':pen-join #':pen-cap #':pen-style #':pen-width #':pen-color)
        (list #f #f #f #f #f)
        #f))
(provide color pen)

(define-primitive :color-red color-red)
(define-primitive :color-blue color-blue)
(define-primitive :color-green color-green)
(define-primitive :color-alpha color-alpha)
(define-primitive :color? color?)
(define-primitive :pen-color pen-color)
(define-primitive :pen-width pen-width)
(define-primitive :pen-style pen-style)
(define-primitive :pen-cap pen-cap)
(define-primitive :pen-join pen-join)
(define-primitive :pen? pen?)

(provide (rename-out [:color-red color-red]
                     [:color-blue color-blue]
                     [:color-green color-green]
                     [:color-alpha color-alpha]
                     [:color? color?]
                     [:pen-color pen-color]
                     [:pen-width pen-width]
                     [:pen-style pen-style]
                     [:pen-cap pen-cap]
                     [:pen-join pen-join]
                     [:pen? pen?]))
