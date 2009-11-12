#lang scheme/base

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


(require mrlib/image-core
         "private/image-more.ss")

(provide overlay
         overlay/places
         overlay/xy
         
         beside
         beside/places

         ;above
         ;above/places
         
         rotate
         frame
   
         scale
         scale/xy
         
         circle
         ellipse
         rectangle
         square
         rhombus
         regular-polygon
         star
         star-polygon
         triangle
         isosceles-triangle
         right-triangle
         line
         add-line
         text
         text/font
         
         x-place?
         y-place?
         image?
         mode?
         angle?
         side-count?
         
         image-width
         image-height)
