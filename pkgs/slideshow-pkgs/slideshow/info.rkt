#lang info

(define collection 'multi)

(define deps '("slideshow-lib" 
               "slideshow-exe"
               "slideshow-doc"))
(define implies '("slideshow-lib" 
                  "slideshow-exe"
                  "slideshow-doc"))

(define pkg-desc "Slide-presentation tool")

(define pkg-authors '(mflatt robby))
