#lang racket/base
; Initial version, Dec. 13, 2010.
; Doesn't work with a literal image, but it works to use a "bitmap"
; reference to a file that's included with the teachpacks.  Dec. 21, 2010.

(require (only-in 2htdp/image bitmap))

(provide (prefix-out pic: (all-defined-out)))

(define bloch (bitmap "pictures/bloch.jpg"))
(define hieroglyphics (bitmap "pictures/small_hieroglyphics.png"))
(define hacker (bitmap "pictures/mad_hacker.png"))
(define book (bitmap "pictures/qbook.png"))
(define stick-figure (bitmap "pictures/stick-figure.png"))
(define scheme-logo (bitmap "pictures/schemelogo.png"))
(define calendar (bitmap "pictures/calendar.png"))
