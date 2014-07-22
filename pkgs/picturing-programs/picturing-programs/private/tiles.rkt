#lang racket/base
; Modified 1/19/2005 to be compatible with new image.ss contracts.
; Modified 2/16/2005 to include stuff from world.ss as well as image.ss
; Modified 2/17/2005 to provide on-update-event (which requires overriding a few
; functions from world.ss)
; Modified 5/20/2005 to rename on-update-event as on-redraw-event, and
; handle pinholes more consistently in image-beside and image-above.
; Modified 1/26/2006 to remove the functions I was replacing in image.ss
; (since image.ss now does things the way I wanted) and
; to remove my tweaked copy of world.ss (since world.ss now does things the
; way I wanted).
; Modified 7/12/2006 to allow image-beside and image-above to take variable numbers of arguments.
; Modified 7/26/2006 to add image-beside-align-top, image-beside-align-bottom, image-above-align-left, and image-above-align-right.
; Modified 12/17/2007 to add crop-top, crop-bottom, crop-left, crop-right.  Also utility functions slice-pic and unslice-pic.
; Modified 12/26/2007 to provide all-from image.ss, so we never have to mention image.ss at all.
; Modified 8/15/2008 to add image-above-align-center and image-beside-align-center.
; Modified 10/28/2008 to use provide/contract (and 4.x-style module specs, rather than (lib blah blah))
; Modified again 10/28/2008 to do more user-friendly "check-arg"-style checking instead.
; Modified 1/3/2009 to fix bugs in crop-* and unslice-pic related to zero-sized images.
; Modified 7/9/2009 for compatibility with DrScheme 4.2
; Modified 10/19/2009 for compatibility with DrScheme 4.2.2: image? is now in htdp/image, so it doesn't need to be required from htdp/advanced.
; Modified 1/12/2010: renamed image-above et al to above et al, image-beside et al to beside et al.
;   place-image and scene+line are now defined in sb-universe, so they don't need to be here.
; Modified 1/30/2010 for compatibility with 4.2.4: require 2htdp/private/universe-image, which
;   has a bunch of functions that accept both htdp-style images and 2htdp-style images.
; Modified 2/10/2010: replaced color-list with alpha-color-list to fix transparency bug.
; Modified 5/24/2010: eliminated all reference to pinholes, scenes, and htdp/image.
; Now using purely 2htdp/image, 2htdp/universe.  Downside: no reflection primitives.
; Modified 6/23/2010: had rotate-cw and rotate-ccw reversed.  And now we DO have reflection,
; so I'm putting it back in -- at least for vertical and horizontal axes.
; Modified 6/26/2010 to rename .ss to .rkt, lang scheme to lang racket, etc.
; Modified 7/2/2010: I did NOT have rotate-cw and rotate-ccw reversed; there's a bug in
; rotate that causes them to work incorrectly on bitmaps.  Reversing them back.
; Modified 12/13/2010: Added flip-main and flip-other (formerly known as
; reflect-main-diag and reflect-other-diag, which had been disabled for
; a while).
; Modified 8/??/2011: reworded a bunch of error messages in accordance
; with Guillaume & Kathi's research.
; Modified 10/??/2011: fixed error messages in crop functions.
; Modified 10/20/2012: moved error-message tests into this file

  (require
   2htdp/image
   lang/error ; check-arg, check-image, etc.
   )

  (provide
   (all-from-out 2htdp/image)
   ; above above-align-right above-align-left above-align-center   ; included in 2htdp/image
   ; beside beside-align-top beside-align-bottom beside-align-center ; include in 2htdp/image
   reflect-vert reflect-horiz ; synonyms for flip-vertical and flip-horizontal, respectively
   reflect-main-diag reflect-other-diag
   flip-main flip-other ; synonyms for the above
   rotate-cw rotate-ccw rotate-180 ; will simply call rotate
   ; show-pinhole   ; what's a pinhole?
   crop-top crop-bottom crop-left crop-right)  ; will simply call crop

  
  ;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))
  

  ; reflect-horiz : image => image
  (define (reflect-horiz picture)
    (check-image 'reflect-horiz picture "first")
    (flip-horizontal picture))
  
  ; reflect-vert : image => image
  (define (reflect-vert picture)
    (check-image 'reflect-vert picture "first")
    (flip-vertical picture))
  
  ; reflect-main-diag : image => image
  (define (reflect-main-diag picture)
    (check-image 'reflect-main-diag picture "first")
    (rotate -45 (flip-vertical (rotate 45 picture))))
  ; there ought to be a more efficient way than this....
  
  ; reflect-other-diag : image => image
  (define (reflect-other-diag picture)
    (check-image 'reflect-other-diag picture "first")
    (rotate 45 (flip-vertical (rotate -45 picture))))
  
  ; synonyms
  (define (flip-main picture) 
    (check-image 'flip-main picture "first")
    (reflect-main-diag picture))
  (define (flip-other picture)
    (check-image 'flip-other picture "first")
    (reflect-other-diag picture))

  ; natural-number? anything -> boolean
  (define (natural-number? x)
    (and (integer? x) (>= x 0)))
  
  ; natural-bounded? natural anything -> boolean
  (define (natural-bounded? max x)
    (and (natural-number? x) (<= x max)))

  ; crop-left : image natural-number -> image
  ; deletes that many pixels from left edge of image
  (define (crop-left picture pixels)
    (check-image 'crop-left picture "first")
    (check-arg 'crop-left (natural-bounded? (image-width picture) pixels)
	(format "natural number <= ~a" (image-width picture))
	 "second" pixels)
    (crop pixels 0
          (- (image-width picture) pixels) (image-height picture)
          picture))
  
  ; crop-top : image number -> image
  ; deletes that many pixels from top edge of image
  (define (crop-top picture pixels)
    (check-image 'crop-top picture "first")
    (check-arg 'crop-top (natural-bounded? (image-height picture) pixels)
	(format "natural number <= ~a" (image-height picture))
	 "second" pixels)
    (crop 0 pixels 
          (image-width picture) (- (image-height picture) pixels)
          picture))
  
  ; crop-right : image number -> image
  ; deletes that many pixels from right edge of image
  (define (crop-right picture pixels)
    (check-image 'crop-right picture "first")
    (check-arg 'crop-right (natural-bounded? (image-width picture) pixels)
	(format "natural number <= ~a" (image-width picture))
	 "second" pixels)
    (crop 0 0
          (- (image-width picture) pixels)
          (image-height picture)
          picture))
  
  ; crop-bottom : image number -> image
  ; deletes that many pixels from bottom edge of image
  (define (crop-bottom picture pixels)
    (check-image 'crop-bottom picture "first")
    (check-arg 'crop-bottom (natural-bounded? (image-height picture) pixels)
	(format "natural number <= ~a" (image-height picture))
	 "second" pixels)
    (crop 0 0
          (image-width picture)
          (- (image-height picture) pixels)
          picture))
  
  
  ; rotate-cw : image => image
  (define (rotate-cw picture)
    (check-image 'rotate-cw picture "first")
    (rotate -90 picture))
  
  ; rotate-ccw : image => image
  ; Ditto.
  (define (rotate-ccw picture)
    (check-image 'rotate-ccw picture "first")
    (rotate 90 picture))
  
  ; rotate-180 : image => image
  (define (rotate-180 picture)
    (check-image 'rotate-180 picture "first")
    (rotate 180 picture))

  
  (module+ test
           (require "book-pictures.rkt")
(require test-engine/racket-tests)
(check-error (reflect-horiz 17)
             "reflect-horiz: expected <image> as first argument, given 17")
; (check-error (reflect-horiz pic:hacker) "shouldn't actually error out")
; (check-expect 3 4)
(check-error (reflect-vert "hello")
             "reflect-vert: expected <image> as first argument, given \"hello\"")
(check-error (reflect-main-diag #t)
             "reflect-main-diag: expected <image> as first argument, given #t")
(check-error (reflect-other-diag #f)
             "reflect-other-diag: expected <image> as first argument, given #f")
(check-error (flip-main 'blue)
             "flip-main: expected <image> as first argument, given 'blue")
(check-error (flip-other "snark")
             "flip-other: expected <image> as first argument, given \"snark\"")
(check-error (crop-left pic:hacker 50)
             "crop-left: expected <natural number <= 48> as second argument, given 50")
(check-error (crop-right pic:bloch 100)
             "crop-right: expected <natural number <= 93> as second argument, given 100")
(check-error (crop-top pic:book 56)
             "crop-top: expected <natural number <= 39> as second argument, given 56")
(check-error (crop-bottom pic:hacker 56)
             "crop-bottom: expected <natural number <= 48> as second argument, given 56")
(check-error (crop-left pic:hacker -3)
             "crop-left: expected <natural number <= 48> as second argument, given -3")
(check-error (crop-top pic:book 3.2)
             "crop-top: expected <natural number <= 39> as second argument, given 3.2")
(check-error (crop-bottom pic:book pic:book)
             "crop-bottom: expected <natural number <= 39> as second argument, given (object:image% ...)") ; was "<image>" in *SL, but "(object:image% ...)" in racket/base
(check-error (rotate-cw 17)
             "rotate-cw: expected <image> as first argument, given 17")
(check-error (rotate-ccw #t)
             "rotate-ccw: expected <image> as first argument, given #t")
(check-error (rotate-180 "goodbye")
             "rotate-180: expected <image> as first argument, given \"goodbye\"")
(test) ; need this if not using *SL testing
)
