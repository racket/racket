#lang racket
(require 2htdp/universe
         htdp/error ; check-arg
         "tiles.rkt"
         "io-stuff.rkt"
	 "map-image.rkt"
         "book-pictures.rkt")
(provide (all-from-out "tiles.rkt")   ; includes all-from-out 2htdp/image, plus a few simple add-ons
         (all-from-out "io-stuff.rkt") ; includes with-{input-from,output-to}-{string,file}, with-io-strings
	 (all-from-out "map-image.rkt") ; includes (map,build)-[masked-]image,  real->int, maybe-color?, name->color,
	 				; get-pixel-color, pixel-visible?
         (prefix-out pic: (all-from-out "book-pictures.rkt")) ; pic:calendar, pp:hacker, etc.
	 )
(provide show-it)
(provide (all-from-out 2htdp/universe))


(define (show-it img)
  (check-arg 'show-it (image? img) "image" "first" img)
  img)
