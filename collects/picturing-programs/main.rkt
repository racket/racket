#lang racket/base
(require 2htdp/universe
         (only-in htdp/error check-arg)
         picturing-programs/tiles
	 picturing-programs/io-stuff
	 picturing-programs/map-image
         picturing-programs/book-pictures)

(provide (all-from-out picturing-programs/tiles)   ; includes all-from-out 2htdp/image, plus a few simple add-ons
         (all-from-out picturing-programs/io-stuff) ; includes with-{input-from,output-to}-{string,file}, with-io-strings
	 (all-from-out picturing-programs/map-image)
	; includes (map,build)(3,4,)-image,  real->int, name->color, colorize, get-pixel-color
         (prefix-out pic: (all-from-out picturing-programs/book-pictures)) ; pic:calendar, pp:hacker, etc.
	 (all-from-out 2htdp/universe)
	 show-it)


(define (show-it img)
  (check-arg 'show-it (image? img) "image" "first" img)
  img)
