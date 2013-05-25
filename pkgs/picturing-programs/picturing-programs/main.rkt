#lang racket/base
(require 2htdp/universe
         (only-in htdp/error check-arg)
         picturing-programs/private/tiles
	 picturing-programs/private/io-stuff
	 picturing-programs/private/map-image
         picturing-programs/private/book-pictures)

(provide (all-from-out picturing-programs/private/tiles)   ; includes all-from-out 2htdp/image, plus a few simple add-ons
         (all-from-out picturing-programs/private/io-stuff) ; includes with-{input-from,output-to}-{string,file}, with-io-strings
	 (all-from-out picturing-programs/private/map-image)
	; includes (map,build)(3,4,)-image,  real->int, name->color, colorize, get-pixel-color
         (all-from-out picturing-programs/private/book-pictures) ; pic:calendar, pic:hacker, etc.
	 (all-from-out 2htdp/universe)
	 show-it)

(provide provide all-defined-out all-from-out rename-out except-out
         prefix-out struct-out)

(define (show-it img)
  (check-arg 'show-it (image? img) "image" "first" img)
  img)
