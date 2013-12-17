#lang racket/base
; Spring 2010: started trying to get this to work.
; Late June 2010: Got build-image and map-image working.
; Added name->color and get-pixel-color.
; Added build-masked-image and map-masked-image.
; July 6, 2010: added change-to-color
; July 28, 2010: added map3-image and build3-image.  Is change-to-color really useful?
; Dec. 26, 2010: added color=? to export (duh!)
; Dec. 26, 2010: API for bitmaps has changed for 5.1, so I need to rewrite to match it.
; Dec. 28, 2010: Robby added alphas into the "color" type, and provided an implementation
; of map-image.  He recommends using racket/draw bitmaps rather than 2htdp/image bitmaps.
; May 10, 2011: added build-image/extra and map-image/extra.
; Dec 1, 2011: allowed map-image and map-image/extra to give their
; function x and y or not, depending on their arity.  This way one
; can write a function from color to color, and immediately map it
; onto an image.
; Apr 27, 2012: get-pixel-color has long had a "cache" of one image so it doesn't need
; to keep re-rendering.  Experimenting with increasing this cache to two images, so we
; can call get-pixel-color on two images in alternation without thrashing.  The cache
; itself seems to work, and having the cache size >= the number of images DOES improve
; performance for a series of get-pixel-color calls rotating among several images (each
; render seems to take about a ms).
; Apr 28, 2012: added fold-image and fold-image/extra.

(require (except-in racket/draw make-color make-pen)
         racket/snip
         racket/class
         2htdp/image
         (only-in htdp/error natural?)
         (only-in mrlib/image-core render-image))
;(require picturing-programs/book-pictures)

;(require mrlib/image-core)
;(require 2htdp/private/image-more)
;; (require 2htdp/private/img-err)
;(require scheme/gui)
(require lang/prim)

(provide-primitives real->int
                    ; maybe-color?
                    name->color
                    colorize
                    get-pixel-color
                    ;pixel-visible?
                    ; change-to-color
                    color=?
                    ;                    show-cache
                    )
(provide-higher-order-primitive map-image (f _))
(provide-higher-order-primitive map3-image (rfunc gfunc bfunc _))
(provide-higher-order-primitive map4-image (rfunc gfunc bfunc afunc _))
;(provide-higher-order-primitive map-masked-image (f _))
(provide-higher-order-primitive build-image (_ _ f))
(provide-higher-order-primitive build3-image (_ _ rfunc gfunc bfunc))
(provide-higher-order-primitive build4-image (_ _ rfunc gfunc bfunc afunc))
;(provide-higher-order-primitive build-masked-image (_ _ f))
(provide-higher-order-primitive build-image/extra (_ _ f _))
(provide-higher-order-primitive map-image/extra (f _ _))
(provide-higher-order-primitive fold-image (f _ _))
(provide-higher-order-primitive fold-image/extra (f _ _ _))


(module+ test
         (require "book-pictures.rkt" test-engine/racket-tests)
         )

; check-procedure-arity : alleged-function nat-num symbol string
; Note: if you invoke these things from a BSL or BSLL program, the syntax checker will
; catch non-procedure arguments before the "(and (procedure? f) ..." test ever sees them,
; but that's no longer true if you invoke them from an ISLL, ASL, or racket program,
; so I'm keeping the test.
(define (check-procedure-arity f n func-name msg)
  (unless (and (procedure? f) (procedure-arity-includes? f n))
    (error func-name msg)))

(define transparent (make-color 0 0 0 0))

(define (maybe-color? thing)
  (or (color? thing)
      (eqv? thing #f)
      ; (image-color? thing) ; handles string & symbol color names
      ))

(define (broad-color? thing)
  (or (maybe-color? thing)
      (image-color? thing)))

; color->color% : does the obvious
; Note that color% doesn't have an alpha component, so alpha is lost.
(define (color->color% c)
  (if (string? c) 
      c
      (make-object color% 
        (color-red c)
        (color-green c)
        (color-blue c))))

; color%->color : does the obvious, with alpha defaulting to full-opaque.
(define (color%->color c)
  (make-color (send c red)
              (send c green)
              (send c blue)))

; name->color : string-or-symbol -> maybe-color
(define (name->color name)
  (unless (or (string? name) (symbol? name))
    (error 'name->color 
           (format "Expected a string or symbol, but received ~v" name)))
  (let [[result (send the-color-database find-color 
                      (if (string? name)
                          name
                          (symbol->string name)))]]
    (if result
        (color%->color result)
        #f)))

(module+ test
         (check-expect (name->color "red") (make-color 255 0 0 255))
         (check-expect (name->color "plaid") #f)
         (check-error (name->color 7 "name->color: Expected a string or symbol, but received 7"))
         )

; colorize : broad-color -> color  -- returns #f for unrecognized names
(define (colorize thing)
  (cond [(color? thing) thing]
        [(eqv? thing #f) transparent]
        [(image-color? thing) (name->color thing)]
        [else (error 'colorize (format "Expected a color, but received ~v" thing))]))

; colorize-func : (... -> broad-color) -> (... -> color)
(define (colorize-func f)
  (compose colorize f))


;; natural? : anything -> boolean
;(define (natural? it)
;  (and (integer? it)
;       (>= it 0)))

; color=? : broad-color broad-color -> boolean
(define (color=? c1 c2)
  (let [[rc1 (colorize c1)]
        [rc2 (colorize c2)]]
    (unless (color? rc1)
      (error 'color=?
             (format "Expected a color or color name as first argument, but received ~v" c1)))
    (unless (color? rc2)
      (error 'color=?
             (format "Expected a color or color name as second argument, but received ~v" c2)))
    (and (= (color-alpha rc1) (color-alpha rc2)) ; Both alphas MUST be equal.
         (or (= (color-alpha rc1) 0)             ; If both are transparent, ignore rgb.
             (and (= (color-red rc1) (color-red rc2))
                  (= (color-green rc1) (color-green rc2))
                  (= (color-blue rc1) (color-blue rc2)))))))

(module+ test
         (check-expect (color=? "red" (make-color 255 0 0)) #t)
         (check-expect (color=? (make-color 0 255 0) 'green) #t)
         (check-expect (color=? "red" (make-color 255 0 1)) #f)
         (check-expect (color=? (make-color 0 255 0 254) 'green) #f)
         (check-expect (color=? (make-color 255 0 0 0) (make-color 0 255 0 0)) #t) ; if both alphas are 0...
         (check-error (color=? 87 (make-color 87 87 87)) "colorize: Expected a color, but received 87")
         (check-error (color=? "red" #t) "colorize: Expected a color, but received #t")
         )



(define (real->int num)
  (inexact->exact (round num)))


; get-px : x y w h bytes -> color
(define (get-px x y w h bytes)
  (define offset (* 4 (+ x (* y w))))
  (make-color (bytes-ref bytes (+ offset 1))
              (bytes-ref bytes (+ offset 2))
              (bytes-ref bytes (+ offset 3))
              (bytes-ref bytes offset)))

; set-px! : bytes x y w h color -> void
(define (set-px! bytes x y w h new-color)
  (define offset (* 4 (+ x (* y w))))
  (bytes-set! bytes offset (color-alpha new-color))
  (bytes-set! bytes (+ offset 1) (color-red new-color))
  (bytes-set! bytes (+ offset 2) (color-green new-color))
  (bytes-set! bytes (+ offset 3) (color-blue new-color)))

; get-pixel-color : x y image -> color
; This will remember the last CACHE-SIZE images on which it was called.
; Really terrible performance if you call it in alternation
; on CACHE-SIZE+1 different images, but should be OK if you call it
; lots of times on the same image.
; Returns transparent if you ask about a position outside the picture.

(define CACHE-SIZE 3)
(define-struct ib (image bytes) #:transparent)
; A cache is a list of at most CACHE-SIZE ib's.
; search-cache: image cache -> bytes or #f
(define (search-cache pic cache)
  (cond [(null? cache) #f]
        [(eqv? pic (ib-image (car cache))) (ib-bytes (car cache))]
        [else (search-cache pic (cdr cache))]))

; We'll do a simple LRU cache-replacement.

; add-and-drop : ib cache -> cache
; preserves size
(define (add-and-drop new-ib cache)
  (cons new-ib (drop-last cache)))

; drop-last : non-empty list -> list
(define (drop-last L)
  (cond [(null? L) (error 'drop-last "list is empty")]
        [(null? (cdr L)) '()]
        [else (cons (car L) (drop-last (cdr L)))]))

(define cache (build-list CACHE-SIZE (lambda (n) (ib #f #f))))

(define (show-cache) (map ib-image cache)) ; exported temporarily for debugging

(define (get-pixel-color x y pic)
  (let* [(w (image-width pic))
         (h (image-height pic))
         (bytes
          (or (search-cache pic cache)
              (let* [(bm (make-bitmap w h))
                     (bmdc (make-object bitmap-dc% bm))
                     (new-bytes (make-bytes (* 4 w h)))]
                (render-image pic bmdc 0 0)
                (send bmdc set-bitmap #f)
                (send bm get-argb-pixels 0 0 w h new-bytes)
                (set! cache (add-and-drop (ib pic new-bytes) cache))
                new-bytes)))]
    
    (if (and (<= 0 x (sub1 w))
             (<= 0 y (sub1 h)))
        (get-px x y w h bytes)
        transparent))
  )

; build-image-internal : natural(width) natural(height) (nat nat -> color) -> image
(define (build-image-internal w h f)
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (define bytes (make-bytes (* w h 4)))
  (for* ((y (in-range 0 h))
         (x (in-range 0 w)))
        (set-px! bytes x y w h (f x y))
        )
  (send bm set-argb-pixels 0 0 w h bytes)
  (make-object image-snip% bm))

; build-image : natural(width) natural(height) (nat nat -> broad-color) -> image
(define (build-image w h f)
  (unless (natural? w)
    (error 'build-image
           (format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build-image
           (format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity f 2 'build-image "Expected a function with contract num(x) num(y) -> color as third argument")
  (build-image-internal w h (colorize-func f)))

(module+ test
         (check-expect (build-image 50 30 (lambda (x y) "red"))
                       (rectangle 50 30 "solid" "red"))
         (check-error (build-image "a" 30 (lambda (x y) "red"))
                      "build-image: Expected a natural number as first argument, but received \"a\"")
         (check-error (build-image 50 #f (lambda (x y) "red"))
                      "build-image: Expected a natural number as second argument, but received #f")
         (check-error (build-image 50 30 70)
                      "build-image: Expected a function with contract num(x) num(y) -> color as third argument")
         (check-error (build-image 50 30 add1)
                      "build-image: Expected a function with contract num(x) num(y) -> color as third argument")
         (check-error (build-image 50 30 +)
                      "colorize: Expected a color, but received 0")
         )

; build-image/extra : natural(width) natural(height) (nat nat any -> broad-color) any -> image
; Like build-image, but passes a fixed extra argument to every call of the function.
; For students who don't yet know function closures.
(define (build-image/extra w h f extra)
  (unless (natural? w)
    (error 'build-image/extra
           (format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build-image/extra
           (format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity f 3 'build-image/extra "Expected a function with contract num(x) num(y) any -> color as third argument")
  (build-image-internal w h
                        (colorize-func (lambda (x y) (f x y extra)))))
(module+ test
         (check-expect (build-image/extra 50 30 (lambda (x y dummy) "red") "blue")
                       (rectangle 50 30 "solid" "red"))
         (check-error (build-image/extra "a" 30 (lambda (x y dummy) "red") "blue")
                      "build-image/extra: Expected a natural number as first argument, but received \"a\"")
         (check-error (build-image/extra 50 #f (lambda (x y dummy) "red") "blue")
                      "build-image/extra: Expected a natural number as second argument, but received #f")
         (check-error (build-image/extra 50 30 70 "blue")
                      "build-image/extra: Expected a function with contract num(x) num(y) any -> color as third argument")
         (check-error (build-image/extra 50 30 add1 "blue")
                      "build-image/extra: Expected a function with contract num(x) num(y) any -> color as third argument")
         (check-error (build-image/extra 50 30 + 7)
                      "colorize: Expected a color, but received 7")
         )


; check-component : anything symbol string -> anything
; returns first argument unaltered if it's an integer in [0-255]
(define (check-component it plaintiff message)
  (if (and (integer? it) (>= it 0) (<= it 255))
      it
      (error plaintiff message)))

; build3-image : nat(width) nat(height) rfunc gfunc bfunc -> image
; where each of rfunc, gfunc, bfunc is (nat(x) nat(y) -> nat)
(define (build3-image w h rfunc gfunc bfunc)
  (unless (natural? w)
    (error 'build3-image
           (format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build3-image
           (format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity rfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> [0-255] as third argument")
  (check-procedure-arity gfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> [0-255] as fourth argument")
  (check-procedure-arity bfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> [0-255] as fifth argument")
  (build-image-internal w h
                        (lambda (x y)
                          (make-color (check-component (rfunc x y) 'build3-image "Expected third argument to return integer in range 0-255")
                                      (check-component (gfunc x y) 'build3-image "Expected fourth argument to return integer in range 0-255")
                                      (check-component (bfunc x y) 'build3-image "Expected fifth argument to return integer in range 0-255")
                                      )))
  )

(module+ test
         (check-expect (build3-image 7 7 (lambda (x y) 0) (lambda (x y) 255) (lambda (x y) 0))
                       (square 7 "solid" "green"))
         (check-error (build3-image 100 100 add1 + +)
                      "build3-image: Expected a function with contract num(x) num(y) -> [0-255] as third argument")
         (check-error (build3-image 100 100 + add1 +)
                      "build3-image: Expected a function with contract num(x) num(y) -> [0-255] as fourth argument")
         (check-error (build3-image 100 100 + + add1)
                      "build3-image: Expected a function with contract num(x) num(y) -> [0-255] as fifth argument")
         (check-error (build3-image 100 100 * + +)
                      "build3-image: Expected third argument to return integer in range 0-255") ; too big
         (check-error (build3-image 100 100 + - +)
                      "build3-image: Expected fourth argument to return integer in range 0-255") ; too small
         (check-error (build3-image 100 100 + + (compose sqrt +))
                      "build3-image: Expected fifth argument to return integer in range 0-255") ; not an integer
         )


; build4-image : nat(width) nat(height) rfunc gfunc bfunc afunc -> image
; where each of rfunc, gfunc, bfunc, afunc is (nat(x) nat(y) -> nat)
(define (build4-image w h rfunc gfunc bfunc afunc)
  (unless (natural? w)
    (error 'build4-image
           (format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build4-image
           (format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity rfunc 2 'build4-image "Expected a function with contract num(x) num(y) -> [0-255] as third argument")
  (check-procedure-arity gfunc 2 'build4-image "Expected a function with contract num(x) num(y) -> [0-255] as fourth argument")
  (check-procedure-arity bfunc 2 'build4-image "Expected a function with contract num(x) num(y) -> [0-255] as fifth argument")
  (check-procedure-arity afunc 2 'build4-image "Expected a function with contract num(x) num(y) -> [0-255] as sixth argument")
  (build-image-internal w h
                        (lambda (x y)
                          (make-color (check-component (rfunc x y) 'build4-image "Expected third argument to return integer in range 0-255")
                                      (check-component (gfunc x y) 'build4-image "Expected fourth argument to return integer in range 0-255")
                                      (check-component (bfunc x y) 'build4-image "Expected fifth argument to return integer in range 0-255")
                                      (check-component (afunc x y) 'build4-image "Expected sixth argument to return integer in range 0-255")
                                      ))))

(module+ test
         (check-expect (build4-image 5 3
                                     (lambda (x y) 0)
                                     (lambda (x y) 0)
                                     (lambda (x y) 255)
                                     (lambda (x y) 127))
                       (rectangle 5 3 "solid" (make-color 0 0 255 127)))
         (check-error (build4-image 100 100 add1 + + +)
                      "build4-image: Expected a function with contract num(x) num(y) -> [0-255] as third argument")
         (check-error (build4-image 100 100 + add1 + +)
                      "build4-image: Expected a function with contract num(x) num(y) -> [0-255] as fourth argument")
         (check-error (build4-image 100 100 + + add1 +)
                      "build4-image: Expected a function with contract num(x) num(y) -> [0-255] as fifth argument")
         (check-error (build4-image 100 100 + + + add1)
                      "build4-image: Expected a function with contract num(x) num(y) -> [0-255] as sixth argument")
         (check-error (build4-image 100 100 + + + (lambda (x y) "hello world"))
                      "build4-image: Expected sixth argument to return integer in range 0-255") ; not even a number
         )

; map-image-internal : (int int color -> color) image -> image
(define (map-image-internal f img)
  (define w (image-width img))
  (define h (image-height img))
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (render-image img bdc 0 0)
  (send bdc set-bitmap #f)
  (define bytes (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h bytes)
  (for* ((y (in-range 0 h))
         (x (in-range 0 w)))
        (define answer (f x y (get-px x y w h bytes)))
        (if (color? answer)
            (set-px! bytes x y w h answer)
            (error 'map-image "Expected a function that returns a color")))
  (send bm set-argb-pixels 0 0 w h bytes)
  (make-object image-snip% bm))

; map-image : ([int int] color -> broad-color) image -> image
(define (map-image f img)
  (unless (image? img)
    (error 'map-image
           (format "Expected an image as second argument, but received ~v" img)))
  (cond [(procedure-arity-includes? f 3)
         (map-image-internal (colorize-func f) img)]
        [(procedure-arity-includes? f 1)            ; allow f : color->color as a simple case
         (map-image-internal (colorize-func (lambda (x y c) (f c))) img)]
        [else (error 'map-image "Expected a function of one or three parameters, returning a color, as first argument")]))


(module+ test
         (check-expect (map-image (lambda (c) "blue") (rectangle 5 3 "solid" "green"))
                       (rectangle 5 3 "solid" "blue"))
         (check-expect (map-image (lambda (x y c) (if (< x 5) "blue" "green")) (rectangle 10 3 "solid" "red"))
                       (beside (rectangle 5 3 "solid" "blue") (rectangle 5 3 "solid" "green")))
         (check-error (map-image (lambda (c) "blue") "green")
                      "map-image: Expected an image as second argument, but received \"green\"")
         (check-error (map-image (lambda (c) 0) (rectangle 5 3 "solid" "green"))
                      "colorize: Expected a color, but received 0")
         )

; map-image/extra : (nat nat color X -> broad-color) image X -> image
; Like map-image, but passes a fixed extra argument to every call of the function.
; For students who don't yet know function closures.
(define (map-image/extra f img extra)
  (unless (image? img)
    (error 'map-image/extra
           (format "Expected an image as second argument, but received ~v" img)))
  (cond [(procedure-arity-includes? f 4)
         (map-image-internal (colorize-func (lambda (x y c) (f x y c extra))) img)]
        [(procedure-arity-includes? f 2)
         (map-image-internal (colorize-func (lambda (x y c) (f c extra))) img)]
        [else (error 'map-image/extra "Expected a function taking two or four parameters, returning a color, as first argument")]))




; The version for use before students have seen structs:
; map3-image :
; (int(x) int(y) int(r) int(g) int(b) -> int(r))
; (int(x) int(y) int(r) int(g) int(b) -> int(g))
; (int(x) int(y) int(r) int(g) int(b) -> int(b))
; image -> image
; Note: by default, preserves alpha values from old image.
(define (map3-image rfunc gfunc bfunc pic)
  (check-procedure-arity rfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as first argument")
  (check-procedure-arity gfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as second argument")
  (check-procedure-arity bfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as third argument")
  (unless (image? pic)
    (error 'map3-image
           (format "Expected an image as fourth argument, but received ~v" pic)))
  (map-image-internal
   (lambda (x y c)
     (define r (color-red c))
     (define g (color-green c))
     (define b (color-blue c))
     (make-color (check-component (rfunc x y r g b) 'map3-image "Expected first argument to return integer in range 0-255")
                 (check-component (gfunc x y r g b) 'map3-image "Expected second argument to return integer in range 0-255")
                 (check-component (bfunc x y r g b) 'map3-image "Expected third argument to return integer in range 0-255")
                 (color-alpha c)))
   pic))

(module+ test
         (check-error (map3-image add1 + + pic:bloch)
                      "map3-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as first argument")
         (check-error (map3-image + add1 + pic:bloch)
                      "map3-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as second argument")
         (check-error (map3-image + + add1 pic:bloch)
                      "map3-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> [0-255] as third argument")
         (check-error (map3-image + + + 17)
                      "map3-image: Expected an image as fourth argument, but received 17")
         (check-error (map3-image - max max (rectangle 5 3 "solid" "blue")) ; too small
                      "map3-image: Expected first argument to return integer in range 0-255")
         (check-error (map3-image max - max (rectangle 5 3 "solid" "blue"))
                      "map3-image: Expected second argument to return integer in range 0-255")
         (check-error (map3-image max max - (rectangle 5 3 "solid" "blue"))
                      "map3-image: Expected third argument to return integer in range 0-255")
         (check-error (map3-image + max max (rectangle 5 3 "solid" "blue"))
                      "map3-image: Expected first argument to return integer in range 0-255") ; too big
)

; map4-image :
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(r))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(g))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(b))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(a))
;  image -> image
(define (map4-image rfunc gfunc bfunc afunc pic)
  (check-procedure-arity rfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as first argument")
  (check-procedure-arity gfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as second argument")
  (check-procedure-arity bfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as third argument")
  (check-procedure-arity afunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as fourth argument")
  (unless (image? pic)
    (error 'map4-image
           "Expected an image as fifth argument, but received ~v" pic))
  (map-image-internal
   (lambda (x y c)
     (define r (color-red c))
     (define g (color-green c))
     (define b (color-blue c))
     (define a (color-alpha c))
     (make-color (check-component (rfunc x y r g b a) 'map4-image "Expected first argument to return integer in range 0-255")
                 (check-component (gfunc x y r g b a) 'map4-image "Expected second argument to return integer in range 0-255")
                 (check-component (bfunc x y r g b a) 'map4-image "Expected third argument to return integer in range 0-255")
                 (check-component (afunc x y r g b a) 'map4-image "Expected fourth argument to return integer in range 0-255")
                 ))
   pic))

(module+ test
         (check-error (map4-image add1 + + + pic:bloch)
                      "map4-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as first argument")
         (check-error (map4-image + add1 + + pic:bloch)
                      "map4-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as second argument")
         (check-error (map4-image + + add1 + pic:bloch)
                      "map4-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as third argument")
         (check-error (map4-image + + + add1 pic:bloch)
                      "map4-image: Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(a) -> [0-255] as fourth argument")
         (check-error (map4-image + + + + 17)
                      "map4-image: Expected an image as fifth argument, but received 17")
         )

; fold-image : ([x y] c X -> X) X image -> X
; fold-image-internal : ([nat nat] color X -> X) X image -> image
(define (fold-image-internal f init img)
  (define w (image-width img))
  (define h (image-height img))
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (render-image img bdc 0 0)
  (send bdc set-bitmap #f)
  (define bytes (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h bytes)
  (define answer init)
  (for* ((y (in-range 0 h))
         (x (in-range 0 w)))
        (set! answer  (f x y (get-px x y w h bytes) answer)))
  answer)

(define (fold-image f init img)
  (unless (image? img)
    (error 'fold-image
           (format "Expected an image as third argument, but received ~v" img)))
  (cond [(procedure-arity-includes? f 4)
         (fold-image-internal f init img)]
        [(procedure-arity-includes? f 2)            ; allow f : color X->X as a simple case
         (fold-image-internal (lambda (x y c old-value) (f c old-value)) init img)]
        [else (error 'fold-image "Expected a function of two or four parameters as first argument")]))

; fold-image/extra : ([x y] c X Y -> X) X image Y -> X
(define (fold-image/extra f init img extra)
  (unless (image? img)
    (error 'fold-image/extra
           (format "Expected an image as third argument, but received ~v" img)))
  (cond [(procedure-arity-includes? f 5)
         (fold-image-internal (lambda (x y c old-value) (f x y c old-value extra)) init img)]
        [(procedure-arity-includes? f 3)
         (fold-image-internal (lambda (x y c old-value) (f c old-value extra)) init img)]
        [else (error 'fold-image/extra "Expected a function taking three or five parameters as first argument")]
        ))

(module+ test
         ; more checks
         ;(check-error (map-image (lambda (c) c) pic:bloch)
         ;             "No, this should NOT produce an error.")
         (test)
         ) ; end of test module
