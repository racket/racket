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

(require racket/draw
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
; This will remember the last image on which it was called.
; Really terrible performance if you call it in alternation
; on two different images, but should be OK if you call it
; lots of times on the same image.
; Returns transparent if you ask about a position outside the picture.
(define get-pixel-color
  (let [[last-image #f]
        [last-bytes #f]]
    (lambda (x y pic)
      (define w (image-width pic))
      (define h (image-height pic))
      (unless (eqv? pic last-image)
        ; assuming nobody mutates an image between one get-pixel-color and the next
        (set! last-image pic)
        (define bm (make-bitmap w h))
        (define bmdc (make-object bitmap-dc% bm))
        (set! last-bytes (make-bytes (* 4 w h)))
        (render-image pic bmdc 0 0)
        (send bmdc set-bitmap #f)
        (send bm get-argb-pixels 0 0 w h last-bytes))
      (if (and (<= 0 x (sub1 w))
               (<= 0 y (sub1 h)))
          (get-px x y w h last-bytes)
          transparent))))

;; pixel-visible? : nat(x) nat(y) image -> boolean
;; similar
;(define pixel-visible?
;  (let [[last-image #f]
;        [last-bm #f]
;        [last-bmdc #f]]
;    (lambda (x y pic)
;      (unless (eqv? pic last-image)
;        (set! last-image pic)
;        (set! last-bm (get-mask pic))
;        (set! last-bmdc (make-object bitmap-dc% last-bm)))
;      (let [[mask-pix (get-px x y last-bmdc)]] ; assumes this doesn't crash if out of bounds
;        (and (equal? mask-pix (make-color 0 0 0)) ; treat anything else as transparent
;             (>= x 0)
;             (>= y 0)
;             (< x (image-width pic))
;             (< y (image-height pic))
;            )))))
;

; build-image-internal : natural(width) natural(height) (nat nat -> color) -> image
(define (build-image-internal w h f)
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (define bytes (make-bytes (* w h 4)))
  (for* ((y (in-range 0 h))
         (x (in-range 0 w)))
        (set-px! bytes x y w h (f x y)))
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

; build3-image : nat(width) nat(height) rfunc gfunc bfunc -> image
; where each of rfunc, gfunc, bfunc is (nat(x) nat(y) -> nat)
(define (build3-image w h rfunc gfunc bfunc)
  (unless (natural? w)
    (error 'build3-image
	(format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build3-image
	(format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity rfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> color as third argument")
  (check-procedure-arity gfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> color as fourth argument")
  (check-procedure-arity bfunc 2 'build3-image "Expected a function with contract num(x) num(y) -> color as fifth argument")
  (build-image-internal w h
                        (lambda (x y)
                          (make-color (rfunc x y) (gfunc x y) (bfunc x y)))))

; build4-image : nat(width) nat(height) rfunc gfunc bfunc afunc -> image
; where each of rfunc, gfunc, bfunc, afunc is (nat(x) nat(y) -> nat)
(define (build4-image w h rfunc gfunc bfunc afunc)
  (unless (natural? w)
    (error 'build4-image
	(format "Expected a natural number as first argument, but received ~v" w)))
  (unless (natural? h)
    (error 'build4-image
	(format "Expected a natural number as second argument, but received ~v" h)))
  (check-procedure-arity rfunc 2 'build-image "Expected a function with contract num(x) num(y) -> color as third argument")
  (check-procedure-arity gfunc 2 'build-image "Expected a function with contract num(x) num(y) -> color as fourth argument")
  (check-procedure-arity bfunc 2 'build-image "Expected a function with contract num(x) num(y) -> color as fifth argument")
  (check-procedure-arity afunc 2 'build-image "Expected a function with contract num(x) num(y) -> color as sixth argument")
  (build-image-internal w h
                        (lambda (x y)
                          (make-color (rfunc x y) (gfunc x y) (bfunc x y) (afunc x y)))))



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
        (set-px! bytes x y w h (f x y (get-px x y w h bytes))))
  (send bm set-argb-pixels 0 0 w h bytes)
  (make-object image-snip% bm))

; map-image : (int int color -> broad-color) image -> image
(define (map-image f img)
  (unless (image? img)
    (error 'map-image
	(format "Expected an image as second argument, but received ~v" img)))
  (cond [(procedure-arity-includes? f 3)
         (map-image-internal (colorize-func f) img)]
        [(procedure-arity-includes? f 1)            ; allow f : color->color as a simple case
         (map-image-internal (colorize-func (lambda (x y c) (f c))) img)]
        [else (error 'map-image "Expected a function of one or three parameters as first argument")]))

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
        [else (error 'map-image/extra "Expected a function taking two or four parameters as first argument")]))




; The version for use before students have seen structs:
; map3-image :
; (int(x) int(y) int(r) int(g) int(b) -> int(r))
; (int(x) int(y) int(r) int(g) int(b) -> int(g))
; (int(x) int(y) int(r) int(g) int(b) -> int(b))
; image -> image
; Note: by default, preserves alpha values from old image.
(define (map3-image rfunc gfunc bfunc pic)
  (check-procedure-arity rfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> num(r) as first argument")
  (check-procedure-arity gfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> num(g) as second argument")
  (check-procedure-arity bfunc 5 'map3-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) -> num(b) as third argument")
  (unless (image? pic)
    (error 'map3-image
	(format "Expected an image as fourth argument, but received ~v" pic)))
   (map-image-internal
      (lambda (x y c)
          (make-color (rfunc x y (color-red c) (color-green c) (color-blue c))
                      (gfunc x y (color-red c) (color-green c) (color-blue c))
                      (bfunc x y (color-red c) (color-green c) (color-blue c))
                      (color-alpha c)))
      pic))

; map4-image :
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(r))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(g))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(b))
;  (int(x) int(y) int(r) int(g) int(b) int(a) -> int(a))
;  image -> image
(define (map4-image rfunc gfunc bfunc afunc pic)
  (check-procedure-arity rfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(alpha) -> num(r) as first argument")
  (check-procedure-arity gfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(alpha) -> num(g) as second argument")
  (check-procedure-arity bfunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(alpha) -> num(b) as third argument")
  (check-procedure-arity afunc 6 'map4-image "Expected a function with contract num(x) num(y) num(r) num(g) num(b) num(alpha) -> num(alpha) as fourth argument")
  (unless (image? pic)
    (error 'map4-image
	"Expected an image as fifth argument, but received ~v" pic))
   (map-image-internal
      (lambda (x y c)
          (make-color (rfunc x y (color-red c) (color-green c) (color-blue c) (color-alpha c))
                      (gfunc x y (color-red c) (color-green c) (color-blue c) (color-alpha c))
                      (bfunc x y (color-red c) (color-green c) (color-blue c) (color-alpha c))
                      (afunc x y (color-red c) (color-green c) (color-blue c) (color-alpha c))))
      pic))
