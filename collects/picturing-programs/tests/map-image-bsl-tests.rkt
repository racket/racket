;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname map-image-bsl-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs/book-pictures)
(require picturing-programs/map-image)
(require 2htdp/image)

; Test cases for primitives:
(check-expect (real->int 3.2) 3)
(check-expect (real->int 3.7) 4)
(check-expect (real->int 3.5) 4)
(check-expect (real->int 2.5) 2)
(check-expect (real->int #i3.2) 3)
(check-expect (real->int #i3.7) 4)
(check-expect (real->int #i3.5) 4)
(check-expect (real->int #i2.5) 2)

;(check-expect (maybe-color? (make-color 3 4 5)) true)
;(check-expect (maybe-color? (make-color 3 4 5 6)) true)
;(check-expect (maybe-color? false) true)
;(check-expect (maybe-color? true) false)
;(check-expect (maybe-color? (make-posn 3 4)) false)
;(check-expect (maybe-color? "red") false)

(check-expect (name->color "white") (make-color 255 255 255))
(check-expect (name->color "black") (make-color 0 0 0))
(check-expect (name->color "blue") (make-color 0 0 255))
(check-expect (name->color "plaid") false)
(check-error (name->color 7) "name->color: argument must be a string or symbol")

(check-expect (color=? (make-color 5 10 15) (make-color 5 10 15)) true)
(check-expect (color=? (make-color 5 10 15) (make-color 5 15 10)) false)
(check-expect (color=? (make-color 255 255 255) "white") true)
(check-expect (color=? (make-color 255 255 255) "blue") false)
(check-expect (color=? "forest green" 'forestgreen) true)
(check-expect (color=? "forest green" 'lightblue) false)
(check-expect (color=? (make-color 5 10 15 20) (make-color 5 10 15)) false)
(check-expect (color=? (make-color 5 10 15 255) (make-color 5 10 15)) true)
(check-expect (color=? (make-color 5 10 15 0) false) true)
(check-expect (color=? (make-color 5 10 15 20) false) false)
(check-error (color=? "white" 3) "colorize: Unrecognized type")
(check-error (color=? "white" "plaid") "color=?: Expected two colors or color names as arguments")

; Test cases for map3-image:

; red-id : x y r g b -> num
(define (red-id x y r g b) r)
; green-id : x y r g b -> num
(define (green-id x y r g b) g)
; blue-id : x y r g b -> num
(define (blue-id x y r g b) b)
; zero-5-args : x y r g b -> num
(define (zero-5-args x y r g b) 0)

(define tri (triangle 60 "solid" "orange"))
; (define hieroglyphics pic:hieroglyphics)
; (define scheme-logo pic:scheme-logo)
; (define bloch pic:bloch)

"tri:" tri
"(map3-image red-id green-id blue-id tri) should be tri:"
(map3-image red-id green-id blue-id tri)
"(map3-image zero-5-args green-id blue-id tri) should be a green triangle:"
(map3-image zero-5-args green-id blue-id tri)

"(map3-image zero-5-args green-id blue-id bloch) should be a de-redded Steve Bloch:"
(map3-image zero-5-args green-id blue-id bloch)

; x-gradient-5 : x y r g b -> num
(define (x-gradient-5 x y r g b) (min 255 (* 4 x)))
; y-gradient-5 : x y r g b -> num
(define (y-gradient-5 x y r g b) (min 255 (* 4 y)))
"(map3-image zero-5-args x-gradient-5 y-gradient-5 tri) should be a triangular window on a 2-dimensional color gradient:"
(map3-image zero-5-args x-gradient-5 y-gradient-5 tri)
"The same thing with some red:"
(map3-image red-id x-gradient-5 y-gradient-5 tri)
"And now let's try it on bloch.  Should get a rectangular 2-dimensional color gradient:"
(map3-image zero-5-args x-gradient-5 y-gradient-5 bloch)
"The same thing preserving the red:"
(map3-image red-id x-gradient-5 y-gradient-5 bloch)
"Rotating colors r->g->b->r:"
(map3-image blue-id red-id green-id bloch)

; Test cases for map4-image:
; red-id6 : x y r g b a -> num
(define (red-id6 x y r g b a) r)
; green-id6 : x y r g b a -> num
(define (green-id6 x y r g b a) g)
; blue-id6 : x y r g b a -> num
(define (blue-id6 x y r g b a) b)
; alpha-id6 : x y r g b a -> num
(define (alpha-id6 x y r g b a) a)
; zero-6-args : x y r g b a -> num
(define (zero-6-args x y r g b a) 0)
; 

"tri:" tri
"(map4-image red-id6 green-id6 blue-id6 alpha-id6 tri) should be tri:"
(map4-image red-id6 green-id6 blue-id6 alpha-id6 tri)
"(map4-image zero-6-args green-id6 blue-id6 alpha-id6 tri) should be a green triangle:"
(map4-image zero-6-args green-id6 blue-id6 alpha-id6 tri)

"(map4-image zero-6-args green-id6 blue-id6 alpha-id6 bloch) should be a de-redded Steve Bloch:"
(map4-image zero-6-args green-id6 blue-id6 alpha-id6 bloch)

(define bluebox (rectangle 100 100 "solid" "light blue"))
; x-gradient-6 : x y r g b a -> num
(define (x-gradient-6 x y r g b a) (min 255 (* 4 x)))
; y-gradient-6 : x y r g b a -> num
(define (y-gradient-6 x y r g b a) (min 255 (* 4 y)))
"(map4-image zero-6-args x-gradient-6 y-gradient-6 alpha-id6 tri) should be a triangular window on a 2-dimensional color gradient, light blue background:"
(overlay (map4-image zero-6-args x-gradient-6 y-gradient-6 alpha-id6 tri) bluebox)
"(map4-image red-id green-id blue-id x-gradient-6 tri) should be a triangle with a 1-dimensional alpha gradient:"
(overlay (map4-image red-id6 green-id6 blue-id6 x-gradient-6 tri) bluebox)

"Same thing on bloch:"
(overlay (map4-image red-id6 green-id6 blue-id6 x-gradient-6 bloch) bluebox)

; Test cases for map-image:
; color-id : x y color -> color
(define (color-id x y c)
  c)

; kill-red : x y color -> color
(define (kill-red x y c)
  (make-color 0 (color-green c) (color-blue c)))
(define (kill-red-preserving-alpha x y c)
  (make-color 0 (color-green c) (color-blue c) (color-alpha c)))

; make-gradient : x y color -> color
(define (make-gradient x y c)
  (make-color 0 (min (* 4 x) 255) (min (* 4 y) 255)))

"tri:" tri
"(map-image color-id tri):" 
(define ex1 (map-image color-id tri)) ex1
"(map-image kill-red tri):" 
(define ex2 (map-image kill-red tri)) ex2
"(map-image kill-red-preserving-alpha tri):"
(define ex2prime (map-image kill-red-preserving-alpha tri)) ex2prime
"(map-image make-gradient tri):"
(define ex3 (map-image make-gradient tri)) ex3
"(map-image kill-red hieroglyphics):"
(define ex4 (map-image kill-red hieroglyphics)) ex4
"(map-image kill-red scheme-logo):"
(define ex5 (map-image kill-red scheme-logo)) ex5
"(map-image kill-red bloch):"
(define ex6 (map-image kill-red bloch)) ex6
(define (return-5 x y c) 5)

(check-error (map-image return-5 bloch) "colorize: Unrecognized type")

; Test cases for build3-image:
(define (x-gradient-2 x y) (min 255 (* 4 x)))
(define (y-gradient-2 x y) (min 255 (* 4 y)))
(define (zero-2-args x y) 0)
"(build3-image 60 40 zero-2-args x-gradient-2 y-gradient-2) should be a 60x40 rectangle with no red, green increasing from left to right, and blue increasing from top to bottom:"
(build3-image 60 40 zero-2-args x-gradient-2 y-gradient-2)
(check-error (build3-image "hello" true sqrt sqrt sqrt)
             "build3-image: Expected natural number as first argument")
(check-error (build3-image 17 true sqrt sqrt sqrt)
             "build3-image: Expected natural number as second argument")
(check-error (build3-image 17 24 sqrt sqrt sqrt)
             "build3-image: Expected function with contract num(x) num(y) -> color as third argument")
(check-error (build3-image 17 24 x-gradient-2 sqrt sqrt)
             "build3-image: Expected function with contract num(x) num(y) -> color as fourth argument")
(check-error (build3-image 17 24 x-gradient-2 y-gradient-2 sqrt)
             "build3-image: Expected function with contract num(x) num(y) -> color as fifth argument")

(define (return-minus-5 x y) -5)
(check-error (build3-image 17 24 x-gradient-2 y-gradient-2 return-minus-5)
             "make-color: expected <integer between 0 and 255> as third argument, given: -5")

; Test cases for build4-image:

; Test cases for build-image:
(define (always-red x y) (name->color "red"))
"(build-image 50 35 (lambda (x y) red)):"
(build-image 50 35 always-red)
"should be a 50x35 red rectangle"
(define (a-gradient x y) (make-color (real->int (* x 2.5))
                                     (real->int (* y 2.5))
                                     0))
"(build-image 100 100 (lambda (x y) (make-color (* x 2.5) (* y 2.5) 0))):"
(build-image 100 100 a-gradient)
"should be a 100x100 square with a color gradient increasing in red from left to right, and in green from top to bottom"
(check-error (build-image 3.2 100 a-gradient) "build-image: Expected natural number as first argument")
(check-error (build-image 100 -2 a-gradient) "build-image: Expected natural number as second argument")
(check-error (build-image 100 100 sqrt) "build-image: Expected function with contract num(x) num(y) -> color as third argument")



(define (other-bloch-pixel x y)
  (get-pixel-color x (- (image-height bloch) y 1) bloch))
"(build-image (image-width bloch) (image-height bloch) other-bloch-pixel): should be flipped vertically"
(build-image (image-width bloch) (image-height bloch) other-bloch-pixel)



(define RADIUS 3)

(define (clip-to n low high)
  (min (max n low) high))
(check-expect (clip-to 10 5 15) 10)
(check-expect (clip-to 10 15 20) 15)
(check-expect (clip-to 10 -20 7) 7)

(define (near-bloch-pixel x y)
  (get-pixel-color
   (clip-to (+ x (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (sub1 (image-width bloch)))
   (clip-to (+ y (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (sub1 (image-height bloch)))
   bloch))

"fuzzy bloch, radius=3, not adjusting size of image:"
(define fuzzy-bloch
  (build-image (image-width bloch) (image-height bloch) near-bloch-pixel))
fuzzy-bloch

(define (near-tri-mpixel x y)
  (get-pixel-color
   (clip-to (+ x (- RADIUS) (random (+ 1 RADIUS RADIUS)))
            0 (+ RADIUS RADIUS -1 (image-width tri)))
   (clip-to (+ y (- RADIUS) (random (+ 1 RADIUS RADIUS)))
            0 (+ RADIUS RADIUS -1 (image-height tri)))
   tri)
  )
(define fuzzy-tri
  (build-image (+ RADIUS RADIUS (image-width tri))
               (+ RADIUS RADIUS (image-height tri))
               near-tri-mpixel))
"fuzzy triangle, radius=3, adjusting size of image to allow fuzz on all sides:"
fuzzy-tri

; Convert all white pixels to transparent
(define (white-pixel->trans x y old-color)
  (if (color=? old-color "white")
      false
      old-color))
(define (white->trans pic)
  (map-image
   white-pixel->trans
   pic))

(define (white-pixel->red x y old-color)
  (if (color=? old-color 'white)
      "red"
      old-color))
(define (white->red pic)
  (map-image white-pixel->red pic))

"(overlay (white->trans hieroglyphics) (rectangle 100 100 'solid 'blue)):"
(define hier (white->trans hieroglyphics))
(overlay hier (rectangle 100 100 "solid" "blue"))

; pixel->gray : x y color -> color
(check-expect (pixel->gray 3 17 (make-color 0 0 0)) (make-color 0 0 0))
(check-expect (pixel->gray 92 4 (make-color 50 100 150)) (make-color 100 100 100))
(define (pixel->gray x y c)
  (make-gray (quotient (+ (color-red c)
                          (color-green c) 
                          (color-blue c))
                       3)))
                                   
; make-gray : natural -> color
(define (make-gray n)
  (make-color n n n))

; color->gray : image -> image
(define (color->gray pic)
  (map-image pixel->gray pic))

(color->gray bloch)
(color->gray hieroglyphics)
