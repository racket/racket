;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname map-image-isl-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)

(define tri (triangle 60 "solid" "orange"))
(define hieroglyphics pic:hieroglyphics)
(define scheme-logo pic:scheme-logo)
(define bloch pic:bloch)

"(build-image 50 35 (lambda (x y) red)):"
(build-image 50 35 (lambda (x y) "red"))
"should be a 50x35 red rectangle"

; myflip : image -> image
; vertical reflection defined by bitmap operations
(define (myflip pic)
  (local [(define (other-pixel x y) (get-pixel-color x (- (image-height pic) y 1) pic))]
    (build-image (image-width pic) (image-height pic)
                 other-pixel)))


(check-expect (myflip pic:bloch) (flip-vertical pic:bloch))

(define RADIUS 1)

(define (clip-to n low high)
  (min (max n low) high))
(check-expect (clip-to 10 5 15) 10)
(check-expect (clip-to 10 15 20) 15)
(check-expect (clip-to 10 -20 7) 7)

; replace-alpha : color number -> color
(define (replace-alpha old-color alpha)
  (make-color (color-red old-color)
              (color-green old-color)
              (color-blue old-color)
              alpha))

(define (myfuzz pic)
  (local [(define (near-pixel x y)
            (get-pixel-color
             (clip-to (+ x (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (sub1 (image-width pic)))
             (clip-to (+ y (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (sub1 (image-height pic)))
             pic)
            )]
    (build-image (image-width pic) (image-height pic)
                 near-pixel)))

(myfuzz bloch)
(myfuzz tri)

(define (masked-fuzz pic)
  ; Like myfuzz, but preserves the old mask
  (local [(define (near-pixel x y)
            (replace-alpha
                (get-pixel-color
                 (clip-to (+ x (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (image-width pic))
                 (clip-to (+ y (- RADIUS) (random (+ 1 RADIUS RADIUS))) 0 (image-height pic))
                 pic)
                (color-alpha (get-pixel-color x y pic))
                ))]
    (build-image (image-width pic) (image-height pic)
                 near-pixel)))
(masked-fuzz bloch)
(masked-fuzz tri)

; Convert all white pixels to transparent
(define (white->trans pic)
  (local [(define white (name->color "white"))
          (define (new-color #; x #; y old-color) ; leave out x & y (dec2011)
            (if (equal? old-color white)
                false
                old-color))]
    (map-image new-color pic)))

(define hier (white->trans hieroglyphics))
(overlay hier (rectangle 100 100 "solid" "blue"))

(define (diamond-color x y)
  (make-color (* 5 (max (abs (- x 50)) (abs (- y 50))))
              0
              (* 2 y)))

(build-image 100 100 diamond-color)

(define (animation-test dummy)
  (big-bang bloch (on-draw show-it) (on-tick myfuzz 1)))
"Run (animation-test 'blah) to test myfuzz as tick handler."
