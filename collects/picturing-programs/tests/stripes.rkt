;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stripes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require "../package/picturing-programs.rkt")

; choose-color : num(x) num(y) -> color
(check-expect (choose-color 57 0) (name->color "red"))
(check-expect (choose-color 57 1) (name->color "blue"))
(check-expect (choose-color 72 2) (name->color "red"))
(check-expect (choose-color 14 9) (name->color "blue"))
(define (choose-color x y)
  ; x number
  ; y number
  (cond [(even? y) (name->color "red")]
        [(odd? y) (name->color "blue")]))

; red-blue-stripes : num(width) num(height) -> image
(check-expect (red-blue-stripes 10 0)
              (rectangle 10 0 "solid" "purple"))
(check-expect (red-blue-stripes 10 1)
              (rectangle 10 1 "solid" "red")) ; fails
(check-expect (red-blue-stripes 10 2)
              (above (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")))
(check-expect (red-blue-stripes 10 3)
              (above (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")
                     (rectangle 10 1 "solid" "red"))) ; fails
(check-expect (red-blue-stripes 10 4)
              (above (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")
                     (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")))
(check-expect (red-blue-stripes 10 5)
              (above (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")
                     (rectangle 10 1 "solid" "red")
                     (rectangle 10 1 "solid" "blue")
                     (rectangle 10 1 "solid" "red"))) ; fails
(define (red-blue-stripes width height)
  ; width number
  ; height number
  (build-image width height choose-color)
  )

(red-blue-stripes 10 3)
"should be"
(above (rectangle 10 1 "solid" "red")
       (rectangle 10 1 "solid" "blue")
       (rectangle 10 1 "solid" "red"))

(red-blue-stripes 10 5)
"should be"
(above (rectangle 10 1 "solid" "red")
       (rectangle 10 1 "solid" "blue")
       (rectangle 10 1 "solid" "red")
       (rectangle 10 1 "solid" "blue")
       (rectangle 10 1 "solid" "red"))

(define s0 (red-blue-stripes 10 0))
(define s1 (red-blue-stripes 10 1))
(define s2 (red-blue-stripes 10 2))
(define s3 (red-blue-stripes 10 3))
(define s4 (red-blue-stripes 10 4))
(define s5 (red-blue-stripes 10 5))

(define grad (build-image 10 10
                          (lambda (x y) (make-color (* 25 x) (* 25 y) 0))))

(define (dump img)
  (map (lambda (y)
         (map (lambda (x)
                (get-pixel-color x y img))
              (list 0 1 2 (- (image-width img) 2) (- (image-width img) 1)))
         )
       (list 0 1 2 (- (image-height img) 2) (- (image-height img) 1))))


(define (red-purple-helper x y c)
  (cond [(color=? c (name->color "red"))
         (name->color "purple")]
        [else c]))

(define (red->purple pic)
  (map red-purple-helper pic))

(check-expect (red->purple (rectangle 50 30 "solid" "blue"))
              (rectangle 50 30 "solid" "blue")) ; does nothing
(check-expect (red->purple (rectangle 50 30 "solid" "red"))
              (rectangle 50 30 "solid" "purple")) ; replaces everything
(check-expect (red->purple (overlay (triangle 30 "solid" "red")
                                    (rectangle 60 60 "solid" "green")))
              (overlay (triangle 30 "solid" "purple")
                       (rectangle 60 60 "solid" "green")))
(check-expect (red->purple (overlay (text "hello" 18 "red")
                                    (ellipse 100 50 "solid" "yellow")))
              (overlay (text "hello" 18 "purple")
                       (ellipse 100 50 "solid" "yellow")))
