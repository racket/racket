#lang scheme/base
(require "../../mrlib/image-core.ss"
         "../../2htdp/private/image-more.ss"
         scheme/math
         scheme/class
         scheme/gui/base
         schemeunit)

(define-syntax-rule (test a => b) (check-equal? a b))

;; test case: (beside (text "a"...) (text "b" ...)) vs (text "ab")

;(show-image (frame (rotate 30 (ellipse 200 400 'solid 'purple))))

(define-simple-check (check-close a b)
  (and (number? a)
       (number? b)
       (< (abs (- a b)) 0.001)))

#;
(show-image
 (overlay/xy (rectangle 100 10 'solid 'red)
             0
             10
             (rectangle 100 10 'solid 'red)))


#;
(show-image
 (let loop ([image (rectangle 400 8 'solid 'red)]
            [n 2])
   (cond
     [(= n 7) image]
     [else
      (loop (overlay/places 'center 'center
                            image
                            (rotate (* 180 (/ 1 n)) image))
            (+ n 1))])))

(define-syntax-rule 
  (round-numbers e)
  (call-with-values (λ () e) round-numbers/values))

(define (round-numbers/values . args) (apply values (round-numbers/proc args)))

(define (round-numbers/proc x)
  (let loop ([x x])
    (cond
      [(number? x) (let ([n (exact->inexact (/ (round (* 100. x)) 100))])
                     (if (equal? n -0.0)
                         0.0
                         n))]
      [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
      [(vector? x) (apply vector (map loop (vector->list x)))]
      [(is-a? x image%)
       (make-image 
        (loop (image-shape x))
        (loop (image-bb x))
        (loop (image-normalized? x)))]
      [(object? x)
       ;; add a random number here to hack around the way Eli's tester treats two errors as a passing test
       (error 'round-numbers/proc "cannot handle objects ~a" (random))]
      [(let-values ([(a b) (struct-info x)]) a)
       =>
       (λ (struct-type)
         (apply
          (struct-type-make-constructor struct-type)
          (map loop (cdr (vector->list (struct->vector x))))))]
      [else x])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  compare-all-rotations
;;

(check-equal? (compare-all-rotations '() '() equal?)
              #t)
(check-equal? (compare-all-rotations '(1) '(1) equal?)
              #t)
(check-equal? (compare-all-rotations '(1) '(2) equal?)
              #f)
(check-equal? (compare-all-rotations '(1 2 3) '(1 2 3) equal?)
              #t)
(check-equal? (compare-all-rotations '(1 2 3) '(2 3 1) equal?)
              #t)
(check-equal? (compare-all-rotations '(1 2 3) '(3 1 2) equal?)
              #t)
(check-equal? (compare-all-rotations '(1 2 3 4) '(4 1 2 3) equal?)
              #t)
(check-equal? (compare-all-rotations '(1 2 3 5) '(4 1 2 3) equal?)
              #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; circle vs ellipse
;;

(check-equal? (ellipse 40 40 'outline 'black)
              (circle 20 'outline 'black))
(check-equal? (ellipse 60 60 'solid 'red)
              (circle 30 'solid 'red))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; width and height
;;

(test (image-width (rectangle 10 20 'solid 'blue))
      =>
      10)
(test (image-height (rectangle 10 20 'solid 'blue))
      =>
      20)
(test (image-width (rectangle 0 100 'solid 'blue))
      =>
      0)
(test (image-height (rectangle 0 100 'solid 'blue))
      =>
      100)
(test (image-width (rectangle 100 0 'solid 'blue))
      =>
      100)
(test (image-height (rectangle 100 0 'solid 'blue))
      =>
      0)

(check-close (image-width (rotate 45 (rectangle 100 0 'solid 'blue)))
             (* (sin (* pi 1/4)) 100))
(check-close (image-height (rotate 45 (rectangle 100 0 'solid 'blue)))
             (* (sin (* pi 1/4)) 100))
(check-close (image-width (rotate 45 (rectangle 0 100 'solid 'blue)))
             (* (sin (* pi 1/4)) 100))
(check-close (image-height (rotate 45 (rectangle 0 100 'solid 'blue)))
             (* (sin (* pi 1/4)) 100))

(test (image-width (scale 4 (rectangle 10 10 'outline 'black)))
      =>
      40)
(test (image-width (rotate 90 (scale 4 (rectangle 10 10 'outline 'black))))
      =>
      40.0)

(test (image-width (scale 4 (rectangle 10 10 'solid 'black)))
      =>
      40)
(test (image-width (rotate 90 (scale 4 (rectangle 10 10 'solid 'black))))
      =>
      40.0)


(test (image-width (ellipse 10 20 'solid 'blue))
      =>
      10)
(test (image-height (ellipse 10 20 'solid 'blue))
      =>
      20)
(test (image-width (ellipse 0 100 'solid 'blue))
      =>
      0)
(test (image-height (ellipse 0 100 'solid 'blue))
      =>
      100)
(test (image-width (ellipse 100 0 'solid 'blue))
      =>
      100)
(test (image-height (ellipse 100 0 'solid 'blue))
      =>
      0)

(test (image-width (rotate 30 (ellipse 100 0 'solid 'blue)))
      =>
      (* (cos (* pi 1/6)) 100))
(test (image-height (rotate 30 (ellipse 100 0 'solid 'blue)))
      =>
      (* (sin (* pi 1/6)) 100))
(check-close (image-width (rotate 30 (ellipse 0 100 'solid 'blue)))
             (* (sin (* pi 1/6)) 100))
(check-close (image-height (rotate 30 (ellipse 0 100 'solid 'blue)))
             (* (cos (* pi 1/6)) 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing overlays
;;

(test (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 100 100 'solid 'blue)))
        (make-translate 0 0 (image-shape (ellipse 120 120 'solid 'red))))
       (make-bb 120
                120
                120)
       #f))

(test (overlay/xy (ellipse 100 100 'solid 'blue)
                  0 0
                  (ellipse 120 120 'solid 'red))
      =>
      (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red)))


(test (overlay/xy (ellipse 50 100 'solid 'red)
                  -25 25
                  (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate
         25 0
         (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate
         0 25
         (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100
                100
                100)
       #f))

(test (overlay/xy (ellipse 100 50 'solid 'green)
                  10 10
                  (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 10 10 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100
                110
                110)
       #f))

(test (overlay (ellipse 100 50 'solid 'green)
               (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100
                100
                100)
       #f))

(test (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red)
               (ellipse 140 140 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 
         0 0
         (make-overlay
          (make-translate 0 0 (image-shape (ellipse 100 100 'solid 'blue)))
          (make-translate 0 0 (image-shape (ellipse 120 120 'solid 'red)))))
        (make-translate 0 0 (image-shape (ellipse 140 140 'solid 'green))))
       (make-bb 140 140 140)
       #f))

(test (overlay/places 'middle
                      'middle
                      (ellipse 100 50 'solid 'green)
                      (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100 100 100)
       #f))

(test (overlay/places 'middle
                      'middle
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))


(test (overlay/places 'right
                      'bottom
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 50 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 50 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))

(test (overlay/places 'right
                      'baseline
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 50 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 50 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))

(test (beside/places 'top
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 0 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/places 'center
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 25 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/places 'baseline
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 50 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside (ellipse 50 100 'solid 'red)
              (ellipse 100 50 'solid 'blue))
      =>
      (beside/places 'top
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing normalization
;;

(test (normalize-shape (image-shape (ellipse 50 100 'solid 'red))
                       values)
      =>
      (make-translate 25 50 (make-ellipse 50 100 0 'solid "red")))

(test (normalize-shape (make-overlay (image-shape (ellipse 50 100 'solid 'red))
                                     (image-shape (ellipse 50 100 'solid 'blue)))
                       values)
      =>
      (make-overlay (image-shape (ellipse 50 100 'solid 'red))
                    (image-shape (ellipse 50 100 'solid 'blue))))

(test (normalize-shape (make-overlay
                        (make-overlay (image-shape (ellipse 50 100 'solid 'red))
                                      (image-shape (ellipse 50 100 'solid 'blue)))
                        (image-shape (ellipse 50 100 'solid 'green)))
                       values)
      =>
      (make-overlay 
       (make-overlay (make-translate 25 50 (make-ellipse 50 100 0 'solid "red"))
                     (make-translate 25 50 (make-ellipse 50 100 0 'solid "blue")))
       (make-translate 25 50 (make-ellipse 50 100 0 'solid "green"))))

(test (normalize-shape (make-overlay
                        (image-shape (ellipse 50 100 'solid 'green))
                        (make-overlay (image-shape (ellipse 50 100 'solid 'red))
                                      (image-shape (ellipse 50 100 'solid 'blue))))
                       values)
      =>
      (make-overlay 
       (make-overlay (make-translate 25 50 (make-ellipse 50 100 0 'solid "green"))
                     (make-translate 25 50 (make-ellipse 50 100 0 'solid "red")))
       (make-translate 25 50 (make-ellipse 50 100 0 'solid "blue"))))

(test (normalize-shape (make-translate 100 100 (image-shape (ellipse 50 100 'solid 'blue)))
                       values)
      =>
      (make-translate 125 150 (make-ellipse 50 100 0 'solid "blue")))

(test (normalize-shape (make-translate 10 20 (make-translate 100 100 (image-shape (ellipse 50 100 'solid 'blue))))
                       values)
      =>
      (make-translate 135 170 (make-ellipse 50 100 0 'solid "blue")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing rotating
;;

(test (bring-between 123 360) => 123)
(test (bring-between 365 360) => 5)
(test (bring-between -5 360) => 355)
(test (bring-between 720 360) => 0)
(test (bring-between 720.5 360) => .5)

(test (round-numbers (rotate 90 (rectangle 100 100 'solid 'blue)))
      =>
      (round-numbers (rectangle 100 100 'solid 'blue)))

(test (round-numbers
       (normalize-shape (image-shape (rotate 90 (rotate 90 (rectangle 50 100 'solid 'purple))))
                        values))
      =>
      (round-numbers
       (normalize-shape (image-shape (rotate 180 (rectangle 50 100 'solid 'purple)))
                        values)))

(test (round-numbers (normalize-shape (image-shape (rotate 90 (ellipse 10 10 'solid 'red)))))
      =>
      (round-numbers (normalize-shape (image-shape (ellipse 10 10 'solid 'red)))))

(test (round-numbers (normalize-shape (image-shape (rotate 90 (ellipse 10 12 'solid 'red)))))
      =>
      (round-numbers (normalize-shape (image-shape (ellipse 12 10 'solid 'red)))))

(test (round-numbers (normalize-shape (image-shape (rotate 135 (ellipse 10 12 'solid 'red)))))
      =>
      (round-numbers (normalize-shape (image-shape (rotate 45 (ellipse 12 10 'solid 'red))))))

(test (round-numbers (rotate -90 (ellipse 200 400 'solid 'purple)))
      =>
      (round-numbers (rotate 90 (ellipse 200 400 'solid 'purple))))

(require (only-in lang/htdp-advanced equal~?))

(test (equal~? (rectangle 100 10 'solid 'red)
               (rotate 90 (rectangle 10 100 'solid 'red))
               0.1)
      =>
      #t)

(test (equal~? (rectangle 100 10 'solid 'red)
               (rotate 90 (rectangle 10.001 100.0001 'solid 'red))
               0.1)
      =>
      #t)

(test (round-numbers
       (normalize-shape
        (image-shape
         (rotate 
          90
          (overlay/xy (rectangle 20 100 'solid 'purple)
                      20 0
                      (ellipse 40 40 'solid 'orange))))))
      =>
      (round-numbers
       (normalize-shape
        (image-shape
         (overlay/xy (rectangle 100 20 'solid 'purple)
                     0 -40
                     (ellipse 40 40 'solid 'orange))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scaling tests
;;

(test (scale 2 (rectangle 100 10 'solid 'blue))
      =>
      (rectangle 200 20 'solid 'blue))

(test (scale 3
             (overlay/xy (rectangle 100 10 'solid 'blue)
                         0
                         20
                         (rectangle 100 10 'solid 'red)))
      =>
      (overlay/xy (rectangle 300 30 'solid 'blue)
                  0
                  60
                  (rectangle 300 30 'solid 'red)))

(test (scale 3
             (overlay/xy (rectangle 100 10 'solid 'blue)
                         0
                         20
                         (overlay/xy (rectangle 100 10 'solid 'blue)
                                     0
                                     20
                                     (rectangle 100 10 'solid 'purple))))
      =>
      (overlay/xy (rectangle 300 30 'solid 'blue)
                  0
                  60
                  (overlay/xy (rectangle 300 30 'solid 'blue)
                              0
                              60
                              (rectangle 300 30 'solid 'purple))))

(test (scale/xy 3 4 (ellipse 30 60 'outline 'purple))
      =>
      (ellipse (* 30 3) (* 60 4) 'outline 'purple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; misc tests
;;

(test (rectangle 100 10 'solid 'blue)
      =>
      (rectangle 100 10 "solid" "blue"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  bitmap tests
;;

(define (fill-bitmap b color)
  (let ([bdc (make-object bitmap-dc% b)])
    (send bdc set-brush color 'solid)
    (send bdc set-pen color 1 'solid)
    (send bdc draw-rectangle 0 0 (send b get-width) (send b get-height))
    (send bdc set-bitmap #f)))

(define blue-10x20-bitmap (make-object bitmap% 10 20))
(fill-bitmap blue-10x20-bitmap "blue")
(define blue-20x10-bitmap (make-object bitmap% 20 10))
(fill-bitmap blue-20x10-bitmap "blue")
(define blue-20x40-bitmap (make-object bitmap% 20 40))
(fill-bitmap blue-20x40-bitmap "blue")

(test (image-right (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      10)
(test (image-bottom (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      20)
(test (image-baseline (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      20)
#|
(test (scale 2 (make-object image-snip% blue-10x20-bitmap))
      =>
      (image-snip->image (make-object image-snip% blue-20x40-bitmap)))
(test (rotate 90 (make-object image-snip% blue-10x20-bitmap))
      =>
      (image-snip->image (make-object image-snip% blue-20x10-bitmap)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  regular polygon
;;

;; note: the regular-polygon and the rectangle generate the points in reverse directions.
(check-equal? (round-numbers (regular-polygon 100 4 'outline 'green))
              (round-numbers (rectangle 100 100 'outline 'green)))

(check-equal? (swizzle (list 0 1 2 3 4) 2)
              (list 0 2 4 1 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  text
;;

(check-equal? (beside/places "baseline"
                             (text "a" 18 "black")
                             (text "b" 18 "black"))
              (text "ab" 18 "black"))

(check-equal? (round-numbers
               (image-width (rotate 45 (text "One" 18 'black))))
              (round-numbers
               (let ([t (text "One" 18 'black)])
                 (image-width (rotate 45 (rectangle (image-width t) 
                                                    (image-height t)
                                                    'solid 'black))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; triangle
;;

(check-equal? (round-numbers (rotate 180 (isosceles-triangle 60 330 "solid" "lightseagreen")))
              (round-numbers (isosceles-triangle 60 30 "solid" "lightseagreen")))

(check-equal? (triangle 40 'outline 'black)
              (regular-polygon 40 3 'outline 'black))

(check-equal? (equal~? (rotate (+ 180 45) (right-triangle 50 50 'solid 'black))
                       (isosceles-triangle 50 90 'solid 'black)
                       0.001)
              #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; square
;;

(check-equal? (square 10 'solid 'black)
              (rectangle 10 10 'solid 'black))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rhombus
;;

(check-equal? (equal~? (rhombus 10 90 'solid 'black)
                       (square 10 'solid 'black)
                       0.01)
              #t)

(check-equal? (equal~? (rhombus 50 150 'solid 'black)
                       (rotate 90 (rhombus 50 30 'solid 'black))
                       0.01)
              #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lines
;;

(check-equal? (image-width (line 10 20 'black))
              11)
(check-equal? (image-height (line 10 20 'black))
              21)

(check-equal? (round-numbers (rotate 90 (line 10 20 'black)))
              (round-numbers (line 20 -10 'black)))

(check-equal? (round-numbers (line 20 30 "red"))
              (round-numbers (rotate 180 (line 20 30 "red"))))

(check-equal? (round-numbers (line -30 20 "red"))
              (round-numbers (line 30 -20 "red")))

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     10 10 90 190 "red"))
              100)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                     10 10 90 190 "red"))
              200)
(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                      10 10 200 200 "red"))
              200)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                      10 10 200 200 "red"))
              200)

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     10 10 300 300 "red"))
              300)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                      10 10 300 300 "red"))
              300)

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     -10 10 100 200 "red"))
              110)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                     -10 10 100 200 "red"))
              200)

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     10 -10 100 200 "red"))
              100)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                      10 -10 100 200 "red"))
              210)

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     100 200 10 -10 "red"))
              100)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                      100 200 10 -10 "red"))
              210)

(check-equal? (image-width (add-line (rectangle 100 200 'solid 'black)
                                     100 200 -10 10 "red"))
              110)
(check-equal? (image-height (add-line (rectangle 100 200 'solid 'black)
                                      100 200 -10 10 "red"))
              200)

(let* ([txt (text "H" 24 'black)]
       [bl (image-baseline txt)])
  (check-equal? (image-baseline (add-line txt 0 0 100 100 'red))
                bl))

(let* ([txt (text "H" 24 'black)]
       [bl (image-baseline txt)])
  (check-equal? (image-baseline (add-line txt 0 -10 100 100 'red))
                (+ bl 10)))