#lang scheme/base
(require "../../2htdp/private/picture.ss" 
         scheme/math
         tests/eli-tester)

(let* ([first (rectangle 100 10 'solid 'red)]
       [second 
        (overlay/places 'center
                        'center
                        first
                        (rotate/places 'center 'center 
                                       (* pi 1/4)
                                       first))]
       [third 
        (overlay/places 'center
                        'center
                        (frame second)
                        (rotate/places 'center 'center 
                                       (* pi 1/8)
                                       (frame second)))])
  (show-picture second
                #;(frame third)))

(define (round-numbers x)
  (let loop ([x x])
    (cond
      [(number? x) (/ (round (* 100. x)) 100)]
      [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
      [(let-values ([(a b) (struct-info x)]) a)
       =>
       (λ (struct-type)
         (apply
          (struct-type-make-constructor
           struct-type)
          (map loop (cdr (vector->list (struct->vector x))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing overlays
;;

(test (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red))
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 100 100 'solid 'blue)))
        (make-translate 0 0 (picture-shape (ellipse 120 120 'solid 'red))))
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
      (make-picture
       (make-overlay
        (make-translate
         25 0
         (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate
         0 25
         (picture-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100
                100
                100)
       #f))

(test (overlay/xy (ellipse 100 50 'solid 'green)
                  10 10
                  (ellipse 50 100 'solid 'red))
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 100 50 'solid 'green)))
        (make-translate 10 10 (picture-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100
                110
                110)
       #f))

(test (overlay (ellipse 100 50 'solid 'green)
               (ellipse 50 100 'solid 'red))
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 100 50 'solid 'green)))
        (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100
                100
                100)
       #f))

(test (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red)
               (ellipse 140 140 'solid 'green))
      =>
      (make-picture
       (make-overlay
        (make-translate 
         0 0
         (make-overlay
          (make-translate 0 0 (picture-shape (ellipse 100 100 'solid 'blue)))
          (make-translate 0 0 (picture-shape (ellipse 120 120 'solid 'red)))))
        (make-translate 0 0 (picture-shape (ellipse 140 140 'solid 'green))))
       (make-bb 140 140 140)
       #f))

(test (overlay/places 'middle
                      'middle
                      (ellipse 100 50 'solid 'green)
                      (ellipse 50 100 'solid 'red))
      =>
      (make-picture
       (make-overlay
        (make-translate 0 25 (picture-shape (ellipse 100 50 'solid 'green)))
        (make-translate 25 0 (picture-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100 100 100)
       #f))

(test (overlay/places 'middle
                      'middle
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-picture
       (make-overlay
        (make-translate 25 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 25 (picture-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))


(test (overlay/places 'right
                      'bottom
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-picture
       (make-overlay
        (make-translate 50 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 50 (picture-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))

(test (overlay/places 'right
                      'baseline
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-picture
       (make-overlay
        (make-translate 50 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 50 (picture-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))

(test (beside/places 'top
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 0 (picture-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/places 'center
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 25 (picture-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/places 'baseline
                     (ellipse 50 100 'solid 'red)
                     (ellipse 100 50 'solid 'blue))
      
      =>
      (make-picture
       (make-overlay
        (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 50 (picture-shape (ellipse 100 50 'solid 'blue))))
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
;;  testing rotation bounding boxes.
;;
#|
(test (simple-bb (make-translate 0 0 (make-rotate (* pi 1/4) (picture-shape (rectangle 100 50 'solid 'red)))))
      =>
      (values 0.0
              (- (imag-part (* (make-rectangular 100 0) (make-polar 1 (* pi 1/4)))))
              (real-part (* (make-rectangular 100 -50) (make-polar 1 (* pi 1/4)))) 
              (- (imag-part (* (make-rectangular 0 -50) (make-polar 1 (* pi 1/4)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing normalization
;;

(test (normalize-shape (picture-shape (ellipse 50 100 'solid 'red))
                       void)
      =>
      (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'red)))))

(test (normalize-shape (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                     (picture-shape (ellipse 50 100 'solid 'blue)))
                       void)
      =>
      (make-overlay (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'red))))
                    (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'blue))))))

(test (normalize-shape (make-overlay
                        (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                      (picture-shape (ellipse 50 100 'solid 'blue)))
                        (picture-shape (ellipse 50 100 'solid 'green)))
                       void)
      =>
      (make-overlay 
       (make-overlay (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'red))))
                     (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'blue)))))
       (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'green))))))

(test (normalize-shape (make-overlay
                        (picture-shape (ellipse 50 100 'solid 'green))
                        (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                      (picture-shape (ellipse 50 100 'solid 'blue))))
                       void)
      =>
      (make-overlay 
       (make-overlay (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'green))))
                     (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'red)))))
       (make-translate 0 0 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'blue))))))

(test (normalize-shape (make-translate 100 100 (picture-shape (ellipse 50 100 'solid 'blue)))
                       void)
      =>
      (make-translate 100 100 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (normalize-shape (make-translate 10 20 (make-translate 100 100 (picture-shape (ellipse 50 100 'solid 'blue))))
                       void)
      =>
      (make-translate 110 120 (make-rotate 0 (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (normalize-shape (make-rotate pi (picture-shape (ellipse 50 100 'solid 'blue)))
                       void)
      =>
      (make-translate 0 0 (make-rotate pi (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (normalize-shape (make-rotate (* pi 1/2) (make-rotate (* pi 1/2) (picture-shape (ellipse 50 100 'solid 'blue))))
                       void)
      =>
      (make-translate 0 0 (make-rotate pi (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (round-numbers
       (normalize-shape (make-rotate pi (make-translate 100 100 (picture-shape (rectangle 50 100 'solid 'blue))))
                        void))
      =>
      (round-numbers (make-translate -100 -100 (make-rotate pi (picture-shape (rectangle 50 100 'solid 'blue))))))


(test (round-numbers
       (normalize-shape (make-rotate (* pi 1/2) (make-translate 100 50 (picture-shape (rectangle 50 100 'solid 'blue))))
                        void))
      =>
      (round-numbers (make-translate 50 -100 (make-rotate (* pi 1/2) (picture-shape (rectangle 50 100 'solid 'blue))))))


(test (round-numbers
       (normalize-shape
        (make-rotate
         (* pi 1/4)
         (make-translate 
          100 100
          (picture-shape (rectangle 100 10 'solid 'red))))
        void))
      =>
      (round-numbers
       (make-translate
        (* 100 (sqrt 2))
        0.0
        (make-rotate
         (* pi 1/4)
         (picture-shape (rectangle 100 10 'solid 'red))))))

(test (round-numbers
       (normalize-shape
        (make-rotate
         (* pi 1/4)
         (make-translate 
          100 100
          (make-rotate
           (* pi 1/4)
           (make-translate 
            100 100
            (picture-shape (rectangle 100 10 'solid 'red))))))
        void))
      =>
      (round-numbers
       (make-translate
        200
        0
        (make-rotate
         (* pi 1/2)
         (picture-shape (rectangle 100 10 'solid 'red))))))

(test (round-numbers
       (normalize-shape
        (make-rotate
         (* pi 1/4)
         (make-translate 
          100 100
          (make-rotate
           (* pi 1/4)
           (make-translate 
            100 100
            (picture-shape (rectangle 100 10 'solid 'red))))))
        void))
      =>
      (round-numbers
       (make-translate
        (* (sqrt 2) 100 2)
        0
        (make-rotate
         (* pi 1/2)
         (picture-shape (rectangle 100 10 'solid 'red))))))

(test (round-numbers
       (normalize-shape
        (picture-shape 
         (rotate (* pi 1/8) (rotate (* pi 1/8) (rectangle 100 10 'solid 'red))))
        void))
      =>
      (round-numbers
       (normalize-shape
        (picture-shape 
         (rotate (* pi 1/4) (rectangle 100 10 'solid 'red)))
        void)))

|#