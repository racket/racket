#lang scheme/base
(require "../../2htdp/private/picture.ss" 
         scheme/math
         tests/eli-tester)

#;
(show-picture
 (let loop ([picture (rectangle 400 8 'solid 'red)]
            [n 2])
   (cond
     [(= n 7) picture]
     [else
      (loop (overlay/places 'center 'center
                            picture
                            (rotate (* pi (/ 1 n)) picture))
            (+ n 1))])))

(define-syntax-rule 
  (round-numbers e)
  (call-with-values (λ () e) round-numbers/values))

(define (round-numbers/values . args) (apply values (round-numbers/proc args)))

(define (round-numbers/proc x)
  (let loop ([x x])
    (cond
      [(number? x) (/ (round (* 100. x)) 100)]
      [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
      [(vector? x) (apply vector (map loop (vector->list x)))]
      [(let-values ([(a b) (struct-info x)]) a)
       =>
       (λ (struct-type)
         (apply
          (struct-type-make-constructor
           struct-type)
          (map loop (cdr (vector->list (struct->vector x))))))]
      [else x])))

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
;;  testing normalization
;;

(test (normalize-shape (picture-shape (ellipse 50 100 'solid 'red))
                       values)
      =>
      (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red))))

(test (normalize-shape (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                     (picture-shape (ellipse 50 100 'solid 'blue)))
                       values)
      =>
      (make-overlay (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red)))
                    (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (normalize-shape (make-overlay
                        (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                      (picture-shape (ellipse 50 100 'solid 'blue)))
                        (picture-shape (ellipse 50 100 'solid 'green)))
                       values)
      =>
      (make-overlay 
       (make-overlay (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red)))
                     (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'blue))))
       (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'green)))))

(test (normalize-shape (make-overlay
                        (picture-shape (ellipse 50 100 'solid 'green))
                        (make-overlay (picture-shape (ellipse 50 100 'solid 'red))
                                      (picture-shape (ellipse 50 100 'solid 'blue))))
                       values)
      =>
      (make-overlay 
       (make-overlay (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'green)))
                     (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'red))))
       (make-translate 0 0 (picture-shape (ellipse 50 100 'solid 'blue)))))

(test (normalize-shape (make-translate 100 100 (picture-shape (ellipse 50 100 'solid 'blue)))
                       values)
      =>
      (make-translate 100 100 (picture-shape (ellipse 50 100 'solid 'blue))))

(test (normalize-shape (make-translate 10 20 (make-translate 100 100 (picture-shape (ellipse 50 100 'solid 'blue))))
                       values)
      =>
      (make-translate 110 120 (picture-shape (ellipse 50 100 'solid 'blue))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing rotating
;;


(test (round-numbers
       (simple-bb
        (make-translate
         50.0
         0
         (make-polygon
          (list (make-point 0 0) (make-point 50 0) (make-point 50 100) (make-point 0 100))
          pi
          'pen
          'brush))))
      =>
      (values 0. -100. 50. 0.))


(test (normalize-shape (picture-shape (rotate pi (rectangle 50 100 'solid 'blue)))
                       values)
      =>
      (make-translate 50.0 100.0 (rotate-atomic pi (picture-shape (rectangle 50 100 'solid 'blue)))))

(test (rotate-simple (* pi 1/2)
                     (rotate-simple (* pi 1/2)
                                    (make-translate 0 0
                                                    (picture-shape (rectangle 50 100 'solid 'purple)))))
      =>
      (rotate-simple pi
                     (make-translate 0 0 (picture-shape (rectangle 50 100 'solid 'purple)))))

(test (normalize-shape (picture-shape (rotate (* pi 1/2) (rotate (* pi 1/2) (rectangle 50 100 'solid 'blue))))
                       values)
      =>
      (make-translate 50.0 100.0 (rotate-atomic pi (picture-shape (rectangle 50 100 'solid 'blue)))))

(test (round-numbers
       (normalize-shape
        (picture-shape
         (rotate pi
                 (overlay/xy (rectangle 50 50 'solid 'blue)
                             50 50
                             (rectangle 50 50 'solid 'red))))
        values))
      =>
      (round-numbers 
       (normalize-shape
        (picture-shape
         (overlay/xy (rectangle 50 50 'solid 'red)
                     50 50
                     (rectangle 50 50 'solid 'blue))))))
