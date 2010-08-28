#lang racket/base
#|
;; snippet of code for experimentation
#lang racket/gui
(require 2htdp/image
         lang/posn
         (only-in lang/htdp-advanced equal~?))

(define images
  (list (rhombus 10 90 'solid 'black)
        (rotate 45 (square 10 'solid 'black))))

(define t (new text%))
(define f (new frame% [label ""] [width 600] [height 400]))
(define ec (new editor-canvas% [parent f] [editor t]))
(for ((i (in-list images))) (send t insert i) (send t insert " "))
(send f show #t)
|#

(require "../image.ss"
         (only-in "../../mrlib/image-core.ss" 
                  image%
                  make-image
                  image-shape
                  image-bb
                  image-normalized?
                  skip-image-equality-fast-path
                  make-overlay
                  make-translate
                  make-bb
                  normalize-shape
                  make-ellipse
                  make-polygon
                  make-point
                  make-crop
                  crop?
                  normalized-shape?
                  image-snip->image
                  to-img)
         (only-in "../private/image-more.ss" 
                  bring-between
                  swizzle)
         ; "../private/img-err.ss"
         "../../mrlib/private/image-core-bitmap.ss"
         lang/posn
         racket/math
         racket/class
         racket/file
         racket/gui/base
         rackunit
         (prefix-in 1: htdp/image)
         (only-in lang/htdp-advanced equal~?))

(require (for-syntax racket/base))
(define-syntax (test stx)
  (syntax-case stx ()
    [(test a => b)
     (with-syntax ([check-equal? (datum->syntax #'here 'check-equal? stx)])
       #`(begin
           ;(printf "running line ~a\n" #,(syntax-line stx))
           #,(quasisyntax/loc stx (check-equal? a b))
           (parameterize ([skip-image-equality-fast-path #t])
             #,(quasisyntax/loc stx (check-equal? a b)))))]))

(define-syntax (test/exn stx)
  (syntax-case stx ()
    [(test/exn a => b)
     (with-syntax ([check-equal? (datum->syntax #'here 'check-equal? stx)])
       #`(let ([reg b])
           (unless (regexp? reg)
             (error 'test/exn "expected a regular expression, got ~e" reg))
           ;(printf "running line ~a\n" #,(syntax-line stx))
           #,(quasisyntax/loc stx (check-regexp-match 
                                   reg
                                   (with-handlers ((exn:fail? exn-message)) a "NO EXN!")))))]))

;; test case: (beside (text "a"...) (text "b" ...)) vs (text "ab")

;(show-image (frame (rotate 30 (ellipse 200 400 'solid 'purple))))

(define-simple-check (check-close a b)
  (and (number? a)
       (number? b)
       (< (abs (- a b)) 0.001)))

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
;; circle vs ellipse
;;

(test (ellipse 40 40 'outline 'black)
      =>
      (circle 20 'outline 'black))
(test (ellipse 60 60 'solid 'red)
      =>
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
             (inexact->exact (ceiling (* (sin (* pi 1/4)) 100))))
(check-close (image-height (rotate 45 (rectangle 100 0 'solid 'blue)))
             (inexact->exact (ceiling (* (sin (* pi 1/4)) 100))))
(check-close (image-width (rotate 45 (rectangle 0 100 'solid 'blue)))
             (inexact->exact (ceiling (* (sin (* pi 1/4)) 100))))
(check-close (image-height (rotate 45 (rectangle 0 100 'solid 'blue)))
             (inexact->exact (ceiling (* (sin (* pi 1/4)) 100))))

(test (image-width (scale 4 (rectangle 10 10 'outline 'black)))
      =>
      40)
(test (image-width (rotate 90 (scale 4 (rectangle 10 10 'outline 'black))))
      =>
      40)

(test (image-width (scale 4 (rectangle 10 10 'solid 'black)))
      =>
      40)
(test (image-width (rotate 90 (scale 4 (rectangle 10 10 'solid 'black))))
      =>
      40)


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
      (inexact->exact (ceiling (* (cos (* pi 1/6)) 100))))
(test (image-height (rotate 30 (ellipse 100 0 'solid 'blue)))
      =>
      (inexact->exact (ceiling (* (sin (* pi 1/6)) 100))))
(check-close (image-width (rotate 30 (ellipse 0 100 'solid 'blue)))
             (* (sin (* pi 1/6)) 100))
(check-close (image-height (rotate 30 (ellipse 0 100 'solid 'blue)))
             (ceiling (* (cos (* pi 1/6)) 100)))

;; zero-sized htdp/image images should also work
(test (image-width (1:text "" 18 "blue"))
      =>
      0)
(test (image-height (1:rectangle 10 0 'solid "red"))
      =>
      0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  polygon equality
;;

(test (polygon (list (make-posn 0 0)
                     (make-posn 10 10)
                     (make-posn 10 0))
               "solid" "plum")
      =>
      (polygon (list (make-posn 10 10)
                     (make-posn 10 0)
                     (make-posn 0 0))
               "solid" "plum"))

(test (polygon (list (make-posn 0 0)
                     (make-posn 0 10)
                     (make-posn 10 10)
                     (make-posn 10 0))
               "solid" "plum")
      =>
      (rectangle 10 10 "solid" "plum"))

(test (polygon (list (make-posn 0 0)
                     (make-posn 0 10)
                     (make-posn 10 10)
                     (make-posn 10 0))
               "solid" "plum")
      =>
      (polygon (list (make-posn 0 0)
                     (make-posn 0 10)
                     (make-posn 10 10)
                     (make-posn 10 0)
                     (make-posn 0 0))
               "solid" "plum"))

(test (polygon (list (make-posn 0 0)
                     (make-posn 0 10)
                     (make-posn 10 10)
                     (make-posn 10 0))
               "outline"
               (make-pen "plum" 8 "solid" "round" "round"))
      =>
      (polygon (list (make-posn 0 0)
                     (make-posn 0 10)
                     (make-posn 10 10)
                     (make-posn 10 0)
                     (make-posn 0 0))
               "outline"
               (make-pen "plum" 8 "solid" "round" "round")))

;; make sure equality isn't equating everything
(test (equal? (rectangle 10 10 'solid 'blue)
              (rectangle 10 10 'solid 'red))
      =>
      #f)

;; make sure 'white and black match up with color structs
(test (rectangle 10 10 'solid (make-color 255 255 255))
      =>
      (rectangle 10 10 'solid 'white))
(test (rectangle 10 10 'solid (make-color 0 0 0))
      =>
      (rectangle 10 10 'solid 'black))

;; test zero sized image equalities

(test (rectangle 0 100 'solid 'white)
      =>
      (rectangle 0 100 'solid 'white))

(test (rectangle 0 100 'solid 'white)
      =>
      (rectangle 0 100 'solid 'black))

(test (rectangle 100 0 'solid 'white)
      =>
      (rectangle 100 0 'solid 'black))

(test (rectangle 0 0 'solid 'black)
      =>
      (rectangle 0 0 'solid 'orange))

(test (equal~? (rectangle 0 100 'solid 'white)
               (rotate 90 (rectangle 100 0 'solid 'black))
               .1)
      =>
      #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing overlays
;;

(test (overlay (ellipse 100 100 'solid 'blue)
               (ellipse 120 120 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 10 10 (image-shape (ellipse 100 100 'solid 'blue)))
        (make-translate 0 0 (image-shape (ellipse 120 120 'solid 'red))))
       (make-bb 120
                120
                120)
       #f))

(test (overlay/xy (ellipse 100 100 'solid 'blue)
                  -10 -10
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
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red))))
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
         10 10
         (make-overlay
          (make-translate 10 10 (image-shape (ellipse 100 100 'solid 'blue)))
          (make-translate 0 0 (image-shape (ellipse 120 120 'solid 'red)))))
        (make-translate 0 0 (image-shape (ellipse 140 140 'solid 'green))))
       (make-bb 140 140 140)
       #f))

(test (overlay/align 'middle
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

(test (overlay/align 'middle
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


(test (overlay/align 'right
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

(test (overlay/align 'right
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

(test (beside/align 'top
                    (ellipse 50 100 'solid 'red)
                    (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 0 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/align 'center
                    (ellipse 50 100 'solid 'red)
                    (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 50 25 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 150 100 100)
       #f))

(test (beside/align 'baseline
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
      (beside/align 'center
                    (ellipse 50 100 'solid 'red)
                    (ellipse 100 50 'solid 'blue)))

(test (above/align 'left
                   (ellipse 50 100 'solid 'red)
                   (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 100 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 100 150 150)
       #f))

(test (above/align 'center
                   (ellipse 50 100 'solid 'red)
                   (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 100 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 100 150 150)
       #f))

(test (above/align 'right
                   (ellipse 50 100 'solid 'red)
                   (ellipse 100 50 'solid 'blue))
      
      =>
      (make-image
       (make-overlay
        (make-translate 50 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 100 (image-shape (ellipse 100 50 'solid 'blue))))
       (make-bb 100 150 150)
       #f))

(test (above (ellipse 50 100 'solid 'red)
             (ellipse 100 50 'solid 'blue))
      =>
      (above/align 'center
                   (ellipse 50 100 'solid 'red)
                   (ellipse 100 50 'solid 'blue)))



(test (underlay (ellipse 100 100 'solid 'blue)
                (ellipse 120 120 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 0 0 (image-shape (ellipse 120 120 'solid 'red)))
        (make-translate 10 10 (image-shape (ellipse 100 100 'solid 'blue))))
       (make-bb 120
                120
                120)
       #f))

(test (underlay/xy (ellipse 100 100 'solid 'blue)
                   -10 -10
                   (ellipse 120 120 'solid 'red))
      =>
      (underlay (ellipse 100 100 'solid 'blue)
                (ellipse 120 120 'solid 'red)))


(test (underlay/xy (ellipse 50 100 'solid 'red)
                   -25 25
                   (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100
                100
                100)
       #f))

(test (underlay/xy (ellipse 100 50 'solid 'green)
                   10 10
                   (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 10 10 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 0 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100
                110
                110)
       #f))

(test (underlay (ellipse 100 50 'solid 'green)
                (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100
                100
                100)
       #f))

(test (underlay (ellipse 100 100 'solid 'blue)
                (ellipse 120 120 'solid 'red)
                (ellipse 140 140 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 
         0 0
         (make-overlay
          (make-translate 0 0 (image-shape (ellipse 140 140 'solid 'green)))
          (make-translate 10 10 (image-shape (ellipse 120 120 'solid 'red)))))
        (make-translate 10 10 (image-shape (ellipse 100 100 'solid 'blue))))
       (make-bb 140 140 140)
       #f))

(test (underlay/align 'middle
                      'middle
                      (ellipse 100 50 'solid 'green)
                      (ellipse 50 100 'solid 'red))
      =>
      (make-image
       (make-overlay
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red)))
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green))))
       (make-bb 100 100 100)
       #f))

(test (underlay/align 'middle
                      'middle
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 0 25 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 25 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100 100 100)
       #f))

(test (underlay/align 'right
                      'bottom
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 0 50 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 50 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100 100 100)
       #f))

(test (underlay/align "right"
                      "baseline"
                      (ellipse 50 100 'solid 'red)
                      (ellipse 100 50 'solid 'green))
      =>
      (make-image
       (make-overlay
        (make-translate 0 50 (image-shape (ellipse 100 50 'solid 'green)))
        (make-translate 50 0 (image-shape (ellipse 50 100 'solid 'red))))
       (make-bb 100 100 100)
       #f))

(test (empty-scene 185 100)
      =>
      (overlay/align "left" "top"
                     (rectangle 184 99 'outline 'solid)
                     (rectangle 185 100 'solid 'white)))

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

(test (normalize-shape (image-shape
                        (beside/align 'top
                                      (rectangle 10 10 'solid 'black)
                                      (crop 0 0 5 5 (rectangle 10 10 'solid 'green)))))
      =>
      (make-overlay
       (make-polygon
        (list (make-point 0 0)
              (make-point 10 0)
              (make-point 10 10)
              (make-point 0 10))
        'solid
        "black")
       (make-crop
        (list (make-point 10 0)
              (make-point 15 0)
              (make-point 15 5)
              (make-point 10 5))
        (make-polygon
         (list (make-point 10 0)
               (make-point 20 0)
               (make-point 20 10)
               (make-point 10 10))
         'solid
         "green"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  testing rotating
;;

(test (bring-between 123 360) => 123)
(test (bring-between 365 360) => 5)
(test (bring-between -5 360) => 355)
(test (bring-between 720 360) => 0)
(test (bring-between 720.5 360) => .5)

(test (equal~? (rotate 90 (rectangle 100 100 'solid 'blue))
               (rectangle 100 100 'solid 'blue)
               .1)
      =>
      #t)

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

(test (equal~? (rotate 
                90
                (overlay/xy (rectangle 20 100 'solid 'purple)
                            20 0
                            (ellipse 40 40 'solid 'orange)))
               (overlay/xy (rectangle 100 20 'solid 'purple)
                           0 -40
                           (ellipse 40 40 'solid 'orange))
               .1)
      =>
      #t)

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
;;  regular polygon
;;

;; note: the regular-polygon and the rectangle generate the points in reverse directions.
(test (round-numbers (regular-polygon 100 4 'outline 'green))
      =>
      (round-numbers (rectangle 100 100 'outline 'green)))

(test (swizzle (list 0 1 2 3 4) 2)
      =>
      (list 0 2 4 1 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  text
;;

(test (beside/align "baseline"
                    (text "a" 18 "black")
                    (text "b" 18 "black"))
      =>
      (text "ab" 18 "black"))

(test (round-numbers
       (image-width (rotate 45 (text "One" 18 'black))))
      =>
      (round-numbers
       (let ([t (text "One" 18 'black)])
         (image-width (rotate 45 (rectangle (image-width t) 
                                            (image-height t)
                                            'solid 'black))))))

(test (text "ab" 18 (make-color 0 0 255))
      =>
      (text "ab" 18 "blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; triangle
;;

(test (round-numbers (rotate 180 (isosceles-triangle 60 330 "solid" "lightseagreen")))
      =>
      (round-numbers (isosceles-triangle 60 30 "solid" "lightseagreen")))

(test (triangle 40 'outline 'black)
      =>
      (regular-polygon 40 3 'outline 'black))

(test (equal~? (rotate (+ 180 45) (right-triangle 50 50 'solid 'black))
               (isosceles-triangle 50 90 'solid 'black)
               0.001)
      =>
      #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; square
;;

(test (square 10 'solid 'black)
      =>
      (rectangle 10 10 'solid 'black))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rhombus
;;

(test (equal~? (rhombus 10 90 'solid 'black)
               (rotate 45 (square 10 'solid 'black))
               0.01)
      =>
      #t)

(test (equal~? (rhombus 50 150 'solid 'black)
               (rotate 90 (rhombus 50 30 'solid 'black))
               0.01)
      =>
      #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lines
;;

(test (image-width (line 10 20 'black))
      =>
      11)
(test (image-height (line 10 20 'black))
      =>
      21)

(test (round-numbers (rotate 90 (line 10 20 'black)))
      =>
      (round-numbers (line 20 -10 'black)))

(test (round-numbers (line 20 30 "red"))
      =>
      (round-numbers (rotate 180 (line 20 30 "red"))))

(test (round-numbers (line -30 20 "red"))
      =>
      (round-numbers (line 30 -20 "red")))

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             10 10 90 190 "red"))
      =>
      100)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              10 10 90 190 "red"))
      =>
      200)
(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             10 10 200 200 "red"))
      =>
      200)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              10 10 200 200 "red"))
      =>
      200)

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             10 10 300 300 "red"))
      =>
      300)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              10 10 300 300 "red"))
      =>
      300)

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             -10 10 100 200 "red"))
      =>
      110)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              -10 10 100 200 "red"))
      =>
      200)

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             10 -10 100 200 "red"))
      =>
      100)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              10 -10 100 200 "red"))
      =>
      210)

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             100 200 10 -10 "red"))
      =>
      100)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              100 200 10 -10 "red"))
      =>
      210)

(test (image-width (add-line (rectangle 100 200 'solid 'black)
                             100 200 -10 10 "red"))
      =>
      110)
(test (image-height (add-line (rectangle 100 200 'solid 'black)
                              100 200 -10 10 "red"))
      =>
      200)

(let* ([txt (text "H" 24 'black)]
       [bl (image-baseline txt)])
  (test (image-baseline (add-line txt 0 0 100 100 'red))
        =>
        bl))

(let* ([txt (text "H" 24 'black)]
       [bl (image-baseline txt)])
  (test (image-baseline (add-line txt 0 -10 100 100 'red))
        =>
        (+ bl 10)))

(test (scene+line (rectangle 100 100 'solid 'black)
                  10 10 
                  90 50
                  "red")
      =>
      (add-line (rectangle 100 100 'solid 'black)
                10 10 
                90 50
                "red"))

(test (image-width (scene+line (rectangle 100 100 'solid 'black)
                               -10 -20
                               110 120
                               "green"))
      =>
      100)
(test (image-height (scene+line (rectangle 100 100 'solid 'black)
                               -10 -20
                               110 120
                               'purple))
      =>
      100)
(test (image-baseline (scene+line (rectangle 100 100 'solid 'black)
                                  -10 -20
                                  110 120
                                  'olive))
      =>
      100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  curves
;;

;; make sure a curve stays roughly in the middle pixels by
;; covering up a white curve with a thin black bar
(test (overlay/align 'middle
                     'middle 
                     (rectangle 82 2 'solid 'black)
                     (add-curve (rectangle 100 20 'solid 'black)
                                10 10 0 1/4
                                90 10 0 1/4 
                                'white))
                      
      =>
      (rectangle 100 20 'solid 'black))

;; and then make sure the curve actually draws something ...
(test (not (equal? (add-curve (rectangle 100 20 'solid 'black)
                              10 10 0 1/4
                              90 10 0 1/4 
                              'white)
                   (rectangle 100 20 'solid 'black)))
      =>
      #t)

(test (scale 2
             (add-curve 
              (rectangle 100 100 'solid 'black)
              20 20 0 1/3 80 80 0 1/3 'white))
      =>
      (add-curve 
       (rectangle 200 200 'solid 'black)
       40 40 0 1/3 160 160 0 1/3 'white))

(test (rotate 
       90
       (add-curve 
        (rectangle 100 100 'solid 'black)
        20 20 0 1/3 80 80 0 1/3 'white))
      =>
      (add-curve 
       (rectangle 100 100 'solid 'black)
       20 80 90 1/3 80 20 90 1/3 'white))

(test (add-curve (rectangle 100 100 'solid 'black)
                 10 10 0 1/4
                 90 90 0 1/4
                 'white)
      =>
      (scene+curve (rectangle 100 100 'solid 'black)
                   10 10 0 1/4
                   90 90 0 1/4
                   'white))
(test (scene+curve (rectangle 100 100 'solid 'black)
                   10 10 0 1/4
                   110 110 0 1/4
                   'red)
      
      =>
      (crop 0 0 100 100
            (add-curve (rectangle 100 100 'solid 'black)
                       10 10 0 1/4
                       110 110 0 1/4
                       'red)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  bitmap tests
;;

(test (clamp-1 0 3 5)
      => 3)
(test (clamp-1 0 0 5)
      => 0)
(test (clamp-1 0 -2 5)
      => 0)
(test (clamp-1 0 4 5)
      => 4)
(test (clamp-1 0 7 5)
      => 4)

(test (build-bytes 5 sqr)
      => (list->bytes '(0 1 4 9 16)))


(define onePixel (list->bytes '(255 0 0 255)))
;(call-with-values (λ () (scale onePixel 1 1 100)) show-bitmap)

(define blue2x1 (list->bytes '(255 0 0 255   255 0 255 0)))
;(call-with-values (λ () (scale blue2x1 2 1 20)) show-bitmap)

(define blue2x2 (list->bytes '(255 0 0 255   255 0 0 255   255 0 0 255   255 0 0 255)))
(define gray2x2 (list->bytes '(255 100 100 100   255 100 100 100   255 100 100 100   255 100 100 100)))
;; Some blue x green checkerboards:
(define checker2x2 (list->bytes '(255 0 0 255   255 0 255 0
                                      255 0 255 0   255 0 0 255)))
(define checker3x3 (list->bytes '(255 0 0 255   255 0 255 0    255 0 0 255
                                      255 0 255 0   255 0 0 255    255 0 255 0
                                      255 0 0 255   255 0 255 0    255 0 0 255 )))


(test (bmbytes-ref/safe checker3x3 3 3 0 0)   => (list->bytes '(255 0 0 255)))
(test (bmbytes-ref/safe checker3x3 3 3 1 1)   => (list->bytes '(255 0 0 255)))
(test (bmbytes-ref/safe checker3x3 3 3 2 2)   => (list->bytes '(255 0 0 255)))
(test (bmbytes-ref/safe checker3x3 3 3 1 2)   => (list->bytes '(255 0 255 0)))
(test (bmbytes-ref/safe checker3x3 3 3 0 3)   => (list->bytes '(  0 0 0 255)))
(test (bmbytes-ref/safe checker3x3 3 3 -1 -1) => (list->bytes '(  0 0 0 255)))
(test (bmbytes-ref/safe checker3x3 3 3 -1 1)  => (list->bytes '(  0 0 255 0)))
(test (bmbytes-ref/safe checker3x3 3 3 1 19)  => (list->bytes '(  0 0 255 0)))

#;
(test (bytes->list (interpolate checker2x2 2 2 1 0))
      =>
      '(255 0 255 0))

#;
(test (bytes->list (interpolate checker3x3 3 3 0 0))
      =>
      '(255 0 0 255))

#;
(test (bytes->list (interpolate checker3x3 3 3 0 1))
      =>
      '(255 0 255 0))

#;
(test (bytes->list (interpolate checker3x3 3 3 0 2))
      =>
      '(255 0 0 255))

#;
(test (bytes->list (interpolate checker3x3 3 3 0.5 0))
      =>
      '(255 0 128 128))

(test (image-width (bitmap icons/stop-16x16.png))
      =>
      16)
(test (image-height (bitmap icons/stop-16x16.png))
      =>
      16)

(test (let ()
        (define bmp (make-object bitmap% 4 4))
        (define mask (make-object bitmap% 4 4))
        (define bdc (make-object bitmap-dc% bmp))
        (send bdc set-brush "black" 'solid)
        (send bdc draw-rectangle 0 0 4 4)
        (send bdc set-bitmap mask)
        (send bdc set-brush "black" 'solid)
        (send bdc clear)
        (send bdc draw-rectangle 1 1 1 1)
        (send bdc set-bitmap #f)
        (let-values ([(bytes w h) (bitmap->bytes bmp mask)])
          bytes))
      =>
      (bytes-append #"\0\0\0\0" #"\0\0\0\0"   #"\0\0\0\0" #"\0\0\0\0"
                    #"\0\0\0\0" #"\377\0\0\0" #"\0\0\0\0" #"\0\0\0\0"
                    #"\0\0\0\0" #"\0\0\0\0"   #"\0\0\0\0" #"\0\0\0\0"
                    #"\0\0\0\0" #"\0\0\0\0"   #"\0\0\0\0" #"\0\0\0\0"))

;; ensure no error
(test (begin (scale 2 (make-object bitmap% 10 10))
             (void))
      =>
      (void))


(define (fill-bitmap b color [x 0] [y 0] [w (send b get-width)] [h (send b get-height)])
  (let ([bdc (make-object bitmap-dc% b)])
    (send bdc set-brush color 'solid)
    (send bdc set-pen color 1 'transparent)
    (send bdc draw-rectangle x y w h)
    (send bdc set-bitmap #f)))

(define blue-10x20-bitmap (make-object bitmap% 10 20))
(fill-bitmap blue-10x20-bitmap "blue")
(define blue-20x10-bitmap (make-object bitmap% 20 10))
(fill-bitmap blue-20x10-bitmap "blue")
(define blue-20x40-bitmap (make-object bitmap% 20 40))
(fill-bitmap blue-20x40-bitmap "blue")

(define green-blue-10x20-bitmap (make-object bitmap% 10 20))
(fill-bitmap green-blue-10x20-bitmap "green")
(fill-bitmap green-blue-10x20-bitmap "blue" 0 0 10 10)

(define green-blue-20x10-bitmap (make-object bitmap% 20 10))
(fill-bitmap green-blue-20x10-bitmap "green")
(fill-bitmap green-blue-20x10-bitmap "blue" 10 0 10 10)

(test (image-width (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      10)
(test (image-height (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      20)
(test (image-baseline (image-snip->image (make-object image-snip% blue-10x20-bitmap)))
      => 
      20)
(test (scale 2 (make-object image-snip% blue-10x20-bitmap))
      =>
      (image-snip->image (make-object image-snip% blue-20x40-bitmap)))

(test (rotate 90 (make-object image-snip% blue-10x20-bitmap))
      =>
      (image-snip->image (make-object image-snip% blue-20x10-bitmap)))

(test (rotate 90 (make-object image-snip% green-blue-20x10-bitmap))
      =>
      (image-snip->image (make-object image-snip% green-blue-10x20-bitmap)))

;; there was a bug in the bounding box computation for scaled bitmaps that this test exposes
(test (image-width (frame (rotate 90 (scale 1/2 (bitmap icons/plt-logo-red-diffuse.png)))))
      =>
      128)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cropping (and place-image)
;;

(test (crop 0 0 10 10 (rectangle 20 20 'solid 'black))
      =>
      (rectangle 10 10 'solid 'black))

(test (equal~? (crop 0 0 40 40 (circle 40 'solid 'red))
               (rotate 180 (crop 40 40 40 40 (circle 40 'solid 'red)))
               0.1)
      =>
      #t)

(test (beside/align 'middle
                    (rectangle 10 10 'solid 'black)
                    (crop 0 0 10 10 (rectangle 10 10 'solid 'green)))
      =>
      (beside/align 'middle
                    (rectangle 10 10 'solid 'black)
                    (rectangle 10 10 'solid 'green)))

(test (place-image/align (circle 4 'solid 'black)
                         10 10
                         'left 'top
                         (rectangle 40 40 'solid 'orange))
      =>
      (underlay/xy (rectangle 40 40 'solid 'orange)
                   10 10
                   (circle 4 'solid 'black)))

(test (place-image/align (circle 4 'solid 'black)
                         50 50
                         'left 'top
                         (rectangle 40 40 'solid 'orange))
      =>
      (rectangle 40 40 'solid 'orange))

(test (place-image/align (circle 4 'solid 'black)
                         36 36
                         'left 'top
                         (rectangle 40 40 'solid 'orange))
      =>
      (underlay/xy (rectangle 40 40 'solid 'orange)
                   36 36
                   (crop 0 0 4 4 (circle 4 'solid 'black))))

(test (place-image/align (circle 8 'solid 'black)
                         -4 -4
                         'left 'top
                         (rectangle 40 40 'solid 'orange))
      =>
      (overlay/xy (crop 4 4 16 16 (circle 8 'solid 'black))
                  0 0
                  (rectangle 40 40 'solid 'orange)))

(test (place-image/align (circle 4 'solid 'black)
                         -4 0
                         'left 'top
                         (rectangle 40 40 'solid 'orange))
      =>
      (overlay/xy (crop 4 0 4 8 (circle 4 'solid 'black))
                  0 0
                  (rectangle 40 40 'solid 'orange)))

(test (place-image/align (circle 4 'solid 'black)
                         5 10 'center 'center
                         (rectangle 40 40 'solid 'orange))
      =>
      (underlay/xy (rectangle 40 40 'solid 'orange)
                   1 6
                   (circle 4 'solid 'black)))


(test (place-image/align (circle 4 'solid 'black)
                         10 15 'right 'bottom
                         (rectangle 40 40 'solid 'orange))
      =>
      (underlay/xy (rectangle 40 40 'solid 'orange)
                   2 7
                   (circle 4 'solid 'black)))

;; this test case checks to make sure the number of crops doesn't 
;; grow when normalizing shapes.

(let* ([an-image 
        (crop
         0 0 50 50
         (crop
          0 10 60 60
          (crop
           10 0 60 60
           (overlay
            (overlay
             (ellipse 20 50 'solid 'red)
             (ellipse 30 40 'solid 'black))
            (overlay
             (ellipse 20 50 'solid 'red)
             (ellipse 30 40 'solid 'black))))))]
       [an-image+crop
        (crop 5 5 10 10 an-image)])
  
  (define (count-crops s)
    (define crops 0)
    (let loop ([s s])
      (when (crop? s)
        (set! crops (+ crops 1)))
      (when (struct? s)
        (for-each loop (vector->list (struct->vector s)))))
    crops)
  
  (test (+ (count-crops (normalize-shape (image-shape an-image))) 1)
        =>
        (count-crops (normalize-shape (image-shape an-image+crop)))))

(check-exn #rx"crop" (λ () (crop 100 100 10 10 (rectangle 20 20 "solid" "black"))))
(check-exn #rx"crop" (λ () (crop 9 100 10 10 (rectangle 20 20 "solid" "black"))))
(check-exn #rx"crop" (λ () (crop 100 9 10 10 (rectangle 20 20 "solid" "black"))))
(check-exn #rx"crop" (λ () (crop -9 9 10 10 (rectangle 20 20 "solid" "black"))))
(check-exn #rx"crop" (λ () (crop 9 -9 10 10 (rectangle 20 20 "solid" "black"))))

(test (crop 20 20 100 100 (rectangle 40 40 "solid" "black"))
      =>
      (rectangle 20 20 "solid" "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  flipping
;;

(test (flip-horizontal (rotate -30 (rectangle 100 10 'solid 'red)))
      =>
      (rotate 30 (rectangle 100 10 'solid 'red)))

(test (flip-vertical (rotate -30 (rectangle 100 10 'solid 'red)))
      =>
      (rotate 30 (rectangle 100 10 'solid 'red)))
(test (flip-vertical
       (rotate
        -30
        (overlay (rectangle 100 10 'solid 'red)
                 (ellipse 10 100 'solid 'blue))))
      =>
      (rotate
       30
       (overlay (rectangle 100 10 'solid 'red)
                (ellipse 10 100 'solid 'blue))))
(test (flip-horizontal (overlay/xy (rectangle 100 10 'solid 'red)
                                   10 10
                                   (ellipse 10 100 'solid 'blue)))
      =>
      (overlay/xy (rectangle 100 10 'solid 'red)
                  80 10
                  (ellipse 10 100 'solid 'blue)))
(test (flip-vertical (overlay/xy (rectangle 100 10 'solid 'red)
                                 10 10
                                 (ellipse 10 100 'solid 'blue)))
      =>
      (overlay/xy (rectangle 100 10 'solid 'red)
                  10 -100
                  (ellipse 10 100 'solid 'blue)))

(test (flip-vertical (add-curve (rectangle 200 100 'solid 'black)
                                20 20 0 1
                                180 80 -90 1/3
                                "white"))
      =>
      (add-curve (rectangle 200 100 'solid 'black)
                 20 80 0 1
                 180 20 90 1/3
                 "white"))

(let* ([bdc (make-object bitmap-dc%)]
       [bm-ul (make-object bitmap% 10 10)]
       [bm-ur (make-object bitmap% 10 10)]
       [bm-ll (make-object bitmap% 10 10)])
  (send bdc set-bitmap bm-ul)
  (send bdc set-pen "red" 1 'transparent)
  (send bdc set-brush "red" 'solid)
  (send bdc clear)
  (send bdc draw-rectangle 0 0 5 5)
  (send bdc set-bitmap bm-ur)
  (send bdc set-pen "red" 1 'solid)
  (send bdc clear)
  (send bdc draw-rectangle 5 0 5 5)
  (send bdc set-bitmap bm-ll)
  (send bdc clear)
  (send bdc draw-rectangle 0 5 5 5)
  (send bdc set-bitmap #f)
  (test (flip-vertical bm-ul) => (to-img bm-ll))
  (test (flip-horizontal bm-ul) => (to-img bm-ur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pen arguments
;;

;; just make sure no errors.
(test (image? (polygon (list (make-posn 0 0)
                             (make-posn 100 100)
                             (make-posn 100 0)
                             (make-posn 0 100))
                       "outline"
                       (make-pen "darkslategray" 6 "solid" "round" "round")))
      =>
      #t)

(test (image? (line 10 
                    10 
                    (make-pen "darkslategray" 6 "solid" "round" "round")))
      =>
      #t)

(test (scale 2
             (polygon (list (make-posn 0 0)
                            (make-posn 100 0)
                            (make-posn 100 100))
                      "outline"
                      (make-pen "black" 6 "solid" "round" "round")))
      =>
      (polygon (list (make-posn 0 0)
                     (make-posn 200 0)
                     (make-posn 200 200))
               "outline"
               (make-pen "black" 12 "solid" "round" "round")))

(test (scale 2
             (ellipse 30 40 "outline"
                      (make-pen "black" 2 "solid" "round" "round")))
      =>
      (ellipse 60 80 "outline"
               (make-pen "black" 4 "solid" "round" "round")))

(test (scale 2
             (polygon (list (make-posn 0 0)
                            (make-posn 100 0)
                            (make-posn 100 100))
                      "outline"
                      (make-pen "black" 0 "solid" "round" "round")))
      =>
      (polygon (list (make-posn 0 0)
                     (make-posn 200 0)
                     (make-posn 200 200))
               "outline"
               (make-pen "black" 0 "solid" "round" "round")))

(test (scale 2
             (add-line
              (rectangle 100 100 'solid 'black)
              20 20 80 80
              (make-pen "black" 6 "solid" "round" "round")))
      =>
      (add-line
       (rectangle 200 200 'solid 'black)
       40 40 160 160
       (make-pen "black" 12 "solid" "round" "round")))

(test (scale 2
             (add-curve
              (rectangle 100 100 'solid 'black)
              20 20 0 1/2
              80 80 0 1/2
              (make-pen "black" 6 "solid" "round" "round")))
      =>
      (add-curve
       (rectangle 200 200 'solid 'black)
       40 40 0 1/2
       160 160 0 1/2
       (make-pen "black" 12 "solid" "round" "round")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test errors.
;;  mostly test that the extra mode check is there
;;

(test/exn (rectangle 10 10 "solid" (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^rectangle: expected <image-color>")

(test/exn (rectangle 10 10 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^rectangle: expected <image-color>")

(test/exn (circle 10 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^circle: expected <image-color>")

(test/exn (ellipse 10 10 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^ellipse: expected <image-color>")

(test/exn (triangle 10 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^triangle: expected <image-color>")

(test/exn (right-triangle 10 12 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^right-triangle: expected <image-color>")

(test/exn (isosceles-triangle 10 120 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^isosceles-triangle: expected <image-color>")

(test/exn (square 10 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^square: expected <image-color>")

(test/exn (rhombus 40 45 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^rhombus: expected <image-color>")

(test/exn (regular-polygon 40 6 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^regular-polygon: expected <image-color>")

(test/exn (star 40 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^star: expected <image-color>")

(test/exn (star-polygon 40 7 3 'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^star-polygon: expected <image-color>")

(test/exn (polygon (list (make-posn 0 0) (make-posn 100 0) (make-posn 100 100))
                   'solid (make-pen "black" 12 "solid" "round" "round"))
          =>
          #rx"^polygon: expected <image-color>")

(test/exn (save-image "tri.png" (triangle 50 "solid" "purple"))
          =>
          #rx"^save-image:")

(test/exn (pen 1 2 3 4 5)
          =>
          #rx"^pen:")

(test/exn (make-pen 1 2 3 4 5)
          =>
          #rx"^make-pen:")

(test/exn (make-color #f #f #f)
          =>
          #rx"^make-color:")
(test/exn (color #f #f #f)
          =>
          #rx"^color:")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  random testing of normalization
;;    make sure normalization actually normalizes
;;    and that normalization doesn't introduce new structs
;;

(require redex/reduction-semantics)

(define-language 2htdp/image
  (image (rectangle size size mode color)
         (line coord coord color)
         (add-curve (rectangle size size mode color)
                    coord coord pull angle
                    coord coord pull angle
                    color)
         (overlay image image)
         (overlay/xy image coord coord image)
         (underlay image image)
         (underlay/xy image coord coord image)
         (let ([i image])
           (crop (max 0 (min (image-width i) coord))
                 (max 0 (min (image-height i) coord))
                 size
                 size
                 i))
         (scale/xy factor factor image)
         (scale factor image)
         (rotate angle image)
         (flip-vertical image)
         (flip-horizontal image)
         (bitmap bmp-spec))
  
  (bmp-spec icons/b-run.png
            icons/stop-16x16.png)

  (factor (+ 1 big-nat) 1/2 1/3 1/4) ;; scaling factors
  (size big-nat)
  (mode 'outline 'solid "outline" "solid")
  (color "red" 'red "blue" "orange" "green" "black")
  (coord big-int)
  (pull 0 1/2 1/3 2 (/ big-nat (+ 1 big-nat)))
  (angle 0 90 45 30 180 natural (* 4 natural))
  
  ; Redex tends to choose small numbers.
  (big-nat (+ (* 10 natural) natural))
  (big-int (+ (* 10 integer) integer)))

(define-namespace-anchor anchor)

(define (image-struct-count obj)
  (let ([counts (make-hash)])
    (let loop ([obj obj])
      (when (struct? obj)
        (let ([stuff (vector->list (struct->vector obj))])
          (unless (member (car stuff) '(struct:flip struct:translate struct:scale)) ;; skip these becuase normalization eliminates them
            (hash-set! counts (car stuff) (+ 1 (hash-ref counts (car stuff) 0))))
          (for-each loop (cdr stuff)))))
    (sort (hash-map counts list) string<=? #:key (λ (x) (symbol->string (car x))))))

(define (check-image-properties img-sexp img)
  (let* ([raw-size (image-struct-count (image-shape img))]
         [normalized (normalize-shape (image-shape img) values)]
         [norm-size (image-struct-count normalized)]) 
    (unless (normalized-shape? normalized)
      (error 'test-image.ss "found a non-normalized shape after normalization:\n~s" 
             img-sexp))
    (unless (equal? norm-size raw-size)
      (error 'test-image.ss "found differing sizes for ~s:\n  ~s\n  ~s" 
             img-sexp raw-size norm-size))))


(time
 (redex-check
  2htdp/image
  image
  (check-image-properties 
   (term image)
   (to-img (eval (term image) (namespace-anchor->namespace anchor))))
  #:attempts 1000))

(define (test-save/load img fn)
  (let ([t1 (new text%)]
        [t2 (new text%)])
    (send t1 insert img)
    (send t1 save-file fn)
    (send t2 load-file fn)
    (let ([s1 (send t1 find-first-snip)]
          [s2 (send t2 find-first-snip)])
      (equal? s1 s2))))

;; scale-down : image -> image
;; scale image so that it is at most 10000 pixels in area
(define (scale-down img)
  (let* ([w (image-width img)]
         [h (image-height img)]
         [s (* w h)]
         [max-s (sqr 100)])
    (if (< s max-s)
        img
        (scale/xy (/ (sqrt max-s) w) 
                  (/ (sqrt max-s) h)
                  img))))

#;
(time
 (let ([fn (make-temporary-file "test-image~a")])
   (redex-check
    2htdp/image
    image
    (let-values ([(ans real cpu gc)
                  (time-apply
                   (λ ()
                     (let ([img (to-img (eval (term image) (namespace-anchor->namespace anchor)))])
                       (test-save/load (scale-down img) fn)))
                   '())])
      (unless (car ans)
        (error 'test-image.rkt
               "saving and loading this image fails:\n  ~s"
               (term image)))
      (unless (< cpu 2000)
        (error 'test-image.rkt
               "saving and loading this image takes too longer than 2 seconds:\n  ~s"
               (term image))))
    #:attempts 1000)))

;;This expression was found by the below. Its problematic because it has a negative width.
#;
(begin
  (define i
   (let* ([b (rectangle 17 17 "solid" "black")]
          [i (overlay/xy b -37 40 b)])
     (rotate 30 (crop 54 30 20 10 i))))
  (image-width i) (image-height i) i)

