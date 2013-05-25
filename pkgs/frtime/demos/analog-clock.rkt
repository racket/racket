#lang frtime
;; Analog Clock
;; Written by Robb Cutler
;; July, 2004
;;
;; Demonstrates animation of an analog clock using the FrTime language.


;; Require the animation library, the gui library, and the library
;; containing the build-list function.
(require frtime/animation
         frtime/gui)

(define clock-radius (make-slider "Clock Size" 40 200 100))

;; distance : number number number number -> number
;; Returns the distance between (x1, y1) and (x2, y2).
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(define clicks-in-clock
  (left-clicks . =#> . (lambda (_)
                         (snapshot (mouse-pos clock-center clock-radius)
                                   (<= (distance (posn-x mouse-pos)
                                                 (posn-y mouse-pos)
                                                 (posn-x clock-center)
                                                 (posn-y clock-center))
                                       clock-radius)))))
                         
(define offset
  (hold
   (clicks-in-clock
    . -=> .
    (snapshot (mouse-pos clock-center)
              (posn- mouse-pos clock-center)))
   #;(make-posn 0 0)))

;; Define follow-mouse which is true when the center of the clock
;; should be at the mouse cursor; false when it is at the last 
;; click position. Clicking the left button of the mouse
;; toggles this value.
(define follow-mouse?
  (hold (merge-e
         clicks-in-clock
         (left-releases . -=> . false)) #f))

;; Define the position of the center and the radius of the clock.
(define clock-center
  (rec p
    (inf-delay
     (until (make-posn 200 200)
            (let ([p1 0])
              (if follow-mouse?
                  (posn- mouse-pos offset)
                  p))))))

;; Define the length of the hands in terms of the radius of the clock.
(define second-hand-length (- clock-radius 5))
(define minute-hand-length (- clock-radius 5))
(define hour-hand-length (* .60 clock-radius))

;; Define the position of the numbers of on the clock face
;; in terms of the radius of the clock.  Because text is positioned using
;; the lower right corner of the text box (rather than using the center
;; of the text box), the numbers are drawn on a circle offset 
;; slightly from the clock face.
(define number-position (- clock-radius 10))
(define number-offset-x -4)
(define number-offset-y 4)

;; Define the time-dependent variables.
(define the-date (seconds->date seconds))
(define the-hour (date-hour the-date))
(define the-minute (date-minute the-date))
(define the-second (date-second the-date))

;; create-posn : posn number number number -> posn
;; Creates a position that is of length radius from the given center
;; The angle from the center is based on value/slots percent clockwise
;; around the center from 12 o'clock.  For example, given value of 35
;; and slots of 60, the position would be at 7 o'clock.
(define (create-posn center radius slots value)
  (make-posn (+ (posn-x center)
                (* radius (cos (+ (* 3 (/ pi 2)) 
                                  (* value (/ (* 2 pi) slots))))))
             (+ (posn-y center)
                (* radius (sin (+ (* 3 (/ pi 2)) 
                                  (* value (/ (* 2 pi) slots))))))))

;; create-number : number -> graph-string
;; Creates the n-th number for the clock face where n is 0-11.
(define (create-number n)
  (make-graph-string (create-posn 
                      (make-posn (+ (posn-x clock-center) number-offset-x)
                                 (+ (posn-y clock-center) number-offset-y))
                      number-position 12 (+ n 1))
                     (number->string (+ n 1))
                     "black"))

;; The clock face is a blue circle and a the numbers around its edge.
(define clock-face
  (list (make-circle clock-center clock-radius (if follow-mouse?
                                                   "lightblue"
                                                   "white"))
        (make-circle clock-center (/ clock-radius 20) "black")
        (make-ring clock-center clock-radius "blue")
        (build-list 12 create-number)))

;; Define the hour hand of the clock.
;; The hour hand is based on the-hour and the-minute in order to
;; make it move smoothly around the clock.
(define hour-hand
  (make-line clock-center
             (create-posn clock-center
                          hour-hand-length 12
                          (+ the-hour (/ the-minute 60)))
             "black"))

;; Define the minute hand of the clock.
(define minute-hand   
  (make-line clock-center
             (create-posn clock-center
                          minute-hand-length 60 the-minute)
             "black"))

;; Define the second hand of the clock.
(define second-hand
  (make-line clock-center
             (create-posn clock-center
                          second-hand-length 60 the-second)
             "red"))

;; Define the clock as the face and the three hands.
(define analog-clock 
  (list clock-face hour-hand minute-hand second-hand))

;; Draw the clock!
(display-shapes
 (list analog-clock (make-graph-string (make-posn 20 20) "Drag me around!" "black")))
