(require (lib "mztake.ss" "mztake")
	 (lib "animation.ss" "frtime")
         (lib "useful-code.ss" "mztake"))

(define/bind (loc "highway.ss" 4) speed)

(printf-b "current speed: ~a" speed)

(define (make-speed-gauge speed)
  (let ([center (make-posn 200 200)])
    (list (make-circle center 170 "black")
          (make-circle center 160 "white")
          (make-rect (make-posn 0 202) 1000 1000 "white")
          (make-line (make-posn 30 201) (make-posn 370 201) "black")
          (make-line center
                     (posn+ center (make-posn (- (* 150 (cos (/ speed 30))))
                                              (- (* 150 (sin (/ speed 30))))))
                     "red"))))


(display-shapes (make-speed-gauge speed))

(set-running! (< speed 55))