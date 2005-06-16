(require (lib "mztake.ss" "mztake")
	 (lib "animation.ss" "frtime"))

(define/bind (loc "highway.ss" 3 4) speed)

(printf-b "current speed: ~a" (hold values-of-speed))
(printf-b "last ten speeds: ~a" (history-b 10 values-of-speed))


(map-e (lambda (a-speed) (when (>= a-speed 55) (pause radar-program)))
       values-of-speed)


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


(display-shapes (make-speed-gauge (hold values-of-speed)))

(set-runnning! true)
