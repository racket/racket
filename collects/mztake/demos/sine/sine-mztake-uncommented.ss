(require (lib "animation.ss" "frtime"))
(require (lib "mztake-syntax.ss" "mztake"))

(define-mztake-process p ("sine.ss" [x/sinx-trace 5 8 bind '(x sin-x)]))


(define x/sinx (hold x/sinx-trace))


(define x     (first x/sinx))
(define sin-x (second x/sinx))


(printf-b "x: ~a" x)
(printf-b "sin(x/20): ~a" sin-x)


(printf-b "largest  x: ~a  sin(x/20): ~a"
          (largest-val-b (changes (first x/sinx)))
          (largest-val-b (changes (second x/sinx))))

(printf-b "smallest x:~a  sin(x/20):~a"
          (smallest-val-b (changes (first x/sinx)))
          (smallest-val-b (changes (second x/sinx))))


(display-shapes
 (list* (make-line (make-posn 0 200) (make-posn 400 200) "gray")
        (make-line (make-posn 200 0) (make-posn 200 400) "gray")
        
        (let ([x (+ 200 x)]
              [sin-x (+ 200 (* 100 sin-x))])
          (history-b 50 (changes (make-circle
                                  (make-posn x sin-x)
                                  5
                                  (if (< 200 sin-x)
                                      (if (< 200 x) "blue" "darkblue")      #| Quadrants 3 and 4 |#
                                      (if (< 200 x) "red" "darkred")))))))) #|           1 and 2 |#


(start/resume p)