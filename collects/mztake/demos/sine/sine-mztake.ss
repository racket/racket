(require (lib "animation.ss" "frtime")
         (lib "useful-code.ss" "mztake"))
(require (lib "mztake.ss" "mztake"))

(define/bind (loc "sine.ss" '(if _) ) x sin-x)

(define (pick-cute-color x y)
  (if (< 200 y)
      (if (< 200 x) "blue" "darkblue")
      (if (< 200 x) "red" "darkred")))

(define (make-cute-circle x y)
  (make-circle (make-posn x y)
               5
               (pick-cute-color x y)))

(display-shapes
 (list* (make-line (make-posn 0 200) (make-posn 400 200) "gray")
        (make-line (make-posn 200 0) (make-posn 200 400) "gray")
        
        (let ([x (+ 200 x)]
              [sin-x (+ 200 (* 100 sin-x))])
          (history-b (changes (make-cute-circle x sin-x)) 50))))

(set-running! #t)

