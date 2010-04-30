#lang frtime
(require frtime/animation)

; the mouse position delayed
(display-shapes
 (let* ([n 4]
        [n-1 (sub1 n)])
   (build-list
    n
    (lambda (i)
      (make-circle (delay-by mouse-pos (* 200 (- n-1 i)))
                   (+ 7 i)
                   (make-rgb (/ (- n-1 i) n) (/ (- n-1 i) n) 1.0))))))
