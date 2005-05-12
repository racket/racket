(module picture mzscheme
  
  (require (lib "graphics.ss" "graphics")
           (lib "math.ss"))
  
  (open-graphics)
  
  (define d 400)
  
  (define viewport (open-viewport "Blah" (* 2 d) (* 2 d)))
  
  (values 1 2 3)
  
  (define-syntax for
    (syntax-rules (=)
      [(_ (var = init) condn delta proc ...)
       (let loop ([var init])
         (when condn
           proc ...
           (loop (delta var))))])) 
  
  (for (i = (- d)) (< i d) add1
       (for (j = (- d)) (< j d) add1
            (when (<= (+ (sqr i) (sqr j)) (sqr d))
              ((draw-pixel viewport) (make-posn (+ d i) (+ d j))
                                     (if (or (zero? i) (zero? j))
                                         (make-rgb 0 0 1)
                                         (let ([i (- 1 (/ (abs (gcd i j)) (+ (abs i) (abs j))))])
                                           (make-rgb i i 1))))))))
