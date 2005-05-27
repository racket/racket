(module value-turtles-examples mzscheme
  (require "value-turtles.ss"
           (lib "etc.ss")
           (lib "math.ss"))

  (provide regular-poly regular-polys radial-turtles spaced-turtles 
           spokes spyro-gyra neato)
  
  (define (regular-poly sides radius tv)
    (local [(define theta (/ (* 2 pi) sides))
            (define side-len (* 2 radius (sin (/ theta 2))))
            (define (draw-sides n tv)
              (cond
                [(zero? n) tv]
                [else
                 (draw-sides
                  (sub1 n)
                  (turn/radians
                   theta
                   (draw side-len tv)))]))]
      (merge
       (clean
        (draw-sides
         sides
         (turn/radians
          (/ (+ pi theta) 2)
          (move
           radius
           tv))))
       tv)))
  
  (define (regular-polys sides s tv)
    (local [(define (make-polys n tv)
              (cond
                [(zero? n) tv]
                [else
                 (make-polys 
                  (sub1 n)
                  (regular-poly 
                   sides 
                   (* n 5)
                   tv))]))]
      (make-polys sides tv)))
  
  (define (radial-turtles n tv)
    (cond
      [(zero? n) tv]
      [else
       (radial-turtles
        (sub1 n)
        (merge 
         tv
         (turn/radians 
          (/ pi (expt 2 (sub1 n)))
          tv)))]))
  
  (define (spaced-turtles n tv)
    (cond
      [(zero? n) tv]
      [else
       (spaced-turtles 
        (sub1 n)
        (merge
         tv
         (move 
          (expt 2 (+ n 1))
          tv)))]))
  
  (define (spokes turtles)
    (draw 
     10
     (turn/radians 
      (/ pi 2)
      (spaced-turtles
       5
       (radial-turtles 
        4
        turtles)))))
  
  (define (spyro-gyra turtles)
    (regular-poly 
     3
     100
     (radial-turtles 
      4
      turtles)))
  
  (define (neato turtles)
    (local [(define (spiral d t turtles)
              (cond
                [(<= 1 d)
                 (spiral 
                  (- d 1)
                  t               
                  (turn/radians 
                   t
                   (draw 
                    d
                    turtles)))]
                [else turtles]))]
      (spiral 
       30 
       (/ pi 12)
       (radial-turtles
        4
        turtles)))))