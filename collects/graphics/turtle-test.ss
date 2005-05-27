(module turtle-test mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           "turtle-examples.ss"
           "turtles.ss")
  
  (define frame (make-object frame% "Turtle Examples"))
  
  (define options
    `(("Triangle" ,(lambda () (regular-poly 3 100)))
      ("Hexagons" ,(lambda () (regular-polys 6 10)))
      ("Spokes" ,spokes)
      ("Spyro Gyra" ,spyro-gyra)
      ("Neato" ,neato)
      ("Graphics BExam" ,(lambda () 
                           (turn/radians (/ pi 4))
                           (move -150)
                           (turn/radians (- (/ pi 4)))
                           (graphics-bexam)))
      ("Serpinski (split)" ,(lambda () (serp serp-size)))
      ("Serpinski (no split)" ,(lambda () (serp-nosplit serp-size)))
      ("Koch (split)" ,(lambda () (koch-split koch-size)))
      ("Koch (no split)" ,(lambda () (koch-draw koch-size)))
      ("Lorenz Attractor" ,lorenz1)
      ("Peano" ,(lambda () (peano-position-turtle) (peano peano-size)))
      ("Fern" ,(lambda () (turn/radians (/ pi 2)) (move -100) (fern1 fern-size)))
      ("Gapped Lines" ,(lambda () (gapped-lines)))))
  
  (for-each (lambda (test) 
              (send
               (make-object button% 
                 (car test)
                 frame
                 (lambda x 
                   (turtles #t)
                   (clear)
                   (yield)
                   ((cadr test))))
               stretchable-width #t))
            options)
  
  (make-object grow-box-spacer-pane% frame)	  
  
  (send frame show #t))