(module sine mzscheme
  (define (run)
    (let loop ([x -200])
      (let ([sin-x (sin (/ x 20.0))])
        (if (x . < . 200)
            (loop (add1 x))
            (loop -200)))))
  (run))