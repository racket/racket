(module montecarlo mzscheme
  ;; a seed specially chosen because it isn't terribly erratic when converging on pi
  (random-seed 846259386)
  
  (define (run)
    (let loop ([hits 1]
               [total 1])
      (let* ([x (- (random 400) 200)]
             [y (- (random 400) 200)]
             [length (sqrt (+ (* x x) (* y y)))]
             [pi (* 4. (/ hits total))])
      (cond [(length . <= . 200) (loop (add1 hits) (add1 total))]
            [else (loop hits (add1 total))]))))
  (run))