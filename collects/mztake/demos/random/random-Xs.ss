(module |random-Xs| mzscheme
  (define (run)
    (let loop ([x (random 200)])
      (loop (random 200))))
  (run))