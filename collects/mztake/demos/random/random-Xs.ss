(module |random-Xs| mzscheme
  (define (run)
    (let loop ([x (random 100)])
      (loop (random 100))))
  (run))