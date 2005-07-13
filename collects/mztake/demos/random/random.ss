(module random mzscheme
  (define (run)
    (let loop ([x (random 20)])
      (loop (random 20))))
  (run))