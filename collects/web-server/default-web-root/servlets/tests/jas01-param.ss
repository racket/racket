(module jas01-param mzscheme
  (provide get-time)

  (define load-time (make-parameter #f))

  (define (get-time)
    (load-time))

  (load-time (current-seconds)))
