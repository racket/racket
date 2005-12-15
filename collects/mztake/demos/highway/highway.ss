(module highway mzscheme
  (let ([nap-time 0.8])
    (let loop ([speed 0])
      (sleep nap-time)
      ;; Generate some fake speeds readings:
      (loop (+ speed 4)))))