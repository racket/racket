(module highway mzscheme
  (let loop ([speed 0])
    (sleep 1)
    ;; Generate some fake speeds readings:
    (loop (* 5 (modulo (current-seconds) 20)))))