(module highway mzscheme
  (let loop ([speed 0])
    (sleep 1)
    ;; Generate some fake speeds readings:
    (loop (+ speed 4))))