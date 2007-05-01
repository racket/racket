(module inf-essence mzscheme  
  (define a (alarm-evt +inf.0))
  
  (let loop ()
    (sync
     (handle-evt a
                 (lambda _
                   (printf "Infinity has passed.~n")))
     (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                               (* 1000 1)))
                 (lambda _
                   (printf "One second has passed.~n"))))
    (loop)))
