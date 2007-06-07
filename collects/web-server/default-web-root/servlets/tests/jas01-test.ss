(module jas01-test mzscheme
  (define start #f)
  (thread-wait
   (thread
    (lambda ()
      (set! start (dynamic-require "jas01.ss" 'start)))))
  (thread-wait
   (thread
    (lambda ()
      (printf "~S~n" (start 'foo))))))
