(module jas01-fix-param mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide get-time)
  
  (define load-time
    (make-web-cell:local #f))
  
  (define (get-time)
    (web-cell:local-ref load-time))
  
  (web-cell:local-set! load-time (current-seconds)))
