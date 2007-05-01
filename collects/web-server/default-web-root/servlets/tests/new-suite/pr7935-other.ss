(module pr7935-other mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  
  (define (start initial-request)
    ;(report-errors-to-browser send/back)
    (/ 1 0)))
