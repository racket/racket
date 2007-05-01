(module fault mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout 30)
  (define X 0)
  (define (start request)
    (send/suspend
     (lambda (k-url)
       `(a ([href ,k-url]) "Click")))
    (set! X (add1 X))
    (format "~a" "fault")))
