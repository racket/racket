(module error mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (with-errors-to-browser
     send/finish
     (lambda ()
       (error 'error "I am an error, do you see me?")))))