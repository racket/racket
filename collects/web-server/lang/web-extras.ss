(module web-extras mzscheme
  (require (lib "url.ss" "net")
           "web.ss"
           "../servlet/helpers.ss")
  (provide send/suspend/dispatch
           redirect/get)
  
  (define-syntax send/suspend/dispatch
    (syntax-rules ()
      [(_ response-generator)
       (extract-proc/url
        (send/suspend/url
         (lambda (k-url)
           (response-generator
            (lambda (proc)
              (embed-proc/url k-url proc))))))]))  
  
  (define (redirect/get)
    (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily)))))