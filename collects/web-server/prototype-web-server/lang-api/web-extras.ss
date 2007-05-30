(module web-extras mzscheme
  (require (lib "url.ss" "net")
           "../private/web.ss"
           (only "../../servlet/helpers.ss"
                 extract-user-pass
                 redirect-to
                 permanently
                 temporarily
                 see-other
                 request-bindings
                 request-headers))
  (provide send/suspend/dispatch
           redirect/get
           extract-user-pass
           redirect-to
           permanently
           temporarily
           see-other
           request-bindings
           request-headers)
  
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