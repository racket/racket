(module counter mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start _)
    (main-page))
  
  (define the-counter (make-parameter 0))
  
  (define (counter)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (h2 ,(number->string (the-counter)))
              (a ([href ,(embed/url
                          (lambda _
                            (parameterize ([the-counter (add1 (the-counter))])
                              (counter))))])
                 "Increment")
              (br)
              (a ([href ,(embed/url
                          (lambda _
                            'exit))])
                 "Exit")))))
  
  (define (main-page)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (h2 "Main page")
              (a ([href ,(embed/url
                          (lambda _
                            (counter)
                            (main-page)))])
                 "View Counter"))))))
