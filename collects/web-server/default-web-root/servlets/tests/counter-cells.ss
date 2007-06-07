(module counter-cells mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start _)
    (main-page))
  
  (define the-counter (make-web-cell 0))
  (define the-header (make-web-cell (box "Main page")))
  
  (define (counter)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (h2 ,(number->string (web-cell-ref the-counter)))
              (a ([href ,(embed/url
                          (lambda _
                            (web-cell-shadow the-counter 
                                                 (add1 (web-cell-ref the-counter)))
                            (counter)))])
                 "Increment")
              (br)
              (a ([href ,(embed/url
                          (lambda _
                            'exit))])
                 "Exit")))))
  
  (define (main-page)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (h2 ,(unbox (web-cell-ref the-header)))
              (form ([method "POST"]
                     [action ,(embed/url
                               (lambda (req)
                                 (set-box! (web-cell-ref the-header)
                                           (extract-binding/single 'header (request-bindings req)))
                                 (main-page)))])
                    (input ([type "text"] [name "header"]))
                    (input ([type "submit"])))
              (br)
              (a ([href ,(embed/url
                          (lambda _
                            (counter)
                            (main-page)))])
                 "View Counter"))))))