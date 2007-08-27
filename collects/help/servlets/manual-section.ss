(module manual-section mzscheme
  (require (lib "servlet.ss" "web-server")
           "../private/manuals.ss"
           "private/html.ss")
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       (let* ([bindings (request-bindings initial-request)]
              [manual (extract-binding/single 'manual bindings)]
              [raw-section (extract-binding/single 'section bindings)]
              ;; remove quotes
              [section (substring raw-section
                                  1 (sub1 (string-length raw-section)))]
              [page (with-handlers
                        ([void (lambda _
                                 (send/finish
                                  (html-page
                                   #:title "Can't find manual section"
                                   #:bodies
                                   `("Error looking up PLT manual section"
                                     (p "Requested manual: "
                                        ,manual (br)
                                        "Requested section: "
                                        ,section)))))])
                      (finddoc-page-anchor manual section))])
         (send/finish (redirect-to page)))))))
