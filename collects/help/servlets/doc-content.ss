(module doc-content mzscheme
  
  (require "private/headelts.ss")
  (require "private/read-lines.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    (let* ([bindings (request-bindings initial-request)]
           [file (extract-binding/single 'file bindings)]
           [caption (extract-binding/single 'caption bindings)]
           [offset (with-handlers 
                       ((void (lambda _ #f)))
                     (string->number 
                      (extract-binding/single 'offset bindings)))])
      `(HTML
        (HEAD (TITLE "PLT Help Desk") 
              ,hd-css
              ,@hd-links)
        ,(read-lines file caption offset)))))