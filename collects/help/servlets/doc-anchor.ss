(module doc-anchor mzscheme
  (require "private/read-doc.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    (let* ([bindings (request-bindings initial-request)]
           [offset (with-handlers
                       ((void (lambda _ #f)))
                     (string->number	 
                      (extract-binding/single 'offset bindings)))])
      (read-doc (extract-binding/single 'file bindings)
                (extract-binding/single 'caption bindings)
                (extract-binding/single 'name bindings)
                offset))))