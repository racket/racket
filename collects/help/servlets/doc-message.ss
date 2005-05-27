(module doc-message mzscheme
  (require "private/headelts.ss"
           "private/util.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    (let ([bindings (request-bindings initial-request)])
      `(HTML 
        (HEAD ,hd-css
              ,@hd-links
              (TITLE "PLT collection message"))
        (BODY 
         ,(format-collection-message 
           (extract-binding/single 'msg bindings))
         (HR))))))