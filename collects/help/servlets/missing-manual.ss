(module missing-manual mzscheme
  (require (lib "servlet.ss" "web-server"))
  
  (require "private/headelts.ss")
  (require "private/util.ss"
           "../private/standard-urls.ss")
  
  (define (no-manual manual label link)
    (let* ([html-url (make-docs-html-url manual)]
           [plt-url (make-docs-plt-url manual)])
      `(html
        (head ,hd-css
              ,@hd-links 
              (TITLE "Missing PLT manual")) 
        (body ((bgcolor "white")) 
              ,(color-with "red" `(h1 "Documentation missing"))
              (P)
              "You tried to access documentation for "
              ,(color-with "blue" `(B ,label)) ". "
              "The documentation is not installed on this "
              "machine, probably because it is not part of the "
              "standard DrScheme distribution."
              (p)
              
              (h2 "Install Locally")
              (a ((href ,plt-url)) "Download and/or install")
              " the documentation."
              (br)
              "After installing, "
              (a ((href ,link)) "continue")
              " to the originally requested page."
              
              (br) nbsp (br)
              
              (h2 "Read Online")
              "Read the documentation on "
              (a ((href ,html-url)) "PLT's servers")
              "."))))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    (let ([bindings (request-bindings initial-request)])
      (no-manual (extract-binding/single 'manual bindings)
                 (extract-binding/single 'name bindings)
                 (extract-binding/single 'link bindings)))))
