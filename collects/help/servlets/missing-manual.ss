(module missing-manual mzscheme
  (require (lib "servlet.ss" "web-server")
           "../private/standard-urls.ss"
           "private/util.ss"
           "private/html.ss")
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)

  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       (let ([bindings (request-bindings initial-request)])
         (no-manual (extract-binding/single 'manual bindings)
                    (extract-binding/single 'name bindings)
                    (extract-binding/single 'link bindings))))))

  (define (no-manual manual label link)
    (let* ([html-url (make-docs-html-url manual)]
           [plt-url (make-docs-plt-url manual)])
      (html-page
       #:title "Missing PLT manual"
       #:bodies
       `(,(with-color "red" `(h1 "Documentation missing"))
          (p "You tried to access documentation for "
             ,(with-color "blue" `(b ,label)) ". "
             "The documentation is not installed on this machine, probably"
             " because it is not part of the standard DrScheme distribution.")
          (br)
          (h2 "Install Locally")
          (p (a ([href ,plt-url]) "Download and/or install")
             " the documentation."
             (br)
             "After installing, "
             (a ((href ,link)) "continue")
             " to the originally requested page."
             (br) nbsp (br))
          (h2 "Read Online")
          (p "Read the documentation on "
             (a ((href ,html-url)) "PLT's servers")
             "."))))))
