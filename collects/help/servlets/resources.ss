(module resources mzscheme
  (require (lib "servlet.ss" "web-server")
           "private/html.ss")
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       (html-page
        #:title "External Resources"
        #:bodies
        `((h1  "External Resources")
          (p "DrScheme is created by "
             (a ([href "http://www.plt-scheme.org/"] [target "_top"]) "PLT")
             " based at Northeastern University, the University of Utah,"
             " Brown University, and the University of Chicago."
             " Here are some links related to our activities.")
          (ul (li (b (a ([href "resources/teachscheme.ss"])
                        "TeachScheme! Workshops"))
                  ": Free summer program")
              (li (b (a ([href "resources/libext.ss"]) "Libraries"))
                  ": From PLT and contributors")
              (li (b (a ([href "resources/maillist.ss"]) "Mailing Lists"))
                  ": How to subscribe"))
          (p "Also, the Schemers.org Web site provides links for "
             "many Scheme resources, including books, implementations, "
             "and libraries: "
             (a ([href "http://www.schemers.org/"] [target "_top"])
                "http://www.schemers.org/") ".")))))))

