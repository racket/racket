(module resources mzscheme
  (require "private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "External Resources"))
      (body
       (h1  "External Resources")
       (p)
       "DrScheme is created by "
       (a ([href "http://www.plt-scheme.org/"] [target "_top"]) "PLT")
       " based at Northeastern University, the University of Utah,"
       " Brown University, and the University of Chicago."
       " Here are some links related to our activities."
       (p)
       (ul (li (b (a ([href "resources/teachscheme.ss"])
                     "TeachScheme! Workshops"))
               ": Free summer program")
           (li (b (a ([href "resources/libext.ss"]) "Libraries"))
               ": From PLT and contributors")
           (li (b (a ([href "resources/maillist.ss"]) "Mailing Lists"))
               ": How to subscribe"))
       (p)
       "Also, the Schemers.org Web site provides links for "
       "many Scheme resources, including books, implementations, "
       "and libraries: "
       (a ([href "http://www.schemers.org/"] [target "_top"])
          "http://www.schemers.org/") "."))))
