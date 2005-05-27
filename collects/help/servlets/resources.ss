(module resources mzscheme
  (require "private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "External Resources"))
      (BODY 
       (H1  "External Resources")
       (P)
       "DrScheme is created by "
       (A ((HREF "http://www.plt-scheme.org/") (TARGET "_top")) "PLT") 
       ", the Programming Languages Team "
       "based at Northeastern University, the University of Utah, "
       "Brown University, and the University of Chicago. "
       "Here are some links related to our activities."
       (P)
       (UL  
        (LI  (B (A ((HREF "resources/teachscheme.ss"))
                   "TeachScheme! Workshops"))
             ": Free summer program")
        (LI  (B (A ((HREF "resources/libext.ss")) 
                   "Libraries")) 
             ": From PLT and contributors")
        (LI  (B (A ((HREF "resources/maillist.ss")) 
                   "Mailing Lists")) ": How to subscribe"))
       (P)
       "Also, the Schemers.org Web site provides links for "
       "many Scheme resources, including books, implementations, "
       "and libraries: " (A ((HREF "http://www.schemers.org/") 
                             (TARGET "_top")) "http://www.schemers.org/") "."))))