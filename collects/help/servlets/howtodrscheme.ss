(module howtodrscheme mzscheme
  (require "private/headelts.ss"
           "../private/manuals.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (TITLE "DrScheme")	
      (HEAD ,hd-css
            ,@hd-links)
      (BODY 
       (H1 "DrScheme") 
       "DrScheme is PLT's flagship programming environment. "
       "See " (A ((HREF "/servlets/scheme/how.ss")) "Software & Components") 
       " for a guide to the full suite of PLT tools." 
       (UL  
        (LI  (B  (A ((HREF "/doc/tour/")) "Tour")) ": An introduction to DrScheme")
        (LI  (B  ,(manual-entry "drscheme" 
                                "graphical interface"
                                "Interface Essentials"))
             ": Quick-start jump into the user manual")
        (LI  (B  (A ((HREF "/servlets/scheme/what.ss")) 
                    "Languages")) 
             ": Languages supported by DrScheme")
        (LI  (B  ,(main-manual-page "drscheme")) ": The complete user manual"))))))