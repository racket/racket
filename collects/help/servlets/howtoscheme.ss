(module howtoscheme mzscheme
  (require "../private/manuals.ss")
  
  (require "private/headelts.ss")
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (TITLE "Software")
      (HEAD ,hd-css
            ,@hd-links)
      (BODY 
       (H1  "Software")  
       (UL  
        (LI  (B  (A ((HREF "howtodrscheme.ss")) "DrScheme")) 
             ": The programming environment")
        (LI  (B  (A ((HREF "/servlets/scheme/what.ss")) "Languages")) 
             ": The family of languages supported by PLT Software")
        (LI  (B  (A ((HREF "/servlets/scheme/how.ss")) "Software & Components")) 
             ": The full suite of PLT tools " 
             (BR) 'nbsp 'nbsp 'nbsp 'nbsp
             (FONT ((SIZE "-2")) 
                   (A ((HREF "/servlets/scheme/how.ss#installed-components")) "Installed Components") ", ..."))
        (LI  (B  (A ((href "/servlets/scheme/doc.ss")) "Documentation")) ": Organization and manuals       " 
             (BR) 'nbsp 'nbsp 'nbsp 'nbsp
             (FONT ((SIZE "-2")) 
                   (A ((HREF "/servlets/manuals.ss")) "Manuals") ", ...") )
        (LI  (B  (A ((HREF "scheme/misc.ss")) "Hints")) 
             ": How to do things in Scheme " )
        (LI  (B 
              ,(manual-entry "drscheme"
                             "frequently asked questions"
                             "FAQ"))
             ": Frequently asked questions")
        (LI  
         (B (A ((HREF "releaseinfo.ss")) "Release Information")) 
         ": License, notes, and known bugs"))))))