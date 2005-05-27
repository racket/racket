(module system mzscheme 
  (require (lib "servlet.ss" "web-server"))
  (require "../../private/headelts.ss"
           "../../../private/manuals.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "How to call low-level system routines")) 
      (BODY
       (H1 "How to call low-level system routines") 
       (A ((NAME "os") (VALUE "Low-level operating system calls")))
       "To call low-level system routines, you must write "
       "an extension to MzScheme using the C programming language. "
       "See " 
       ,(main-manual-page "insidemz")
       " for details."))))