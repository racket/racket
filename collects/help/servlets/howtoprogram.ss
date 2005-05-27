(module howtoprogram mzscheme
  (require "private/util.ss"
           "private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (TITLE "Program Design")
      (HEAD ,hd-css
            ,@hd-links)
      (BODY 
       (H1  "Program Design")
       ,(color-highlight `(H2  "For Students"))
       "The textbook " (I  "How to Design Programs") 
       " provides an introduction to programming using the DrScheme environment. "
       "The book is not distributed with DrScheme, but it is available online at " 
       (PRE  
        "   " (A ((HREF "http://www.htdp.org/")
                  (TARGET "_top")) "http://www.htdp.org/"))  
       (P)
       "Help Desk provides the following interactive support for the textbook:"
       (UL  
        (LI  (B  (A ((HREF "/doc/teachpack/index.html"))
                    "Teachpack documentation"))))
       (P)
       ,(color-highlight
         `(H2  "For Experienced Programmers"))
       (UL  (LI  (B  (A ((HREF "/doc/t-y-scheme/index.htm"))
                        "Teach Yourself Scheme in Fixnum Days")) 
                 ": For programmers with lots of experience in other languages"))
       ,(color-highlight `(H2  "For Teachers and Researchers"))
       (UL  (LI  (B  (A ((HREF "/servlets/research/why.ss")) "Why DrScheme?"))
                 ": PLT's vision "))))))