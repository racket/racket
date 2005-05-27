(module script mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "../../private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "How to write Unix shell scripts"))  
      (BODY 
       (H1 "How to write Unix shell scripts")  
       (A ((NAME "sh") (VALUE "Shell scripts")))
       "When MzScheme is installed as part of the standard Unix "
       "PLT distribution, " 
       (TT "plt/bin/mzscheme") " and  " 
       (TT "plt/bin/mred") " are binary executables." 
       (P) 
       "Thus, they can be used with Unix's " (TT  "#!") 
       " convention as follows:"
       (PRE  
        "  #! /usr/local/lib/plt/bin/mzscheme -r  ... " (BR) 
        " " (I  "scheme-program") " ...")
       "assuming that the " (TT  "plt") " tree is installed as " 
       (TT "/usr/local/lib/plt") ". "
       "To avoid specifying an absolute path, use " 
       (TT "/usr/bin/env") ":"
       (PRE  
        " #! /usr/bin/env mzscheme -r  ... " (BR)
        " " (I  "scheme-program") " ...")
       (P)
       "The above works when " 
       (TT  "mzscheme") 
       " is in the user's path. "
       "The " (TT "mred") " executable can be used in the "
       "same way for GUI scripts."
       (P)
       "Within " (I "scheme-program") ", " 
       (TT  "(current-command-line-arguments)") 
       " produces a vector of strings for the arguments "
       "passed to the script. The vector is also available as " 
       (TT "argv") "."))))