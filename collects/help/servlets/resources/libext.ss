(module libext mzscheme
  (require "../private/headelts.ss"
           "../private/util.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Libraries"))
      (BODY 
       (H1  "Libraries")
       (A ((NAME "libraries") (VALUE "extensions")))  
       (A ((NAME "mrspidey") (VALUE "mrspidey")))
       (A ((NAME "static debugger") (VALUE "static debugger")))
       (A ((NAME "mysterx") (VALUE "mysterx")))
       (A ((NAME "mzcom") (VALUE "mzcom")))
       (A ((NAME "COM") (VALUE "COM")))
       (A ((NAME "srpersist") (VALUE "srpersist")))
       (A ((NAME "ODBC") (VALUE "ODBC")))
       (A ((NAME "databases") (VALUE "databases")))
       "Many libraries and extensions are available for PLT software. "
       "See the " 
       (A ((HREF "http://www.cs.utah.edu/plt/develop/")
           (TARGET "_top")) "PLT libraries and extensions")
       " page for a comprehensive listing." 
       (P)
       "If you write a PLT library or extension, we would like to "
       "hear about it!  Please send a message about it to "
       "Matthew Flatt at " 
       (TT "mflatt@cs.utah.edu") " so we can list it. "
       "Thanks for your efforts!"))))