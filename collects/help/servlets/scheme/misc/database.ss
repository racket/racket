(module database mzscheme
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
            (TITLE "How to connect to databases"))
      (BODY 
       (H1  "How to connect to databases") 
       (A ((NAME "db") (VALUE "Database connections")))
       "SrPersist (\"Sister Persist\") is an ODBC interface for "
       "DrScheme and MzScheme. "
       "Download SrPersist from "
       (PRE  
        " " 
        (A ((HREF "http://www.plt-scheme.org/software/srpersist/")
            (TARGET "_top")) "http://www.plt-scheme.org/software/srpersist/") ". ")
       "ODBC is a very low-level interface. "
       "Francisco Solsona has built a higher-level interface, "
       "SchemeQL, that uses SrPersist.  See "
       (PRE 
        " " 
        (A ((HREF "http://schematics.sourceforge.net/schemeql.html")
            (TARGET "_top")) "http://schematics.sourceforge.net/schemeql.html"))
       " for more details."))))