(module database mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "../../private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html 
         (head ,hd-css
               ,@hd-links
               (title "How to connect to databases"))
         (body 
          (h1  "How to connect to databases") 
          (a ([name "db"] [value "Database connections"]))
          "SrPersist (\"Sister Persist\") is an ODBC interface for "
          "DrScheme and MzScheme. "
          "Download SrPersist from "
          (pre
           " "
           (a ([href "http://www.plt-scheme.org/software/srpersist/"]
               [target "_top"])
              "http://www.plt-scheme.org/software/srpersist/") ". ")
          "ODBC is a very low-level interface. "
          "Francisco Solsona has built a higher-level interface, "
          "SchemeQL, that uses SrPersist.  See "
          (pre
           " "
           (a ((href "http://schematics.sourceforge.net/schemeql.html")
               (target "_top"))
              "http://schematics.sourceforge.net/schemeql.html"))
          " for more details."))))))
