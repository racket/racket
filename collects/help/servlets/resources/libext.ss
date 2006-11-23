(module libext mzscheme
  (require "../private/headelts.ss"
           "../private/util.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "Libraries"))
      (body
       (h1  "Libraries")
       (a ([name "libraries"] [value "extensions"]))
       (a ([name "mrspidey"] [value "mrspidey"]))
       (a ([name "static debugger"] [value "static debugger"]))
       (a ([name "mysterx"] [value "mysterx"]))
       (a ([name "mzcom"] [value "mzcom"]))
       (a ([name "COM"] [value "COM"]))
       (a ([name "srpersist"] [value "srpersist"]))
       (a ([name "ODBC"] [value "ODBC"]))
       (a ([name "databases"] [value "databases"]))
       "Many libraries and extensions are available for PLT software. "
       "See the "
       (a ([href "http://www.cs.utah.edu/plt/develop/"]
           [target "_top"])
          "PLT libraries and extensions")
       " page for a comprehensive listing."
       (p)
       "If you write a PLT library or extension, we would like to hear about"
       " it!  Please send a message about it to Matthew Flatt at "
       (TT "mflatt@cs.utah.edu") " so we can list it. "
       "Thanks for your efforts!"))))
