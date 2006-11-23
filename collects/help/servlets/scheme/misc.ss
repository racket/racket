(module misc mzscheme
  (require (lib "servlet.ss" "web-server")
           "../private/headelts.ss"
           "../private/util.ss")
  ;; (listof string string) -> xexpr
  (define (make-link-line url/txt)
    (let ([url (car url/txt)]
          [txt (cadr url/txt)])
      `(li (b (a ([href ,(string-append "/servlets/scheme/misc/" url)])
                 ,txt)))))

  (define links
    '(("standalone.ss" "How to build a stand-alone executable")
      ("graphics.ss"   "How to write graphics programs")
      ("script.ss"     "How to write Unix shell scripts")
      ("batch.ss"      "How to write Windows batch files")
      ("cgi.ss"        "How to write CGI scripts")
      ("activex.ss"    "How to use ActiveX components")
      ("database.ss"   "How to connect to databases")
      ("system.ss"     "How to call low-level system routines")))

  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html (head ,hd-css ,@hd-links (TITLE "How to do things in Scheme"))
           (body
            (h1 "How to do things in Scheme")
            (ul ,@(map make-link-line links))
            (p)
            "If you did't find what you're looking for in the list above, try "
            (a ((href "/servlets/howtouse.ss#search")) "searching")
            " in Help Desk.  Also, check "
            (a ((href "http://www.htus.org/")) (i "How to Use Scheme"))
            "."))))
