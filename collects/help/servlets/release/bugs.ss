(module bugs mzscheme
  (require (lib "string.ss")
           "../private/util.ss"
           "../private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "Known Bugs"))
      (body
       (h1 "Known Bugs in PLT Scheme")
       (a ([name "bugs"] [value "Bugs"]))
       "For an up-to-date list of bug reports, see the "
       (a ([href "http://bugs.plt-scheme.org/query/"] [target "_top"])
          "PLT bug report query page")) ".")))
