(module bugs mzscheme
  (require (lib "string.ss"))
  
  (require "../private/util.ss")
  (require "../private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (define stupid-internal-define-syntax (report-errors-to-browser send/finish))
    
    `(HTML
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Known Bugs"))
      (BODY
       (H1 "Known Bugs in PLT Scheme")
       (A ((NAME "bugs") (VALUE "Bugs")))
       "For an up-to-date list of bug reports, see the "
       (A ((HREF "http://bugs.plt-scheme.org/query/")
           (TARGET "_top")) "PLT bug report query page")) ".")))