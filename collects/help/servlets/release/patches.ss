(module patches mzscheme
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
            (TITLE "Downloadable Patches"))
      (H1 "Downloadable Patches")
      (A ((NAME="patches") (VALUE "Downloadable patches")))
      "The following Web page may contain downloadable patches to fix serious bugs in "
      "version " ,(version) " of the PLT software:"
      (P)
      'nbsp 'nbsp
      ,(let ([url (format "http://download.plt-scheme.org/patches/~a/" (version))])
         `(A ((HREF ,url)
              (TARGET "_top")) ,url)))))