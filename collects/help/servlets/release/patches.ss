(module patches mzscheme
  (require "../private/headelts.ss"
           "../private/util.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
    `(html
      (head ,hd-css ,@hd-links (title "Downloadable Patches"))
      (body
       (h1 "Downloadable Patches")
       (a ([name "patches"] [value "Downloadable patches"]))
       "The following Web page may contain downloadable patches to fix "
       "serious bugs in version " ,(version) " of the PLT software:"
       (p)
       nbsp nbsp
       ,(let ([url (format "http://download.plt-scheme.org/patches/~a/"
                           (version))])
          `(a ([href ,url] [target "_top"]) ,url))))))))