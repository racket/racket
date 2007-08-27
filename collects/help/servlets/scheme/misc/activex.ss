(module activex mzscheme
  (require "../../private/util.ss")
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
            (title "How to use ActiveX components"))
      (body
       (h1  "How to use ActiveX components")
       (a ([name "com"] [value "COM"]))
       (a ([name "activex"] [value "ActiveX"]))
       "If you run Windows, you can use MysterX, a library for "
       "controlling COM and ActiveX components within DrScheme, "
       "MzScheme, or MrEd.  MysterX is available from "
       (pre
        nbsp nbsp
        (a ((href "http://www.plt-scheme.org/software/mysterx/")
            (target "_top"))
           "http://www.plt-scheme.org/software/mysterx/"))
       (p)
       ,(collection-doc-link "mysterx" "The MysterX collection")))))))
