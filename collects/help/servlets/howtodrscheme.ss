(module howtodrscheme mzscheme
  (require "private/headelts.ss"
           "../private/manuals.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html
         (head ,hd-css ,@hd-links (title "DrScheme"))
         (body
          (h1 "DrScheme")
          "DrScheme is PLT's flagship programming environment. "
          "See " (a ((href "/servlets/scheme/how.ss")) "Software & Components")
          " for a guide to the full suite of PLT tools."
          (ul (li (b  (a ([href ,(get-manual-index "tour")])) "Tour")
                  ": An introduction to DrScheme")
              (li (b  ,(manual-entry "drscheme"
                                     "graphical interface"
                                     "Interface Essentials"))
                  ": Quick-start jump into the user manual")
              (li  (b  (a ([href "/servlets/scheme/what.ss"])
                          "Languages"))
                   ": Languages supported by DrScheme")
              (li  (b  ,(main-manual-page "drscheme"))
                   ": The complete user manual"))))))))