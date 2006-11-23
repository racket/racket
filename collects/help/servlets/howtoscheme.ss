(module howtoscheme mzscheme
  (require "../private/manuals.ss"
           "private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "Software"))
      (body
       (h1  "Software")
       (ul (li (b (a ([href "howtodrscheme.ss"]) "DrScheme"))
               ": The programming environment")
           (li (b (a ([href "/servlets/scheme/what.ss"]) "Languages"))
               ": The family of languages supported by PLT Software")
           (li (b (a ([href "/servlets/scheme/how.ss"])
                     "Software & Components"))
               ": The full suite of PLT tools "
               (br) nbsp nbsp nbsp nbsp
               (font ([size "-2"])
                 (a ([href "/servlets/scheme/how.ss#installed-components"])
                    "Installed Components")
                 ", ..."))
           (li (b (a ([href "/servlets/scheme/doc.ss"]) "Documentation"))
               ": Organization and manuals       "
               (br) nbsp nbsp nbsp nbsp
               (font ([size "-2"])
                 (a ([href "/servlets/manuals.ss"]) "Manuals") ", ...") )
           (li (b (a ([href "scheme/misc.ss"]) "Hints"))
               ": How to do things in Scheme " )
           (li (b ,(manual-entry "drscheme" "frequently asked questions" "FAQ"))
               ": Frequently asked questions")
           (li (b (a ([href "releaseinfo.ss"]) "Release Information"))
               ": License, notes, and known bugs"))))))
