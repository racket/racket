(module info setup/infotab
  (define name "Web Server")  
  (define scribblings '(("docs/reference/web-reference.scrbl" (multi-page main-doc))
                        ("docs/guide/web-guide.scrbl" (multi-page main-doc))))
  
  (define mzscheme-launcher-libraries
    (list "private/main.ss"))
  (define mzscheme-launcher-names
    (list "PLT Web Server")))