(module info setup/infotab
  (define name "Web Server")  
  (define scribblings '(("docs/reference/web-reference.scrbl" (multi-page main-doc))
                        ("docs/guide/web-guide.scrbl" (multi-page main-doc))))
  
  (define mzscheme-launcher-libraries
    (list "private/launch-text.ss"))
  (define mzscheme-launcher-names
    (list "PLT Web Server Text")) 

  (define mred-launcher-libraries
    (list "private/launch-gui.ss"))
  (define mred-launcher-names
    (list "PLT Web Server")))
