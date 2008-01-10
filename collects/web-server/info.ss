(module info setup/infotab
  (define name "Web Server")
  (define scribblings '(("scribblings/web-server.scrbl" (multi-page main-doc))))

  (define mzscheme-launcher-libraries
    (list "main.ss"))
  (define mzscheme-launcher-names
    (list "PLT Web Server")))
