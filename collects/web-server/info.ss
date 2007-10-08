(module info (lib "infotab.ss" "setup")
  (define name "Web Server")  
  ; Name clash
  #;(define scribblings '(("docs/reference/reference.scrbl" (multi-page main-doc))))
  
  (define mzscheme-launcher-libraries
    (list "private/launch-text.ss"))
  (define mzscheme-launcher-names
    (list "PLT Web Server Text")) 

  (define mred-launcher-libraries
    (list "private/launch-gui.ss"))
  (define mred-launcher-names
    (list "PLT Web Server")))
