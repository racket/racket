(module info (lib "infotab.ss" "setup")
  (define name "Web Server")  
  ; XXX Uncomment and change doc-installer
  #;(define post-install-collection "docs/doc-installer.ss")
  
  (define mzscheme-launcher-libraries
    (list "private/launch-text.ss"))
  (define mzscheme-launcher-names
    (list "PLT Web Server Text")) 

  (define mred-launcher-libraries
    (list "private/launch-gui.ss"))
  (define mred-launcher-names
    (list "PLT Web Server")))
