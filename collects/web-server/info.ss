(module info (lib "infotab.ss" "setup")
  (define name "Web Server")

  (define compile-subcollections 
    (list))
  
  (define mzscheme-launcher-libraries
    (list "private/launch-text.ss" "private/setup-launch.ss" ))
  (define mzscheme-launcher-names
    (list "PLT Web Server Text" "PLT Web Server Setup")) 

  (define mred-launcher-libraries
    (list "private/launch-gui.ss"))
  (define mred-launcher-names
    (list "PLT Web Server")))
