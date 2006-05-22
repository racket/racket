(module info (lib "infotab.ss" "setup")
  (define name "Web Server")
  (define mzscheme-launcher-libraries
    (list "text-launch.ss" "monitor-launch.ss" "setup-launch.ss" ))
  (define mzscheme-launcher-names
    (list "PLT Web Server Text" "PLT Web Server Monitor" "PLT Web Server Setup"))

  (define mred-launcher-libraries (list "gui-launch.ss"))
  (define mred-launcher-names (list "PLT Web Server")))
