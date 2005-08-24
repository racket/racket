(module info (lib "infotab.ss" "setup")
  (define name "Web Server")
  (define mzscheme-launcher-libraries (list "text-launch.ss" "monitor-launch.ss"))
  (define mzscheme-launcher-names (list "web-server-text" "web-server-monitor"))

  (define mred-launcher-libraries (list "gui-launch.ss"))
  (define mred-launcher-names (list "web-server")))
