(module info (lib "infotab.ss" "setup")
  ;; Modify these four definitions to customize the tool.
  ;; Also replace the "icon.png" and "server-cert.pem" files.
  (define name "Course Handin")
  (define collection "handin-client")
  ;; (define server:port "localhost:7979")
  ;; --> server:port should be defined here, if it is not defined, not tool
  ;;     will appear.  To try things, you can set the PLT_HANDIN_SERVER_PORT
  ;;     environment variable when this is undefined.

  ;; The following are optional. Uncomment and fill in
  ;; the values to add a menu item under "Help" to open
  ;; the specified web page (using the user's chosen web
  ;; browser.)
  ;(define web-menu-name "Course Homepage")
  ;(define web-address "http://www.university.edu/course/")

  #;(define tool-icons (list (list "icon.png" collection)))
  #;(define tools '(("tool.ss")))
  #;(define tool-names (list name))
  (define requires '(("mred") ("openssl"))))
