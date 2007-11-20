(module options mzscheme

  ;; This module provides configuration options that are shared
  ;; between servlets and the web-server.  (Mostly to allow
  ;; configuration as an application or as a standalone server.)

  (provide helpdesk-platform internal-port)

  ;; internal browser or external browser?
  ;;   (used to produce simpler html for the internal browser)
  (define helpdesk-platform
    (make-parameter
     'internal-browser-simple  ; main page only
     ;; 'internal-browser      ; menu + main page
     ;; 'external-browser
     ))

  ;; Port for the server to listen on
  ;;   (relevant only for a standalone server)
  (define internal-port (make-parameter 8012))

  )
