(module launch mzscheme
  (provide launch
           shutdown)
  
  ;;; launch.ss
  
  ; INSTRUCTIONS
  ;   This file launches a web-server serving a local
  ;   version of "Little Helper". Open this file in DrScheme
  ;   and hit "Run". Then open the printed link in a
  ;   web-browser near you.
  
  ; NOTES
  ;   You can change the port number used, but 
  ;   editing "servlets/config.scm".
  
  (require (lib "web-server.ss" "web-server")
           (lib "web-config-unit.ss" "web-server")
           (lib "config.ss" "planet")
           (lib "etc.ss")
           "web-root/servlets/private/config.scm")
  
  ;;; WEB-SERVER CONFIGURATION
  
  (define config-sexp
    `((port ,port)
      (max-waiting 40)
      (initial-connection-timeout 30)
      (default-host-table
        (host-table
         (default-indices "index.html" "index.htm")
         (log-format parenthesized-default)
         (messages
          (servlet-message "servlet-error.html")
          (authentication-message "forbidden.html")
          (servlets-refreshed "servlet-refresh.html")
          (passwords-refreshed "passwords-refresh.html")
          (file-not-found-message "not-found.html")
          (protocol-message "protocol-error.html")
          (collect-garbage "collect-garbage.html"))
         (timeouts
          (default-servlet-timeout 30)
          (password-connection-timeout 300)
          (servlet-connection-timeout 86400)
          (file-per-byte-connection-timeout 1/20)
          (file-base-connection-timeout 30))
         (paths
          (configuration-root "conf")
          (host-root     ,(build-path (this-expression-source-directory) "web-root"))
          (file-root     "htdocs")
          (servlet-root  ".")
          (mime-types "mime.types")
          (password-authentication "passwords"))))
      (virtual-host-table)))
  
  
  ; start the web-server, and store a shutdown function
  (define shutdown 
    (λ () (error "Little Helper isn't running"))
    #;(serve/web-config@ (configuration-table-sexpr->web-config@ config-sexp)))
  
  (define (launch)
    (let ([stop (serve/web-config@ (configuration-table-sexpr->web-config@ config-sexp))])
      (set! shutdown
            (λ ()
              (stop)
              (set! shutdown (λ () (error "Little Helper isn't running")))))
      (display (format "Start here: http://localhost:~a/servlets/view.scm\n\n" port))
      (display "Call (shutdown) to stop Little Helper.\n")))
  
  (display "Evaluate  (launch)   to start Little Helper.\n")
  )
