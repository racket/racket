(module hardcoded-configuration mzscheme
  (require (lib "configuration-structures.ss" "web-server" "private")
           (lib "util.ss" "web-server" "private")
           (lib "response.ss" "web-server"))
  
  (provide config:port
           config:max-waiting
           config:listen-ip
           config:initial-connection-timeout
           config:virtual-hosts
           hardcoded-host)
  
  ;; ************************************************************
  ;; HARDCODED CONFIGURATION STUFF
  
  (define config:port 8000)
  (define config:max-waiting 20)
  (define config:listen-ip #f)
  (define config:initial-connection-timeout 30)
  
  ;; ************************************************************
  ;; HARDCODED HOST
  
  ; error-response : nat str str [(cons sym str) ...] -> response
  ; more here - cache files with a refresh option.
  ; The server should still start without the files there, so the
  ; configuration tool still runs.  (Alternatively, find an work around.)
  (define (error-response code short text-file . extra-headers)
    (make-response/full code short
                        (current-seconds) TEXT/HTML-MIME-TYPE
                        extra-headers
                        (list (read-file text-file))))
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
  ;; error files:
  (define server-root-path (build-path "~" "Development" "plt" "default-web-root"))
  (define default-host-path (build-path server-root-path "conf"))
  
  (define servlet-error-file (build-path default-host-path "servlet-error.html"))
  (define access-denied-file (build-path default-host-path "forbidden.html"))
  (define servlet-refresh-file (build-path default-host-path "servlet-refresh.html"))
  (define password-refresh-file (build-path default-host-path "passwords-refresh.html"))
  (define file-not-found-file (build-path default-host-path "not-found.html"))
  (define protocol-file (build-path default-host-path "protocol-error.html"))
  (define collect-garbage-file (build-path default-host-path "collect-garbage.html"))
  
  (define hardcoded-host
    ; host = (make-host (listof str) (str str sym url str -> str)
    ;                   passwords resopnders timeouts paths)
    (make-host
     
     ;; indices
     (list "index.html" "index.htm")
     
     ;; log-format
     'none
     
     ;; log-message
     "log"
     
     ;; passwords
     "passwords"
     
     (make-responders
      
      ;; servlet: url tst -> response 
      (lambda (url exn)
        ; more here - use separate log file
        ;(printf "Servlet exception:\n~s\n" (exn-message exn))
        ((error-display-handler)
         (format "Servlet exception:\n~a\n" (exn-message exn))
         exn)
        (error-response 500 "Servlet error" servlet-error-file))
      
      ;; servlet-loading: url tst -> response
      ; more here - parameterize error based on a configurable file, perhaps?
      ; This is slightly tricky since the (interesting) content comes from the exception.
      (lambda (url exn)
        ((error-display-handler)
         (format "Servlet loading exception:\n~a\n" (exn-message exn))
         exn)
        (make-response/full 500 "Servlet didn't load"
                            (current-seconds)
                            #"text/plain" ;TEXT/HTML-MIME-TYPE
                            '() ; check
                            (list "Servlet didn't load.\n"
                                  (exn->string exn))))
      
      ;; authentication: url (cons sym str) -> response
      (lambda (uri recommended-header)
        (error-response 401 "Authorization Required" access-denied-file
                        recommended-header))
      
      ;; servlets-refreshed: -> response
      (lambda ()
        (error-response 200 "Servlet cache refreshed" servlet-refresh-file))
      
      ;; passwords-refreshed: -> response
      (lambda ()
        (error-response 200 "Passwords refreshed" password-refresh-file))
      
      ;; file-not-found: url->response
      (lambda (url)
        (error-response 404 "File not found" file-not-found-file))
      
      ;; protocol: string -> response
      (lambda (error-message)
        (error-response 400 "Malformed Request" protocol-file))
      
      ;; collect-garbage: -> response
      (lambda ()
        (error-response 200 "Collected Garbage" collect-garbage-file))
      
      )
     
     ; timeouts = (make-timeouts nat^5)
     (make-timeouts
      ; default-servlet-timeout
      60
      ;password-connection-timeout
      300
      ; servlet-connection-timeout
      86400
      ; file-per-byte-connection-timeout
      1/20
      ; file-base-connection-timeout
      30)
     
     ; paths = (make-paths str^6)
     (make-paths
      ; configuration-root
      (build-path server-root-path "conf")
      ; host-root
      (build-path server-root-path "default-web-root")
      ; log-file-path
      "log"
      ; file-root
      (build-path server-root-path "htdocs")
      ; servlet-root
      (build-path "~" "Development" "Projects" "exp" "prototype-web-server")
      ; mime-types
      (build-path server-root-path "mime.types")             
      ; password-authentication
      (build-path server-root-path "passwords"))))
  
  ;; config:virtual-hosts: alpha -> host
  ;; return a default host structure
  (define config:virtual-hosts
    (lambda (ignore)
      hardcoded-host)))