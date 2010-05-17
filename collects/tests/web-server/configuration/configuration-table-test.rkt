#lang racket/base
(require rackunit
         (only-in mzlib/file
                  make-temporary-file)
         web-server/configuration/configuration-table)
(provide configuration-table-tests)

(define configuration-table-tests
  (test-suite
   "Configuration Table"
   
   (test-case
    "Default configuration file may be parsed"
    (check-not-false (read-configuration-table default-configuration-table-path)))
   
   (test-case
    "Default configuration file may be written"
    (check-not-false (write-configuration-table 
                      (read-configuration-table default-configuration-table-path)
                      (make-temporary-file))))
   
   (test-case
    "False allowed in configuration file for passwords"
    (let ([false-sexpr
           `((port 80)
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
                 (host-root ".")
                 (log-file-path "log")
                 (file-root "htdocs")
                 (servlet-root ".")
                 (mime-types "mime.types")
                 (password-authentication #f))))
             (virtual-host-table))])
      (check-equal? 
       (configuration-table->sexpr
        (sexpr->configuration-table
         false-sexpr))        
       false-sexpr)))
   
   
   (test-case
    "Default configuration file may be converted to sexpr and back"
    (check-not-false
     (sexpr->configuration-table
      (configuration-table->sexpr
       (read-configuration-table
        default-configuration-table-path)))))))
