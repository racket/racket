(module config mzscheme
  (require (lib "file.ss")
           (lib "configuration.ss" "web-server")
           "internal-hp.ss")
  
  (provide config)
  
  (define config
    (let* ([build-normal-path
            (lambda args
              (normalize-path
               (apply build-path args)))]
           [help-path (build-normal-path (collection-path "help"))]
           [doc-path (build-normal-path help-path 'up "doc")]
           [host-root (build-normal-path help-path "web-root")]
           [servlet-root help-path]
           [make-host-config
            (λ (file-root)
              `(host-table
                (default-indices "index.html" "index.htm")
                (log-format parenthesized-default)
                (messages
                 (servlet-message "servlet-error.html")
                 (authentication-message "forbidden.html")
                 (servlets-refreshed "servlet-refresh.html")
                 (passwords-refreshed "passwords-refresh.html")
                 (file-not-found-message "not-found.html")
                 (protocol-message "protocol-error.html"))
                (timeouts
                 (default-servlet-timeout 120)
                 (password-connection-timeout 300)
                 (servlet-connection-timeout 86400)
                 (file-per-byte-connection-timeout 1/20)
                 (file-base-connection-timeout 30))
                (paths
                 (configuration-root "conf")
                 (host-root ,host-root)
                 (log-file-path "log")
                 (file-root ,file-root)
                 (servlet-root ,servlet-root)
                 (password-authentication "passwords"))))])
      (build-developer-configuration/vhosts
       `((port ,internal-port)
         (max-waiting 40)
         (initial-connection-timeout 30)
         (default-host-table
           ,(make-host-config (build-normal-path doc-path 'up)))
         (virtual-host-table
          (,addon-host 
           ,(make-host-config
             (build-path (find-system-path 'addon-dir))))))))))
