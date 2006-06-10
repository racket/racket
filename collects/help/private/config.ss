(module config mzscheme
  (require (lib "file.ss")
           (lib "configuration.ss" "web-server")
	   (lib "dirs.ss" "setup")
           (lib "config.ss" "planet")
           "internal-hp.ss")
  
  (provide config)
  
  (define config
    (let* ([build-normal-path
            (lambda args
              (normalize-path
               (apply build-path args)))]
           [help-path (build-normal-path (collection-path "help"))]
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
                 (protocol-message "protocol-error.html")
		 (collect-garbage "collect-garbage.html"))
                (timeouts
                 (default-servlet-timeout 12000)
                 (password-connection-timeout 3000)
                 (servlet-connection-timeout 864000)
                 (file-per-byte-connection-timeout 10)
                 (file-base-connection-timeout 30000))
                (paths
                 (configuration-root "conf")
                 (host-root ,host-root)
                 (log-file-path #f)
                 (file-root ,file-root)
                 (servlet-root ,servlet-root)
                 (mime-types "../../web-server/default-web-root/mime.types")
                 (password-authentication "passwords"))))])
      (build-developer-configuration/vhosts
       `((port ,internal-port)
         (max-waiting 40)
         (initial-connection-timeout 30)
         (default-host-table
           ,(make-host-config (find-collects-dir)))
	 (virtual-host-table
	  ,@(map
	     (lambda (virtual-host dir)
	       `(,virtual-host 
		 ,(make-host-config dir)))
	     (cons planet-host (append doc-hosts collects-hosts))
	     (cons (PLANET-DIR) (append doc-dirs collects-dirs)))))))))
