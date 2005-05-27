
(module web-status-server mzscheme
  (require (lib "unitsig.ss")
	   (lib "web-server-unit.ss" "web-server")
	   (lib "sig.ss" "web-server")
           (lib "configuration.ss" "web-server")
	   (lib "ssl-tcp-unit.ss" "net")
	   (lib "tcp-sig.ss" "net")
	   (lib "tcp-unit.ss" "net")
	   (lib "file.ss"))

  (provide serve-status)

  (define (serve-status port-no)
    
    (define config
      `((port ,port-no)
	(max-waiting 40)
	(initial-connection-timeout 30)
	(default-host-table
	  (host-table
	   (default-indices "index.html")
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
	    (host-root ,(path->string (build-path (collection-path "handin-server") "status-web-root")))
	    (log-file-path ,(path->string (build-path (current-directory) "web-status-log.ss")))
	    (file-root "htdocs")
	    (servlet-root ,(path->string (build-path (collection-path "handin-server") "status-web-root")))
	    (password-authentication ,(path->string (build-path (current-directory) "web-status-passwords"))))))
	(virtual-host-table)))

    (define config@
      (let ([file (make-temporary-file)])
	(with-output-to-file file
	  (lambda ()
	    (write config))
	  'truncate)
	(begin0
	 (load-configuration file)
	 (delete-file file))))

    (define-values/invoke-unit/sig web-server^
      (compound-unit/sig
       (import)
       (link
	[T : net:tcp^ ((make-ssl-tcp@
			"server-cert.pem" "private-key.pem" #f #f
			#f #f #f))]
	[C : web-config^ (config@)]
	[S : web-server^ (web-server@ T C)])
       (export (open S)))
      #f)

    (serve)))

