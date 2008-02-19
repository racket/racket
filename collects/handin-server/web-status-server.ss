#lang scheme/base
(require scheme/unit
         net/ssl-tcp-unit
         net/tcp-sig
         net/tcp-unit
         (only-in mzlib/etc this-expression-source-directory)
         web-server/web-server-unit
         web-server/web-server-sig
         web-server/web-config-sig
         web-server/web-config-unit
         web-server/configuration/namespace
         "private/config.ss")

(provide serve-status)

(define (serve-status port-no)

  (define web-dir
    (path->string
     (or (get-conf 'web-base-dir)
         (build-path (this-expression-source-directory) "status-web-root"))))

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
          (protocol-message "protocol-error.html")
          (collect-garbage "collect-garbage.html"))
         (timeouts
          (default-servlet-timeout 120)
          (password-connection-timeout 300)
          (servlet-connection-timeout 86400)
          (file-per-byte-connection-timeout 1/20)
          (file-base-connection-timeout 30))
         (paths
          (configuration-root "conf")
          (host-root ,web-dir)
          (log-file-path ,(cond [(get-conf 'web-log-file) => path->string]
                                [else #f]))
          (file-root "htdocs")
          (servlet-root ,web-dir)
          (mime-types ,(path->string
                        (build-path (collection-path "web-server")
                                    "default-web-root"
                                    "mime.types")))
          (password-authentication "unused"))))
      (virtual-host-table)))

  (define configuration
    (configuration-table-sexpr->web-config@
     config
     #:web-server-root web-dir
     #:make-servlet-namespace
     (make-make-servlet-namespace
      #:to-be-copied-module-specs
      '(handin-server/private/md5
        handin-server/private/logger
        handin-server/private/config
        handin-server/private/hooker
        handin-server/private/reloadable))))

  (define-unit-binding config@ configuration (import) (export web-config^))
  (define-unit-binding ssl-tcp@
    (make-ssl-tcp@ "server-cert.pem" "private-key.pem" #f #f #f #f #f)
    (import) (export tcp^))
  (define-compound-unit/infer status-server@
    (import)
    (link ssl-tcp@ config@ web-server@)
    (export web-server^))
  (define-values/invoke-unit/infer status-server@)

  (serve))
