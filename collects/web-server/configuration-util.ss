(module configuration-util mzscheme
  (require (lib "file.ss")           
           (lib "pretty.ss"))
  (require "configuration-table-structs.ss")
  (provide (all-defined))
  
  ; write-configuration-table : configuration-table path -> void
  ; writes out the new configuration file
  (define (write-configuration-table new configuration-path)
    (write-to-file
     configuration-path
     `((port ,(configuration-table-port new))
       (max-waiting ,(configuration-table-max-waiting new))
       (initial-connection-timeout ,(configuration-table-initial-connection-timeout new))
       (default-host-table
         ,(format-host (configuration-table-default-host new)))
       (virtual-host-table
        . ,(map (lambda (h) (list (car h) (format-host (cdr h))))
                (configuration-table-virtual-hosts new))))))
  
  ; format-host : host-table
  (define (format-host host)
    (let ([t (host-table-timeouts host)]
          [p (host-table-paths host)]
          [m (host-table-messages host)])
      `(host-table
        ; more here - configure
        (default-indices "index.html" "index.htm")
        ; more here - configure
        (log-format parenthesized-default)
        (messages
         (servlet-message ,(messages-servlet m))
         (authentication-message ,(messages-authentication m))
         (servlets-refreshed ,(messages-servlets-refreshed m))
         (passwords-refreshed ,(messages-passwords-refreshed m))
         (file-not-found-message ,(messages-file-not-found m))
         (protocol-message ,(messages-protocol m))
         (collect-garbage ,(messages-collect-garbage m)))
        (timeouts
         (default-servlet-timeout ,(timeouts-default-servlet t))
         (password-connection-timeout ,(timeouts-password t))
         (servlet-connection-timeout ,(timeouts-servlet-connection t))
         (file-per-byte-connection-timeout ,(timeouts-file-per-byte t))
         (file-base-connection-timeout ,(timeouts-file-base t)))
        (paths
         (configuration-root ,(paths-conf p))
         (host-root ,(paths-host-base p))
         (log-file-path ,(paths-log p))
         (file-root ,(paths-htdocs p))
         (servlet-root ,(paths-servlet p))
         (mime-types ,(paths-mime-types p))
         (password-authentication ,(paths-passwords p))))))
  
  ; write-to-file : str TST -> void
  (define (write-to-file file-name x)
    (call-with-output-file file-name
      (lambda (out) (pretty-print x out))
      'truncate)))