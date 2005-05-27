(module parse-table mzscheme
  (require (lib "match.ss")
           "configuration-table-structs.ss")
  (provide parse-configuration-table)
    
  ; parse-configuration-table : tst -> configuration-table
  (define parse-configuration-table
    (match-lambda
     [`((port ,port)
        (max-waiting ,max-waiting)
        (initial-connection-timeout ,initial-connection-timeout)
        (default-host-table
         ,default-host-table)
        (virtual-host-table . ,virtual-host-table))
      (if (and (nat? port) (nat? max-waiting) (number? initial-connection-timeout)
               ; more here - list? isn't really picky enough
               (list? virtual-host-table))
          (make-configuration-table
           port max-waiting initial-connection-timeout
           (parse-host default-host-table)
           (map (lambda (h)
                  (if (and (pair? h) (pair? (cdr h)) (null? (cddr h)))
                      (cons (car h) (parse-host (cadr h)))
                      (error 'parse-configuration-table "invalid virtual-host entry ~s" h)))
                virtual-host-table))
          (error 'parse-configuration-table "invalid configuration values ~s"
                 (list port max-waiting initial-connection-timeout default-host-table virtual-host-table)))]
     [x (error 'parse-configuration-table "malformed configuration ~s" x)]))
  
  ; parse-host : tst -> host-table
  (define parse-host
    (match-lambda
     [`(host-table
        (default-indices . ,default-indices)
        (log-format ,log-format)
        (messages
         (servlet-message ,servlet-message)
         (authentication-message ,authentication-message)
         (servlets-refreshed ,servlets-refreshed)
         (passwords-refreshed ,passwords-refreshed)
         (file-not-found-message ,file-not-found-message)
         (protocol-message ,protocol-message))
        (timeouts
         (default-servlet-timeout ,default-servlet-timeout)
         (password-connection-timeout ,password-connection-timeout)
         (servlet-connection-timeout ,servlet-connection-timeout)
         (file-per-byte-connection-timeout ,file-per-byte-connection-timeout)
         (file-base-connection-timeout ,file-base-connection-timeout))
        (paths
         (configuration-root ,configuration-root)
         (host-root ,host-root)
         (log-file-path ,log-file-path)
         (file-root ,file-root)
         (servlet-root ,servlet-root)
         (password-authentication ,password-authentication)))
      (make-host-table
       default-indices log-format
       (make-messages servlet-message
                      authentication-message
                      servlets-refreshed
                      passwords-refreshed
                      file-not-found-message
                      protocol-message)
       (make-timeouts default-servlet-timeout
                      password-connection-timeout
                      servlet-connection-timeout
                      file-per-byte-connection-timeout
                      file-base-connection-timeout)
       (make-paths configuration-root host-root log-file-path file-root servlet-root password-authentication))]
     [x (error 'parse-host "malformed host ~s" x)]))
  
  ; nat? : tst -> bool
  (define (nat? x)
    (and (number? x) (exact? x) (integer? x) (<= 0 x))))
