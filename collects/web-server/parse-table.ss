(module parse-table mzscheme
  (require (lib "list.ss"))
  (require "configuration-table-structs.ss"
           "bindings.ss")
  (provide parse-configuration-table)
  
  (define (get-binding key bindings default)
    (first (get-binding* key bindings (list default))))
  
  (define (get-binding* key bindings default)
    (with-handlers ([exn? (lambda _ default)])
      (extract-binding/single key bindings)))
  
  ; parse-configuration-table : tst -> configuration-table
  (define (parse-configuration-table t)
    (define port (get-binding 'port t 80))
    (define max-waiting (get-binding 'max-waiting t 40))
    (define initial-connection-timeout (get-binding 'initial-connection-timeout t 30))
    (define default-host-table (get-binding* 'default-host-table t `()))
    (define virtual-host-table (get-binding* 'virtual-host-table t `()))
    (if (and (nat? port) (nat? max-waiting) (number? initial-connection-timeout)
             ; more here - list? isn't really picky enough
             (list? virtual-host-table))
        (make-configuration-table
         port max-waiting initial-connection-timeout
         (parse-host default-host-table)
         (map (lambda (h)
                (if (and (pair? h) (pair? (cdr h)) (null? (cddr h)))
                    (cons (car h) (parse-host (cdr h)))
                    (error 'parse-configuration-table "invalid virtual-host entry ~s" h)))
              virtual-host-table))
        (error 'parse-configuration-table "invalid configuration values ~s"
               (list port max-waiting initial-connection-timeout default-host-table virtual-host-table))))
  
  ; parse-host : tst -> host-table
  (define (parse-host t)
    (define host-table (get-binding* 'host-table t `()))
    (define default-indices (get-binding* 'default-indices host-table `("index.html" "index.htm")))
    (define log-format (get-binding 'log-format host-table 'parenthesized-default))
    (define messages (get-binding* 'messages host-table `()))
    (define servlet-message (get-binding 'servlet-message messages "servlet-error.html"))
    (define authentication-message (get-binding 'authentication-message messages "forbidden.html"))
    (define servlets-refreshed (get-binding 'servlets-refreshed messages "servlet-refresh.html"))
    (define passwords-refreshed (get-binding 'passwords-refreshed messages "passwords-refresh.html"))
    (define file-not-found-message (get-binding 'file-not-found-message messages "not-found.html"))
    (define protocol-message (get-binding 'protocol-message messages "protocol-error.html"))
    (define collect-garbage (get-binding 'collect-garbage messages "collect-garbage.html"))
    (define timeouts (get-binding* 'timeouts host-table `()))
    (define default-servlet-timeout (get-binding 'default-servlet-timeout timeouts 30))
    (define password-connection-timeout (get-binding 'password-connection-timeout timeouts 300))
    (define servlet-connection-timeout (get-binding 'servlet-connection-timeout timeouts (* 60 60 24)))
    (define file-per-byte-connection-timeout (get-binding 'file-per-byte-connection-timeout timeouts 1/20))
    (define file-base-connection-timeout (get-binding 'file-base-connection-timeout timeouts 30))
    (define paths (get-binding* 'paths host-table `()))
    (define configuration-root (get-binding 'configuration-root paths "conf"))
    (define host-root (get-binding 'host-root paths "default-web-root"))
    (define log-file-path (get-binding 'log-file-path paths "log"))
    (define file-root (get-binding 'file-root paths "htdocs"))
    (define servlet-root (get-binding 'servlet-root paths "."))
    (define mime-types (get-binding 'mime-types paths "mime.types"))
    (define password-authentication (get-binding 'password-authentication paths "passwords"))
    (make-host-table
     default-indices log-format
     (make-messages servlet-message
                    authentication-message
                    servlets-refreshed
                    passwords-refreshed
                    file-not-found-message
                    protocol-message
                    collect-garbage)
     (make-timeouts default-servlet-timeout
                    password-connection-timeout
                    servlet-connection-timeout
                    file-per-byte-connection-timeout
                    file-base-connection-timeout)
     (make-paths configuration-root
                 host-root
                 log-file-path
                 file-root
                 servlet-root
                 mime-types
                 password-authentication)))
  
  ; nat? : tst -> bool
  (define (nat? x)
    (and (number? x) (exact? x) (integer? x) (<= 0 x))))
