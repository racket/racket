#lang racket/base
(require racket/contract
         racket/list
         (for-syntax racket/base)
         racket/pretty
         racket/runtime-path
         "configuration-table-structs.rkt"
         web-server/http/bindings)
(define configuration-table-sexpr? list?)

(provide/contract
 [configuration-table-sexpr? (any/c . -> . boolean?)]
 [read-configuration-table (path-string? . -> . configuration-table?)]
 [write-configuration-table (configuration-table? path-string? . -> . void)]
 [configuration-table->sexpr (configuration-table? . -> . configuration-table-sexpr?)]
 [sexpr->configuration-table (configuration-table-sexpr? . -> . configuration-table?)]
 [default-configuration-table-path path?])

(define-runtime-path default-configuration-table-path
  "../default-web-root/configuration-table.rkt")

(define (get-binding key bindings default)
  (first (get-binding* key bindings (list default))))

(define (get-binding* key bindings default)
  (with-handlers ([exn:fail? (lambda _ default)])
    (extract-binding/single key bindings)))

(define (read-configuration-table table-file-name)
  (sexpr->configuration-table (call-with-input-file table-file-name read)))

; parse-configuration-table : tst -> configuration-table
(define (sexpr->configuration-table t)
  (define port (get-binding 'port t 80))
  (define max-waiting (get-binding 'max-waiting t 511))
  (define initial-connection-timeout (get-binding 'initial-connection-timeout t 30))
  (define default-host-table (get-binding* 'default-host-table t `()))
  (define virtual-host-table (get-binding* 'virtual-host-table t `()))
  (make-configuration-table
   port max-waiting initial-connection-timeout
   (parse-host default-host-table)
   (map (lambda (h)
          (cons (car h) (parse-host (cdr h))))
        virtual-host-table)))

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

(define (configuration-table->sexpr new)
  `((port ,(configuration-table-port new))
    (max-waiting ,(configuration-table-max-waiting new))
    (initial-connection-timeout ,(configuration-table-initial-connection-timeout new))
    (default-host-table
      ,(host-table->sexpr (configuration-table-default-host new)))
    (virtual-host-table
     . ,(map (lambda (h) (list (car h) (host-table->sexpr (cdr h))))
             (configuration-table-virtual-hosts new)))))

; write-configuration-table : configuration-table path -> void
; writes out the new configuration file
(define (write-configuration-table new configuration-path)
  (define sexpr (configuration-table->sexpr new))
  (call-with-output-file configuration-path
    (lambda (out) (pretty-write sexpr out))
    #:exists 'truncate))

; host-table->sexpr : host-table
(define (host-table->sexpr host)
  (let ([t (host-table-timeouts host)]
        [p (host-table-paths host)]
        [m (host-table-messages host)])
    `(host-table
      (default-indices "index.html" "index.htm")
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
