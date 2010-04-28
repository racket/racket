#lang racket/base
(require mzlib/unit
         racket/contract)
(require web-server/private/util
         web-server/private/cache-table
         web-server/configuration/configuration-table-structs
         web-server/configuration/configuration-table
         web-server/configuration/namespace
         web-server/configuration/responders
         web-server/web-config-sig)
(provide/contract
 [configuration-table->web-config@
  (->* (path-string?)
       (#:port (or/c false/c number?)
               #:listen-ip (or/c false/c string?)
               #:make-servlet-namespace make-servlet-namespace/c)
       (unit/c (import) (export web-config^)))]
 [configuration-table-sexpr->web-config@
  (->* (configuration-table-sexpr?)
       (#:web-server-root path-string?
                          #:port (or/c false/c number?)
                          #:listen-ip (or/c false/c string?)
                          #:make-servlet-namespace make-servlet-namespace/c)
       (unit/c (import) (export web-config^)))])

; configuration-table->web-config@ : path -> configuration
(define (configuration-table->web-config@ 
         table-file-name
         #:port [port #f]
         #:listen-ip [listen-ip #f]
         #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)])
  (configuration-table-sexpr->web-config@ 
   (call-with-input-file table-file-name read)
   #:web-server-root (directory-part table-file-name)
   #:port port
   #:listen-ip listen-ip
   #:make-servlet-namespace make-servlet-namespace))

; configuration-table-sexpr->web-config@ : string? sexp -> configuration
(define (configuration-table-sexpr->web-config@
         sexpr
         #:web-server-root [web-server-root (directory-part default-configuration-table-path)]         
         #:port [port #f]
         #:listen-ip [listen-ip #f]
         #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)])
  (complete-configuration
   web-server-root
   (sexpr->configuration-table sexpr)
   #:port port
   #:listen-ip listen-ip
   #:make-servlet-namespace make-servlet-namespace))

; : str configuration-table -> configuration
(define (complete-configuration 
         base table         
         #:port [port #f]
         #:listen-ip [listen-ip #f]
         #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)])
  (define default-host
    (apply-default-functions-to-host-table
     base (configuration-table-default-host table)))
  (define expanded-virtual-host-table
    (map (lambda (x)
           (list (regexp (string-append (car x) "(:[0-9]*)?"))
                 (apply-default-functions-to-host-table base (cdr x))))
         (configuration-table-virtual-hosts table)))
  (build-configuration
   table
   (gen-virtual-hosts expanded-virtual-host-table default-host)
   #:port port
   #:listen-ip listen-ip
   #:make-servlet-namespace make-servlet-namespace))

; : configuration-table host-table -> configuration
(define (build-configuration 
         table the-virtual-hosts
         #:port [port #f]
         #:listen-ip [listen-ip #f]
         #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)])
  (define the-port (or port (configuration-table-port table)))
  (define the-listen-ip (or listen-ip #f))
  (define the-make-servlet-namespace make-servlet-namespace)
  (unit
    (import)
    (export web-config^)
    (define port the-port)
    (define max-waiting (configuration-table-max-waiting table))
    (define listen-ip the-listen-ip)
    (define initial-connection-timeout (configuration-table-initial-connection-timeout table))
    (define virtual-hosts the-virtual-hosts)
    (define make-servlet-namespace the-make-servlet-namespace)))

; apply-default-functions-to-host-table : str host-table -> host
;; Greg P: web-server-root is the directory-part of the path to the configuration-table (I don't think I like this.)
(define (apply-default-functions-to-host-table web-server-root host-table)
  (let ([paths (expand-paths web-server-root (host-table-paths host-table))])
    (make-host
     (host-table-indices host-table)
     (host-table-log-format host-table) (paths-log paths)
     (paths-passwords paths)
     (let ([m (host-table-messages host-table)]
           [conf (paths-conf paths)])
       (make-responders
        servlet-error-responder
        servlet-loading-responder
        (gen-authentication-responder (build-path-unless-absolute conf (messages-authentication m)))
        (gen-servlets-refreshed (build-path-unless-absolute conf (messages-servlets-refreshed m)))
        (gen-passwords-refreshed (build-path-unless-absolute conf (messages-passwords-refreshed m)))
        (gen-file-not-found-responder (build-path-unless-absolute conf (messages-file-not-found m)))
        (gen-protocol-responder (build-path-unless-absolute conf (messages-protocol m)))
        (gen-collect-garbage-responder (build-path-unless-absolute conf (messages-collect-garbage m)))))
     (host-table-timeouts host-table)
     paths)))

; expand-paths : str paths -> paths
(define (expand-paths web-server-root paths)
  (let ([build-path-unless-absolute
         (lambda (b p) 
           (if p 
               (build-path-unless-absolute b p)
               #f))])
    (let* ([host-base (build-path-unless-absolute web-server-root (paths-host-base paths))]
           [htdocs-base (build-path-unless-absolute host-base (paths-htdocs paths))])
      (make-paths (build-path-unless-absolute host-base (paths-conf paths))
                  host-base
                  (build-path-unless-absolute host-base (paths-log paths))
                  htdocs-base
                  (build-path-unless-absolute htdocs-base (paths-servlet paths))
                  (build-path-unless-absolute host-base (paths-mime-types paths))                    
                  (build-path-unless-absolute host-base (paths-passwords paths))))))

; gen-virtual-hosts : (listof (list regexp host)) host ->
; str -> host-configuration
(define (gen-virtual-hosts expanded-virtual-host-table default-host)
  (lambda (host-name-possibly-followed-by-a-collon-and-a-port-number)
    (or (ormap (lambda (x)
                 (and (regexp-match (car x) host-name-possibly-followed-by-a-collon-and-a-port-number)
                      (cadr x)))
               expanded-virtual-host-table)
        default-host)))
