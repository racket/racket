(module web-config-unit mzscheme
  (require (lib "unit.ss")
           (lib "kw.ss"))
  (require "private/util.ss"
           "private/cache-table.ss"
           "configuration/configuration-table-structs.ss"
           "configuration/configuration-table.ss"
           "configuration/namespace.ss"
           "configuration/responders.ss"
           "web-config-sig.ss")
  (provide configuration-table->web-config@
           configuration-table-sexpr->web-config@)
  
  ; configuration-table->web-config@ : path -> configuration
  (define/kw (configuration-table->web-config@ table-file-name
                                               #:other-keys bct-keys)
    (apply configuration-table-sexpr->web-config@ 
           (call-with-input-file table-file-name read)
           #:web-server-root (directory-part table-file-name)
           bct-keys))
  
  ; configuration-table-sexpr->web-config@ : string? sexp -> configuration
  (define/kw (configuration-table-sexpr->web-config@ sexpr
                                                     #:key
                                                     [web-server-root (directory-part default-configuration-table-path)]
                                                     #:other-keys bct-keys)
    (apply complete-configuration
           web-server-root
           (sexpr->configuration-table sexpr)
           bct-keys))
  
  ; : str configuration-table -> configuration
  (define/kw (complete-configuration base table
                                     #:other-keys bct-keys)
    (define default-host
      (apply-default-functions-to-host-table
       base (configuration-table-default-host table)))
    (define expanded-virtual-host-table
      (map (lambda (x)
             (list (regexp (string-append (car x) "(:[0-9]*)?"))
                   (apply-default-functions-to-host-table base (cdr x))))
           (configuration-table-virtual-hosts table)))
    (apply build-configuration
           table
           (gen-virtual-hosts expanded-virtual-host-table default-host)
           bct-keys))
  
  ; : configuration-table host-table -> configuration
  (define/kw (build-configuration table the-virtual-hosts
                                  #:key
                                  [port #f]
                                  [listen-ip #f]
                                  [make-servlet-namespace (make-make-servlet-namespace)])
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
      (define scripts (box (make-cache-table)))
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
          (gen-servlet-responder (build-path-unless-absolute conf (messages-servlet m)))
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
      (let ([host-base (build-path-unless-absolute web-server-root (paths-host-base paths))])
        (make-paths (build-path-unless-absolute host-base (paths-conf paths))
                    host-base
                    (build-path-unless-absolute host-base (paths-log paths))
                    (build-path-unless-absolute host-base (paths-htdocs paths))
                    (build-path-unless-absolute host-base (paths-servlet paths))
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
          default-host))))