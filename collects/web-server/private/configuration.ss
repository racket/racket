(module configuration mzscheme
  (require (lib "unit.ss")
           (lib "kw.ss")
           (lib "contract.ss"))
  (require "configuration-structures.ss"
           "configuration-table-structs.ss"
           "util.ss"
           "cache-table.ss"
           "../configuration/namespace.ss"
           "../configuration/responders.ss"
           "../web-config-sig.ss")
  
  ; : str configuration-table -> configuration
  (define (complete-configuration base table)
    (build-configuration
     table
     (let ([default-host
             (apply-default-functions-to-host-table
              base (configuration-table-default-host table))]
           [expanded-virtual-host-table
            (map (lambda (x)
                   (list (regexp (string-append (car x) "(:[0-9]*)?"))
                         (apply-default-functions-to-host-table base (cdr x))))
                 (configuration-table-virtual-hosts table))])
       (gen-virtual-hosts expanded-virtual-host-table default-host))))
  
  ; complete-developer-configuration : str configuration-table -> configuration
  (define (complete-developer-configuration base table)
    (build-configuration
     table
     (gen-virtual-hosts null (apply-default-functions-to-host-table
                              base
                              (configuration-table-default-host table)))))
  
  ; : configuration-table host-table -> configuration
  (define/kw (build-configuration table the-virtual-hosts
                                  #:key
                                  [make-servlet-namespace default-make-servlet-namespace])
    (define the-make-servlet-namespace make-servlet-namespace)
    (unit
      (import)
      (export web-config^)
      (define port (configuration-table-port table))
      (define max-waiting (configuration-table-max-waiting table))
      (define listen-ip #f) ; more here - add to configuration table
      (define initial-connection-timeout (configuration-table-initial-connection-timeout table))
      (define virtual-hosts the-virtual-hosts)
      (define access (make-hash-table))
      (define instances (make-hash-table))
      (define scripts (box (make-cache-table)))
      (define make-servlet-namespace the-make-servlet-namespace)))
  
  (define default-make-servlet-namespace (make-make-servlet-namespace))
    
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
          default-host)))
  
  (provide 
   build-configuration
   apply-default-functions-to-host-table)
  (provide/contract
   [complete-configuration (path-string? configuration-table? . -> . configuration?)]
   [complete-developer-configuration (path-string? configuration-table? . -> . configuration?)]))