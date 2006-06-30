(module configuration mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss"))
  (require "configuration-structures.ss"
           "configuration-table-structs.ss"
           "sig.ss"
           "util.ss"
           "parse-table.ss"
           "private/cache-table.ss"
           "response.ss")
  
  (define default-configuration-table-path
    (build-path (collection-path "web-server") "configuration-table"))
  
  ; get-configuration : path -> configuration-table
  (define (get-configuration table-file-name)
    (parse-configuration-table (call-with-input-file table-file-name read)))
  
  ; load-configuration : path -> configuration
  (define (load-configuration table-file-name)
    (complete-configuration (directory-part table-file-name) (get-configuration table-file-name)))
  
  ; load-developer-configuration : path -> configuration
  (define (load-developer-configuration table-file-name)
    (complete-developer-configuration (directory-part table-file-name)
                                      (get-configuration table-file-name)))
  
  ; build-developer-configuration : tst -> configuration-table
  (define (build-developer-configuration s-expr)
    (complete-developer-configuration (directory-part default-configuration-table-path)
                                      (parse-configuration-table s-expr)))
  
  ;; added 2/3/05 by Jacob -- Help Desk needs to support virtual hosts
  ; build-developer-configuration/vhosts : tst -> configuration-table
  (define (build-developer-configuration/vhosts s-expr)
    (complete-developer-configuration/vhosts (directory-part default-configuration-table-path)
                                             (parse-configuration-table s-expr)))
  
  ; : str configuration-table/vhosts -> configuration
  (define (complete-developer-configuration/vhosts base table)
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
  
  ; : str configuration-table -> configuration
  (define (complete-developer-configuration base table)
    (build-configuration
     table
     (gen-virtual-hosts null (apply-default-functions-to-host-table
                              base
                              (configuration-table-default-host table)))))
  
  ; : configuration-table host-table -> configuration
  (define (build-configuration table the-virtual-hosts)
    (unit/sig web-config^
      (import)
      (define port (configuration-table-port table))
      (define max-waiting (configuration-table-max-waiting table))
      (define listen-ip #f) ; more here - add to configuration table
      (define initial-connection-timeout (configuration-table-initial-connection-timeout table))
      (define virtual-hosts the-virtual-hosts)
      (define access (make-hash-table))
      (define instances (make-hash-table))
      (define scripts (box (make-cache-table)))
      (define make-servlet-namespace the-make-servlet-namespace)))
  
  ; begin stolen from commander.ss, which was stolen from private/drscheme/eval.ss
  ; FIX - abstract this out to a namespace library somewhere (ask Robby and Matthew)
  (define to-be-copied-module-specs
    '(mzscheme
      ;; allow people (SamTH) to use MrEd primitives from servlets.
      ;; GregP: putting mred.ss here is a bad idea because it will cause
      ;; web-server-text to have a dependency on mred
      ;; JM: We get around it by only doing it if the module is already attached.
      (lib "mred.ss" "mred")
      (lib "servlet.ss" "web-server")))
  
  ; JBC : added error-handler hack; the right answer is only to transfer the 'mred'
  ; module binding when asked to, e.g. by a field in the configuration file.
  ; GregP: put this back in if Sam's code breaks
  ;  (for-each (lambda (x) (with-handlers ([exn:fail? (lambda (exn) 'dont-care)])
  ;                          ; dynamic-require will fail when running web-server-text.
  ;                          ; maybe a warning message in the exception-handler?
  ;                          (dynamic-require x #f)))
  ;            to-be-copied-module-specs)
  
  ;; get the names of those modules.
  (define to-be-copied-module-names
    (let ([get-name
           (lambda (spec)
             (if (symbol? spec)
                 spec
                 ((current-module-name-resolver) spec #f #f)))])
      (map get-name to-be-copied-module-specs)))
  ; end stolen
  
  (define (the-make-servlet-namespace)
    (let ([server-namespace (current-namespace)]
          [new-namespace (make-namespace)])
      (parameterize ([current-namespace new-namespace])
        (for-each (lambda (name)
                    (with-handlers ([exn? void])
                      (namespace-attach-module server-namespace name)))
                  to-be-copied-module-names)
        new-namespace)))
  
  ; : (listof (cons sym TST)) -> configuration
  ; more here - this is ugly.  It also does not catch "unbound identifiers" since I use symbols.
  ; I considered several other solutions:
  ; - write the compound unit multiple times (no abstraction)
  ; - use opt-lambda and pass in 'please-use-the-default for unchanged flags
  ; - write three different functional updaters and re-compound the unit 1--3 times
  (define (update-configuration configuration flags)
    (compound-unit/sig
      (import)
      (link
       [config : web-config^ (configuration)]
       [new-config : web-config/local^
                   ((unit/sig web-config/local^
                      (import (raw : web-config/local^))
                      (define port (extract-flag 'port flags raw:port))
                      (define listen-ip (extract-flag 'ip-address flags raw:listen-ip))
                      (define instances (extract-flag 'instances flags raw:instances))
                      (define make-servlet-namespace (extract-flag 'namespace flags raw:make-servlet-namespace)))
                    (config : web-config/local^))])
      (export (open (config : web-config/pervasive^))
              (open (new-config : web-config/local^)))))
  
  ; error-response : nat str str [(cons sym str) ...] -> response
  ; more here - cache files with a refresh option.
  ; The server should still start without the files there, so the
  ; configuration tool still runs.  (Alternatively, find an work around.)
  (define (error-response code short text-file . extra-headers)
    (make-response/full code short
                        (current-seconds) TEXT/HTML-MIME-TYPE
                        extra-headers
                        (list (read-file text-file))))
  
  ; servlet-loading-responder : url tst -> response
  ; more here - parameterize error based on a configurable file, perhaps?
  ; This is slightly tricky since the (interesting) content comes from the exception.
  (define (servlet-loading-responder url exn)
    (make-response/full 500 "Servlet didn't load"
                        (current-seconds)
                        #"text/plain" ;TEXT/HTML-MIME-TYPE
                        '() ; check
                        (list "Servlet didn't load.\n"
                              (exn->string exn))))
  
  ; gen-servlet-not-found : str -> url -> response
  (define (gen-servlet-not-found file-not-found-file)
    (lambda (url)
      (error-response 404 "Servlet not found" file-not-found-file)))
  
  ; gen-servlet-responder : str -> url tst -> response
  (define (gen-servlet-responder servlet-error-file)
    (lambda (url exn)
      ; XXX use separate log file
      ((error-display-handler)
       (format "Servlet exception:\n~a\n" (exn-message exn))
       exn)
      (error-response 500 "Servlet error" servlet-error-file)))
  
  ; gen-servlets-refreshed : str -> -> response
  (define (gen-servlets-refreshed servlet-refresh-file)
    (lambda ()
      (error-response 200 "Servlet cache refreshed" servlet-refresh-file)))
  
  ; gen-passwords-refreshed : str -> -> response
  (define (gen-passwords-refreshed password-refresh-file)
    (lambda ()
      (error-response 200 "Passwords refreshed" password-refresh-file)))
  
  ; gen-authentication-responder : str -> url (cons sym str) -> response
  (define (gen-authentication-responder access-denied-file)
    (lambda (uri recommended-header)
      (error-response 401 "Authorization Required" access-denied-file
                      recommended-header)))
  
  ; gen-protocol-responder : str -> str -> response
  (define (gen-protocol-responder protocol-file)
    (lambda (error-message)
      (error-response 400 "Malformed Request" protocol-file)))
  
  ; gen-file-not-found-responder : str -> url -> response
  (define (gen-file-not-found-responder file-not-found-file)
    (lambda (url)
      (error-response 404 "File not found" file-not-found-file)))
  
  ; gen-collect-garbage-responder : str -> -> response
  (define (gen-collect-garbage-responder file)
    (lambda ()
      (error-response 200 "Garbage collected" file)))
  
  (define servlet?
    (let ([servlets-regexp (regexp "^/servlets/.*")])
      (lambda (str)
        (regexp-match servlets-regexp str))))
  
  ; read-file : str -> str
  (define (read-file path)
    (call-with-input-file path
      (lambda (in) (read-string (file-size path) in))))
  
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
  
  (provide/contract
   [complete-configuration (string? configuration-table? . -> . configuration?)]
   [get-configuration (string? . -> . configuration-table?)]
   ; XXX contract
   [build-developer-configuration (list? . -> . configuration?)]
   ; XXX contract
   [build-developer-configuration/vhosts (list? . -> . configuration?)]
   [default-configuration-table-path path?]
   [update-configuration (configuration? (listof (cons/c symbol? any/c)) . -> . configuration?)]
   [load-configuration (path? . -> . configuration?)]
   [load-developer-configuration (path? . -> . configuration?)])  
  (provide/contract
   [error-response ((natural-number/c string? string?) (listof (cons/c symbol? string?)) . ->* . (response?))]
   ; XXX contract
   [servlet-loading-responder (string? any/c . -> . response?)]
   [gen-servlet-not-found (string? . -> . (string? . -> . response?))]
   [gen-servlet-responder (string? . -> . (string? any/c . -> . response?))]
   [gen-servlets-refreshed (string? . -> . (-> response?))]
   [gen-passwords-refreshed (string? . -> . (-> response?))]
   [gen-authentication-responder (string? . -> . (string? (cons/c symbol? string?) . -> . response?))]
   [gen-protocol-responder (string? . -> . (string? . -> . response?))]
   [gen-file-not-found-responder (string? . -> . (string? . -> . response?))]
   [gen-collect-garbage-responder (string? . -> . (-> response?))]))