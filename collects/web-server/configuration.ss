(module configuration mzscheme
  (require (lib "unitsig.ss")
           (lib "kw.ss")
           (lib "list.ss")
           (lib "contract.ss"))
  (require "private/configuration.ss"
           "private/configuration-structures.ss"
           "private/configuration-table-structs.ss"
           "private/util.ss"
           "private/parse-table.ss"
           "sig.ss")
  
  (define default-configuration-table-path
    (build-path (collection-path "web-server") "configuration-table"))
  
  ; get-configuration : path -> configuration-table
  (define (get-configuration table-file-name)
    (parse-configuration-table (call-with-input-file table-file-name read)))
  
  ; load-configuration : path -> configuration
  (define (load-configuration table-file-name)
    (complete-configuration (directory-part table-file-name) (get-configuration table-file-name)))
  
  ; load-configuration-sexpr : sexp -> configuration
  (define/kw (load-configuration-sexpr sexpr
                                       #:other-keys bct-keys)                                       
    (define table (parse-configuration-table sexpr))
    (apply build-configuration table
           (lambda (host)
             (configuration-table-default-host table))
           bct-keys))
  
  ; load-developer-configuration : path -> configuration
  (define (load-developer-configuration table-file-name)
    (complete-developer-configuration
     (directory-part table-file-name)
     (get-configuration table-file-name)))
  
  ; build-developer-configuration : tst -> configuration-table
  (define (build-developer-configuration s-expr)
    (complete-configuration ; used to be: complete-developer-configuration
     (directory-part default-configuration-table-path)
     (parse-configuration-table s-expr)))
  
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
  
  (provide/contract
   [complete-configuration (path-string? configuration-table? . -> . configuration?)]
   [get-configuration (path-string? . -> . configuration-table?)]
   ; XXX contract
   [build-developer-configuration (list? . -> . configuration?)]
   [default-configuration-table-path path?]
   [update-configuration (configuration? (listof (cons/c symbol? any/c)) . -> . configuration?)]
   [load-configuration-sexpr (list? . -> . configuration?)]
   [load-configuration (path-string? . -> . configuration?)]
   [load-developer-configuration (path-string? . -> . configuration?)]))
