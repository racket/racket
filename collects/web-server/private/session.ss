(module session mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "url.ss" "net")
           "response-structs.ss"
           "request-structs.ss")
  
  ;; make-session-url: url (listof string) -> url
  ;; produce a new url for this session:
  ;;   Minimal path to the servlet.
  ;;   No query.
  ;;   No fragment.
  (define (make-session-url uri new-path)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (map (lambda (p) (make-path/param p empty))
          new-path)
     empty
     #f))
  
  (define-struct session (cust namespace servlet url))
  
  (provide/contract
   [struct session ([cust custodian?]
                    [namespace namespace?]
                    [servlet (request? . -> . response?)]
                    [url url?])]
   [lookup-session ((listof string?) . -> . (or/c session? false/c))]
   [install-session (session? (listof string?) . -> . void)]
   [new-session (custodian? namespace? url? (listof string?) . -> . session?)])
  (provide current-session)  
  
  (define current-session (make-parameter #f))
  
  (define the-session-table (make-hash-table 'weak 'equal))
  
  ;; new-session : namespace path uri (listof string) -> session
  (define (new-session cust ns uri paths)
    (define ses (make-session
                 cust
                 ns
                 (lambda (req) (error "session not initialized"))
                 (make-session-url uri paths)))
    #;(printf "New session of ~a~n" (hash-table-count the-session-table))    
    ses)
  
  (define (install-session ses paths)
    (hash-table-put! the-session-table paths ses))
  
  ;; lookup-session : (listof string) -> (union session #f)
  (define (lookup-session paths)
    (hash-table-get the-session-table paths 
                    (lambda () #f))))