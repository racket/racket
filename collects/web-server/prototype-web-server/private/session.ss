(module session mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")
           (lib "request-structs.ss" "web-server")
           (lib "response.ss" "web-server")
           "url-param.ss")
  (provide current-session)
  
  (define-struct session (id cust namespace servlet url))
  
  (provide/contract
   [struct session ([id number?]
                    [cust custodian?]
                    [namespace namespace?]
                    [servlet (request? . -> . response?)]
                    [url url?])]
   [extract-session (url? . -> . (or/c number? false/c))]
   [lookup-session (number? . -> . (or/c session? false/c))]
   [new-session (custodian? namespace? url? . -> . session?)])
  
  (define current-session (make-parameter #f))
  
  ;; new-session-id : -> number
  (define new-session-id
    (let ([ses-id 0])
      (lambda ()
        (set! ses-id (add1 ses-id))
        ses-id)))
  
  (define the-session-table (make-hash-table))
  
  ;; new-session : namespace path -> session
  (define (new-session cust ns uri)
    (let* ([new-id (new-session-id)]
           [ses (make-session
                 new-id
                 cust
                 ns
                 (lambda (req) (error "session not initialized"))
                 (encode-session uri new-id))])
      (hash-table-put! the-session-table new-id ses)
      ses))
  
  ;; lookup-session : number -> (union session #f)
  (define (lookup-session ses-id)
    (hash-table-get the-session-table ses-id (lambda () #f)))
  
  ;; encode-session : url number -> url
  (define (encode-session a-url ses-id)
    (insert-param a-url "s" (number->string ses-id)))
    
  ;; extract-session : url -> (union number #f)
  ;; Determine if the url encodes a session-id and extract it
  (define (extract-session a-url)
    (define id (extract-param a-url "s"))
    (with-handlers ([exn? (lambda _ #f)])
      (string->number id))))