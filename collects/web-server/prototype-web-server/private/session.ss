(module session mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "request-structs.ss" "web-server")
           (lib "response.ss" "web-server"))
  (provide current-session)
  
  (define-struct session (id cust namespace servlet url))
  
  (provide/contract
   [struct session ([id number?]
                    [cust custodian?]
                    [namespace namespace?]
                    [servlet (request? . -> . response?)]
                    [url url?])]
   [lookup-session (number? . -> . (or/c session? false/c))]
   [new-session (custodian? namespace? url? . -> . session?)])
  
  (define current-session (make-parameter #f))
  
  ;; new-session-id: -> number
  (define new-session-id
    (let ([ses-id 0])
      (lambda ()
        (set! ses-id (add1 ses-id))
        ses-id)))
  
  (define the-session-table (make-hash-table))
  
  ;; new-session: namespace path -> session
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
  
  ;; lookup-session: number -> (union session #f)
  (define (lookup-session ses-id)
    (hash-table-get the-session-table ses-id (lambda () #f)))
  
  ;; encode-session: url number -> url
  (define (encode-session a-url ses-id)
    (insert-param a-url (number->string ses-id)))
  
  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (replace-path
     (lambda (old-path)
       (if (null? old-path)
           (list (make-path/param "" (list new-param-str)))
           (let* ([car-old-path (car old-path)])
             (cons (make-path/param (if (path/param? car-old-path)
                                        (path/param-path car-old-path)
                                        car-old-path)
                                    (list new-param-str))
                   (cdr old-path)))))
     in-url))
  
  ;; replace-path: (url-path -> url-path) url -> url
  ;; make a new url by replacing the path part of a url with a function
  ;; of the url's old path
  ;; also remove the query
  (define (replace-path proc in-url)
    (let ([new-path (proc (url-path in-url))])
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       #t
       new-path
       (url-query in-url)
       (url-fragment in-url)))))