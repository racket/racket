(module servlet-url mzscheme
  (require (lib "list.ss")
           (lib "contract.ss")
           (lib "url.ss" "net"))
  (require "../private/request-structs.ss"
           "../private/util.ss")
  
  (define-struct servlet-url (url))
  (define (servlet-url->url-string/no-continuation su)
    (define in-url (servlet-url-url su))
    (define first? (box #t))
    (url->string
     (url-replace-path
      (lambda (ps)
        (map (lambda (p/p)
               (if (unbox first?)
                   (begin0 (make-path/param (path/param-path p/p) empty)
                           (set-box! first? #f))
                   p/p))
             ps))
      in-url)))
  (define (request->servlet-url req)
    (make-servlet-url (request-uri req)))
  
  (provide/contract
   [servlet-url->url-string/no-continuation (servlet-url? . -> . string?)]
   [request->servlet-url (request? . -> . servlet-url?)]))