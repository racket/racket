(module url mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "struct.ss"))
  (require "private/url.ss"
           "request-structs.ss")
           
  ;; URL parsing
  (provide (struct servlet-url (protocol host port servlets-root instance-id k-id nonce servlet-path extra-path))
           servlet-url->url-string
           servlet-url->url-string/no-continuation
           servlet-url->servlet-url/no-extra-path 
           request->servlet-url
           uri->servlet-url)
  (define-struct servlet-url (protocol host port
                                       servlets-root 
                                       instance-id k-id nonce 
                                       servlet-path extra-path))
  (define (servlet-url->url-string/no-continuation su)
    (url->string
     (make-url (servlet-url-protocol su)
               #f
               #f ;(servlet-url-host su)
               #f ;(servlet-url-port su)
               #t
               (append (map (lambda (p/p)
                              (if (and (not (empty? (path/param-param p/p)))
                                       ; XXX: not robust
                                       (match-url-params (first (path/param-param p/p))))
                                  (make-path/param (path/param-path p/p) empty)
                                  p/p))
                            (servlet-url-servlets-root su))
                       (servlet-url-servlet-path su)
                       (servlet-url-extra-path su))
               empty
               #f)))
  (define (servlet-url->url-string su)
    (let ([the-url
           (make-url (servlet-url-protocol su)
                         #f
                         #f ;(servlet-url-host su)
                         #f ;(servlet-url-port su)
                         #t
                         (append (reverse (rest (reverse (servlet-url-servlets-root su))))
                                 (list (make-path/param (path/param-path (first (reverse (servlet-url-servlets-root su))))
                                                        empty))
                                 (servlet-url-servlet-path su)
                                 (servlet-url-extra-path su))
                         empty
                         #f)])
      (if (and (servlet-url-instance-id su)
               (servlet-url-k-id su) 
               (servlet-url-nonce su))               
          (embed-ids (list (servlet-url-instance-id su)
                           (servlet-url-k-id su) 
                           (servlet-url-nonce su))
                     the-url)
          (url->string the-url))))
  (define (servlet-url->servlet-url/no-extra-path su)
    (copy-struct servlet-url su
                 [servlet-url-extra-path empty]))
  (define (request->servlet-url req)
    (uri->servlet-url (request-uri req)
                      (request-host-ip req)
                      (request-host-port req)))
  (define uri->servlet-url
    (opt-lambda (uri [default-host #f] [default-port #f])
      (let-values ([(k-instance k-id k-salt)
                    (let ([k-parts (continuation-url? uri)])
                      (if k-parts
                          (apply values k-parts)
                          (values #f #f #f)))]
                   [(servlet-path path)
                    (let loop ([servlet-path empty]
                               [path (rest (url-path uri))])
                      (if (empty? path)
                          (values servlet-path path)
                          (let ([cur (first path)])
                            (if (regexp-match "\\.ss$" (path/param-path cur))
                                (values (append servlet-path (list cur))
                                        (rest path))
                                (loop (append servlet-path (list cur))
                                      (rest path))))))])
        (make-servlet-url (url-scheme uri)
                          (or (url-host uri) default-host)
                          (or (url-port uri) default-port)
                          (list (first (url-path uri)))
                          k-instance k-id k-salt
                          servlet-path
                          path)))))
