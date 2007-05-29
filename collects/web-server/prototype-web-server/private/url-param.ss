(module url-param mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "serialize.ss")
           "utils.ss")
  
  (provide/contract
   [extract-param (url? string? . -> . (or/c string? false/c))]
   [insert-param (url? string? string? . -> . url?)])
  
  ;; extract-param : url string -> string
  (define (extract-param url key)
    (define ps
      (apply append
             (map path/param-param (url-path url))))
    (let/ec esc
      (for-each (lambda (p)
                  (with-handlers ([exn? void])
                    (define l (read/string p))
                    (esc (cdr (assoc key l)))))
                ps)
      #f))
  
  ;; insert-param : url string string -> url
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url key val)
    (replace-path
     (match-lambda
       [(list)
        (list (make-path/param 
               ""
               (list (write/string (list (cons key val))))))]
       [old
        (match (reverse old)
          [(list-rest f r)
           (reverse (list* (make-path/param 
                            (path/param-path f)
                            (list (write/string
                                   (list* (cons key val)
                                          (with-handlers ([exn? (lambda _ empty)])
                                            (filter (lambda (k*v) (not (equal? key (car k*v))))
                                                    (read/string (first (path/param-param f)))))))))
                           r))])])
     in-url))
  
  ;; replace-path : (url-path -> url-path) url -> url
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