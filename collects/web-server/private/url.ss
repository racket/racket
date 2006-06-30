(module url mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss"))
  
  (provide/contract
   ; XXX contract maybe
   [match-url-params (string? . -> . (or/c false/c (list/c string? string? string? string?)))]
   [continuation-url? (url? . -> . (or/c boolean? (list/c number? number? number?)))]
   [embed-ids ((list/c number? number? number?) url? . -> . string?)])
  
  ;; ********************************************************************************
  ;; Parameter Embedding
  
  (define URL-PARAMS:REGEXP (regexp "([^\\*]*)\\*([^\\*]*)\\*([^\\*]*)"))
  
  (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))
  
  ;; embed-ids: (list number number number) url -> string
  (define embed-ids
    (match-lambda*
      [(list (list inst-id k-id salt) in-url)
       (insert-param
        in-url
        (format "~a*~a*~a" inst-id k-id salt))]))
  
  ;; continuation-url?: url -> (or/c (list number number number) #f)
  ;; determine if this url encodes a continuation and extract the instance id and
  ;; continuation id.
  (define (continuation-url? a-url)
    (let ([k-params (filter match-url-params
                            (apply append (map path/param-param (url-path a-url))))])
      (if (empty? k-params)
          #f
          (match (match-url-params (first k-params))
            [(list s instance k-id salt)
             (let ([instance/n (string->number instance)]
                   [k-id/n (string->number k-id)]
                   [salt/n (string->number salt)])
               (if (and (number? instance/n)
                        (number? k-id/n)
                        (number? salt/n))
                   (list instance/n
                         k-id/n
                         salt/n)
                   ; XXX: Maybe log this in some way?
                   #f))]))))
  
  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (url->string
     (replace-path
      (lambda (old-path)        
        (if (empty? old-path)
            (list (make-path/param "" (list new-param-str)))
            (list* (make-path/param (path/param-path (first old-path))
                                    (list new-param-str))
                   (rest old-path))))
      in-url)))
  
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
       (url-path-absolute? in-url)
       new-path
       empty
       (url-fragment in-url)))))
