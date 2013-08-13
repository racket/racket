#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (request-binding/string req id [fail? #t])
  (define res
    (bindings-assq (string->bytes/utf-8 id)
                   (request-bindings/raw req)))
  (cond
    [res
     (bytes->string/utf-8
      (binding:form-value
       res))]
    [fail?
     (error 'pnr "Missing field ~e" id)]
    [else
     #f]))

(define (pkg-index/basic+versions get-pkgs pkg-name->info)
  (define (req-version req) (request-binding/string req "version" #f))
  (define (write-info req pkg-name)
    (response/sexpr
     (pkg-name->info pkg-name
                     #:version (req-version req))))
  (define (display-info req pkg-name)
    (define info (pkg-name->info pkg-name #:version (req-version req)))
    (response/xexpr
     `(html
       (body
        (h1 ,pkg-name)
        (p (a ([href ,(hash-ref info 'source)]) ,(hash-ref info 'source)))
        (p ,(hash-ref info 'checksum))))))
  (define (list-pkgs req)
    (response/xexpr
     `(html
       (body
        (table
         (tr (th "Package"))
         ,@(for/list ([n (in-list (sort (get-pkgs) string<=?))])
             `(tr
               (td (a ([href ,(get-url display-info n)]) ,n)))))))))
  (define (write-pkgs req)
    (response/sexpr (get-pkgs)))
  (define (write-pkgs/all req)
    (response/sexpr
     (for/hash ([n (in-list (get-pkgs))])
       (values n (pkg-name->info n #:version (req-version req))))))
  (define-values (dispatch get-url)
    (dispatch-rules
     [() list-pkgs]
     [("") list-pkgs]
     [("pkgs") write-pkgs]
     [("pkgs-all") write-pkgs/all]
     [("pkg" (string-arg) "display") display-info]
     [("pkg" (string-arg)) write-info]))
  dispatch)

(define (pkg-index/basic get-pkgs pkg-name->info)
  (pkg-index/basic+versions
   get-pkgs
   (λ (pkg-name #:version version)
     (pkg-name->info pkg-name))))

(provide
 (contract-out
  [response/sexpr
   (-> any/c
       response?)]
  [request-binding/string
   (->* (request? string?)
        (boolean?)
        (or/c string? false/c))]
  [pkg-index/basic+versions
   (-> (-> (listof string?))
       (-> string? #:version (or/c string? false/c)
           (hash/c symbol? any/c))
       (-> request? response?))]
  [pkg-index/basic
   (-> (-> (listof string?))
       (-> string? (hash/c symbol? any/c))
       (-> request? response?))]))
