#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (pkg-index/basic get-pkgs pkg-name->info)
  (define (write-info req pkg-name)
    (response/sexpr (pkg-name->info pkg-name)))
  (define (display-info req pkg-name)
    (define info (pkg-name->info pkg-name))
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
       (values n (pkg-name->info n)))))
  (define-values (dispatch get-url)
    (dispatch-rules
     [() list-pkgs]
     [("") list-pkgs]
     [("pkgs") write-pkgs]
     [("pkgs-all") write-pkgs/all]
     [("pkg" (string-arg) "display") display-info]
     [("pkg" (string-arg)) write-info]))
  dispatch)

(provide/contract
 [pkg-index/basic
  (-> (-> (listof string?))
      (-> string? (hash/c symbol? any/c))
      (-> request? response?))])
