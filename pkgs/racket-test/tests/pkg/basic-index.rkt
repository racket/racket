#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (pkg-index/basic pkg-name->info all-pkgs)
  (define (write-info req pkg-name)
    (response/sexpr (pkg-name->info pkg-name)))
  (define-values (dispatch get-url)
    (dispatch-rules
     [("pkgs-all") (lambda (req)
                     (response/sexpr (all-pkgs)))]
     [("pkgs") (lambda (req)
                 (response/sexpr (hash-keys (all-pkgs))))]
     [("pkg" (string-arg)) write-info]))
  dispatch)

(provide/contract
 [pkg-index/basic
  (-> (-> string? (hash/c symbol? any/c))
      (-> hash?)
      (-> request? response?))])
