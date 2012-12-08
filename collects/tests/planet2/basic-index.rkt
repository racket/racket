#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (planet2-index/basic pkg-name->info)
  (define (write-info req pkg-name)
    (response/sexpr (pkg-name->info pkg-name)))
  (define-values (dispatch get-url)
    (dispatch-rules
     [("pkg" (string-arg)) write-info]))
  dispatch)

(provide/contract
 [planet2-index/basic
  (-> (-> string? (hash/c symbol? any/c))
      (-> request? response?))])
