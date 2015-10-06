#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define error-count (random 25))
(printf "Server error error generator starts as ~s\n" error-count)

(define (pkg-index/basic pkg-name->info all-pkgs)
  (define (write-info req pkg-name)
    (define i (pkg-name->info pkg-name))
    ;; Every 25 reseponses or so, generate a 5xx server error;
    ;; retries should mask the error:
    (set! error-count (add1 error-count))
    (cond
     [(zero? (modulo error-count 5))
      (response 500 #"Oops" (current-seconds) #f empty void)]
     [i (response/sexpr i)]
     [else
      ;; "Randomly" return #f or a 404, either of which should be
      ;; treated as not-found failure:
      (if (odd? error-count)
          (response 404 #"Not Found" (current-seconds) #f empty void)
          (response/sexpr #f))]))
  (define-values (dispatch get-url)
    (dispatch-rules
     [("pkgs-all") (lambda (req)
                     (response/sexpr (all-pkgs)))]
     [("pkgs") (lambda (req)
                 (response/sexpr (hash-keys (all-pkgs))))]
     [("pkg" "broken")
      (lambda (req)
        (response 401 #"Broken" (current-seconds) #f empty void))]
     [("pkg" (string-arg)) write-info]))
  dispatch)

(provide/contract
 [pkg-index/basic
  (-> (-> string? (or/c #f (hash/c symbol? any/c)))
      (-> hash?)
      (-> request? response?))])
