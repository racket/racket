#lang racket/base
(require racket/contract
         racket/match
         racket/string
         racket/promise
         "url-string.rkt")

(define-logger net/url)

(define proxiable-url-schemes
  (list "http"))

(define (supported-proxy-url-scheme? scm)
  (member scm proxiable-url-schemes))

;; envars is a list of environment variables to check
;; usually "plt_<xxx>_proxy", then "<xxx>_proxy" then "proxy", but that's defined by caller
(define (parse-proxy-envars . envars)
  (define (inr envar)
    (define v (getenv envar))
    (match v
      [(or #f "") #f]
      [(app string->url
            (url (? supported-proxy-url-scheme? scheme) user (? string? host) (? integer? port)
                 _ path query fragment))
       (unless (and (not user) (null? path) (null? query) (not fragment))
         (log-net/url-error "$~a contains ignored URL components" envar))
       (list scheme host port)]
      [inv (log-net/url-error "$~a contained invalid proxy URL format: ~s" envar inv)
           #f]))
  
  (for*/first ((envar (in-list envars))
               (cpse (in-value (inr envar)))
               #:when cpse)
    cpse))

(define (valid-proxy-servers-entry? v)
  (and (list? v)
       (= 3 (length v))
       (supported-proxy-url-scheme? (car v))
       (string? (car v))
       (exact-integer? (caddr v))
       (<= 1 (caddr v) 65535)))

(define current-proxy-servers
  (make-parameter null
                  (lambda (v)
                    (unless (and (list? v)
                                 (andmap valid-proxy-servers-entry? v))
                      (raise-type-error
                       'current-proxy-servers
                       "list of list of scheme, string, and exact integer in [1,65535]"
                       v))
                    (map (lambda (v)
                           (list (string->immutable-string (car v))
                                 (string->immutable-string (cadr v))
                                 (caddr v)))
                         v))))

;; envars is a list of environment variables to check
;; usually "plt_<xxx>_proxy", then "<xxx>_proxy" then "proxy", but that's defined by caller
;; no proxy servers have no URL scheme since they describe the network topology
(define (parse-no-proxy-envars . envars)
  (define (inr envar)
    (match (getenv envar)
      [(or #f "") #f]
      [hostnames (string-split hostnames ",")]))
  
  (for*/first ((envar (in-list envars))
               (cpse (in-value (inr envar)))
               #:when cpse)
    cpse))

(define current-no-proxy-servers
  (make-parameter null
                  (lambda (v)
                    (unless (and (list? v)
                                 (andmap (lambda (v)
                                           (or (string? v)
                                               (regexp? v)))
                                         v))
                      (raise-type-error 'current-no-proxy-servers
                                        "list of string or regexp"
                                        v))
                    (map (match-lambda
                           [(? regexp? re) re]
                           [(regexp "^(\\..*)$" (list _ m))
                            (regexp (string-append ".*" (regexp-quote m)))]
                           [(? string? s) (regexp (string-append "^"(regexp-quote s)"$"))])
                         v))))

;; force this (and add-no-proxies-from-environment-promise) if you're using (current-proxy-servers)
;; outside of proxy-server-for
;;
;; exported for testing, but not documented
(define (make-add/parse-proxies-promise)
  (delay
    (for ((scm (in-list proxiable-url-schemes))
          #:unless (memf (lambda (cand) (string=? scm (car cand)))
                         (current-proxy-servers)))
      (define proxy-from-env (parse-proxy-envars
                              (format "plt_~a_proxy" scm)
                              (format "~a_proxy" scm)
                              "proxy"))
      (when proxy-from-env (current-proxy-servers
                            (cons proxy-from-env (current-proxy-servers)))))))

(define current-parse/add-proxies-promise
  (make-parameter (make-add/parse-proxies-promise)))

;; exported for testing, but not documented
(define (make-add/parse-no-proxies-promise)
  (delay
    (define no-proxy-from-env (parse-no-proxy-envars "plt_no_proxy"
                                                     "no_proxy"))
    (when no-proxy-from-env
      (current-no-proxy-servers
       (append no-proxy-from-env
               (current-no-proxy-servers))))))

(define current-parse/add-no-proxies-promise
  (make-parameter (make-add/parse-no-proxies-promise)))

(define (proxy-server-for url-schm (dest-host-name #f))
  (force (current-parse/add-proxies-promise))
  (force (current-parse/add-no-proxies-promise))
  (let ((rv (assoc url-schm (current-proxy-servers))))
    (cond [(not dest-host-name) rv]
          [(memf (lambda (np)
                   (regexp-match np dest-host-name))
                 (current-no-proxy-servers)) #f]
          [else rv])))


;; documented functions are explicitly reprovided through net/url
(provide/contract
 (proxiable-url-schemes (listof string?))
 (valid-proxy-servers-entry? (-> (list/c string? string? number?) boolean?))
 (current-proxy-servers
  (parameter/c (or/c false/c (listof (list/c string? string? number?)))))
 (current-no-proxy-servers
  (parameter/c (or/c false/c (listof (or/c string? regexp?)))))
 (make-add/parse-proxies-promise (-> (promise/c void?)))
 (make-add/parse-no-proxies-promise (-> (promise/c void?)))
 (proxy-server-for (->* (string?) ((or/c false/c string?))
                        (or/c false/c (list/c string? string? number?)))))
