#lang racket/base

(provide serve-git-http-proxy!)

(require net/base64
         net/uri-codec
         net/url
         racket/match
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         web-server/http
         web-server/servlet-env)

(define (url-path-string url)
  (string-join (map path/param-path (url-path url))
               "/" #:before-first "/"))

(define (url-query-string url)
  (alist->form-urlencoded (url-query url)))

(define (verify-authorization header-value)
  ; strip #"Basic " off of the header value
  (define encoded-value (subbytes header-value 6))
  (equal? (string-split (bytes->string/utf-8 (base64-decode encoded-value)) ":")
          '("user" "password")))

(define (serve-git-http-proxy req)
  ; check if the right Authorization header is provided
  (define authorization (headers-assq* #"Authorization" (request-headers/raw req)))
  (cond
    [(and authorization (verify-authorization (header-value authorization)))
     (parameterize ([current-environment-variables (environment-variables-copy
                                                    (current-environment-variables))])
       ; git-http-backend depends on these environment variables to find the git repo
       (putenv "GIT_PROJECT_ROOT" (path->string (find-system-path 'temp-dir)))
       (putenv "GIT_HTTP_EXPORT_ALL" "")

       ; set standard CGI environment variables
       (environment-variables-set! (current-environment-variables)
                                   #"REQUEST_METHOD" (request-method req))
       (putenv "PATH_INFO" (url-path-string (request-uri req)))
       (putenv "QUERY_STRING" (url-query-string (request-uri req)))

       (let ([content-type (headers-assq* #"Content-Type" (request-headers/raw req))])
         (when content-type
           (environment-variables-set! (current-environment-variables)
                                       #"CONTENT_TYPE" (header-value content-type))))

       ; run git-http-backend
       (match-define (list git-response-port git-body-port _ git-info-port _)
         (process* (find-executable-path "git") "http-backend"))

       ; pass POST body to git-http-backend
       (when (request-post-data/raw req)
         (write-bytes (request-post-data/raw req) git-body-port))
       (close-output-port git-body-port)

       ; convert CGI headers to ones the web server can understand
       (define headers
         (for/list ([line (in-lines git-response-port)]
                    #:break (zero? (string-length line)))
           (apply header (map string->bytes/utf-8 (string-split line ": ")))))

       ; produce a response
       (response 200 #"OK" (current-seconds) #f headers
                 (λ (out)
                   (copy-port git-response-port out)
                   (close-input-port git-response-port)
                   (close-input-port git-info-port))))]
    ; if authorization fails, return a WWW-Authenticate header
    [else (response/full 401 #"Authorization Required" (current-seconds)
                         #"text/plain; charset=utf-8"
                         (list (header #"WWW-Authenticate" #"Basic"))
                         (list #"Repository not found."))]))

(define (serve-git-http-proxy! #:port port)
  (serve/servlet serve-git-http-proxy
                 #:port port
                 #:command-line? #t
                 #:servlet-regexp #rx""))
