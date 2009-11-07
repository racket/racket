#lang scheme/base
(require scheme/list
         scheme/contract
         net/url)

(provide/contract
 [url-replace-path (((listof path/param?) . -> . (listof path/param?)) url? . -> . url?)]
 [url-path->string ((listof path/param?) . -> . string?)])

;; replace-path: (url-path -> url-path) url -> url
;; make a new url by replacing the path part of a url with a function
;; of the url's old path
;; also remove the query
(define (url-replace-path proc in-url)
  (let ([new-path (proc (url-path in-url))])
    (make-url
     (url-scheme in-url)
     (url-user in-url)
     (url-host in-url)
     (url-port in-url)
     (url-path-absolute? in-url)
     new-path
     empty
     (url-fragment in-url))))

;; ripped this off from url-unit.ss
(define (url-path->string strs)
  (apply string-append
         (apply append
                (map (lambda (s) (list "/" (maybe-join-params s)))
                     strs))))

;; needs to unquote things!
(define (maybe-join-params s)
  (if (string? s)
      s
      (let ([s (path/param-path s)])
        (if (string? s)
            s
            (case s
              [(same) "."]
              [(up)   ".."]
              [else (error 'maybe-join-params
                           "bad value from path/param-path: ~e" s)])))))