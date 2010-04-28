#lang racket
(require net/url
         web-server/http)

(define-match-expander url/path
  (syntax-rules ()
    [(_ path-pat)
     ; url = scheme, user, host, port, absolute?, path, query, fragment
     (struct url (_ _ _ _ _ path-pat _ _))]))

(define-match-expander url/paths
  (syntax-rules ()
    [(_ path-pat ...)
     (url/path (app (lambda (ps) (map path/param-path ps))
                   (list path-pat ...)))]))

(define-match-expander request/url
  (syntax-rules ()
    [(_ url-pat)
     ; req = method, url, headers, bindings, post-data, host-ip, host-port, client-ip
     (struct request (_ url-pat _ _ _ _ _ _))]))

(provide url/path
         url/paths
         request/url)
