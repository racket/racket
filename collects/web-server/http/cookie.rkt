#lang racket/base
(require net/cookie
         web-server/http/request-structs
         web-server/http/response-structs
         xml
         web-server/private/xexpr
         racket/contract)

(provide/contract 
 [make-cookie ((cookie-name? cookie-value?)
               (#:comment (or/c false/c string?)
                #:domain (or/c false/c valid-domain?)
                #:max-age (or/c false/c exact-nonnegative-integer?)
                #:path (or/c false/c string?)
                #:expires (or/c false/c string?)
                #:secure? (or/c false/c boolean?))
               . ->* . cookie?)]
 [cookie->header (cookie? . -> . header?)])

(define-syntax setter
  (syntax-rules ()
    [(_ e)
     e]
    [(_ e (f arg) . more)
     (let ([x e])
       (setter (if arg
                   (f x arg)
                   x)
               . more))]))

(define (make-cookie name val
                     #:comment [comment #f]
                     #:domain  [domain #f]
                     #:max-age [max-age #f]
                     #:path    [path #f]
                     #:expires [expires #f]
                     #:secure? [secure? #f])
  (setter (set-cookie name val)
          (cookie:add-comment comment)
          (cookie:add-domain domain)
          (cookie:add-expires expires)
          (cookie:add-max-age max-age)
          (cookie:add-path path)
          (cookie:secure secure?)))

;; cookie->header : cookie -> header
;; gets the header that will set the given cookie
(define (cookie->header cookie)
  (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie))))

