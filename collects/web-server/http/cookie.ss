#lang scheme
(require net/cookie
         web-server/http/request-structs
         web-server/http/response-structs
         xml
         web-server/private/xexpr
         scheme/contract)

(provide/contract 
 [make-cookie ((string? string?) (#:comment (or/c false/c string?)
                                            #:domain (or/c false/c valid-domain?)
                                            #:max-age (or/c false/c exact-nonnegative-integer?)
                                            #:path (or/c false/c string?)
                                            #:secure? (or/c false/c boolean?))
                                 . ->* . cookie?)]
 [cookie->header (cookie? . -> . header?)]
 [xexpr-response/cookies ((listof cookie?) pretty-xexpr/c . -> . response/full?)])

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
                     #:secure? [secure? #f])
  (setter (set-cookie name val)
          (cookie:add-comment comment)
          (cookie:add-domain domain)
          (cookie:add-max-age max-age)
          (cookie:add-path path)
          (cookie:secure secure?)))

;; cookie->header : cookie -> header
;; gets the header that will set the given cookie
(define (cookie->header cookie)
  (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie))))

;; build-cookie-response : xexpr[xhtml] (listof cookie) -> response
(define (xexpr-response/cookies cookies xexpr)
  (make-response/full 
   200
   #"Okay"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   (map cookie->header cookies) ; rfc2109 also recommends some cache-control stuff here
   (list 
    (string->bytes/utf-8 
     (xexpr->string xexpr)))))
