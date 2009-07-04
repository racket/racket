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

(define (set-when-true fn val)
  (if val
      (λ (c) (fn c val))
      (λ (c) c)))

(define-syntax o
  (syntax-rules ()
    [(o f) f]
    [(o f f2 ...) (lambda (x) (o* x f f2 ...))]))

(define-syntax o*
  (syntax-rules ()
    [(o* x) x]
    [(o* x f g ...) (f (o* x g ...))]))

(define (make-cookie name val
                     #:comment [comment #f]
                     #:domain  [domain #f]
                     #:max-age [max-age #f]
                     #:path    [path #f]
                     #:secure? [secure? #f])
  ((o (set-when-true cookie:add-comment comment)
      (set-when-true cookie:add-domain domain)
      (set-when-true cookie:add-max-age max-age)
      (set-when-true cookie:add-path path)
      (set-when-true cookie:secure secure?))
   (set-cookie name val)))

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
