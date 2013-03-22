#lang racket/base
(require racket/string
         racket/contract
         xml
         xml/path
         racket/port
         net/url
         file/md5
         pkg/util)

(define (gravatar-hash email)
  (bytes->string/utf-8
   (md5
    (string-downcase
     (string-trim email)))))

(module+ test
  (require rackunit)
  (check-equal? (gravatar-hash "MyEmailAddress@example.com ")
                "0bc83cb571cd1c50ba6f3e8a78ef1346")
  (check-equal? (gravatar-hash "MyEmailAddress@example.com ")
                (gravatar-hash "    MyEmailAddress@example.com     ")))

(define (gravatar-image-url email)
  (format "https://secure.gravatar.com/avatar/~a.jpg?d=retro"
          (gravatar-hash email)))

(define (gravatar-profile email)
  (parameterize ([collapse-whitespace #t]
                 [xexpr-drop-empty-attributes #t])
    (call/input-url+200
     (string->url
      (format "http://www.gravatar.com/~a.xml"
              (gravatar-hash email)))
     (compose string->xexpr port->string))))

(define (gravatar-display-name email)
  (define profile (gravatar-profile email))
  (and profile
       (se-path* '(response entry displayName)
                 profile)))

(module+ test
  (check-equal? (gravatar-display-name "jay.mccarthy@gmail.com")
                "Jay McCarthy")
  (check-equal? (gravatar-display-name "jay@racket-lang.org")
                #f))

(provide/contract
 [gravatar-display-name (-> string? (or/c string? false/c))]
 [gravatar-profile (-> string? xexpr?)]
 [gravatar-image-url (-> string? string?)])
