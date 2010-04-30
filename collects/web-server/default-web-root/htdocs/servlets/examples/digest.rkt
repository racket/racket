#lang web-server/insta
(require racket/pretty)

(define private-key "private-key")
(define opaque "opaque")

(define (start req)
  (match (request->digest-credentials req)
    [#f
     (make-response/basic
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list (make-digest-auth-header
             (format "Digest Auth Test: ~a" (gensym))
             private-key opaque)))]
    [alist
     (define check
       (make-check-digest-credentials
        (password->digest-HA1 (lambda (username realm) "pass"))))
     (define pass?
       (check "GET" alist))
     `(html (head (title "Digest Auth Test"))
            (body 
             (h1 ,(if pass? "Pass!" "No Pass!"))
             (pre ,(pretty-format alist))))]))
