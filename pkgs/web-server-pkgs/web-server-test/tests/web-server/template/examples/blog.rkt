#lang racket
(require web-server/templates
         web-server/servlet
         web-server/servlet-env)

(define-struct post (title body comments))

(define posts
  (list 
   (make-post
    "(Y Y) Works: The Why of Y"
    "..."
    (list 
     "First post! - A.T."
     "Didn't I write this? - Matthias"))
   (make-post
    "Church and the States"
    "As you may know, I grew up in DC, not technically a state..."
    (list
     "Finally, A Diet That Really Works! As Seen On TV"))))

(define (template section body)
  (list TEXT/HTML-MIME-TYPE
        (include-template "blog.html")))

(define (extract-post req)
  (define title (extract-binding/single 'title (request-bindings req)))
  (define body (extract-binding/single 'body (request-bindings req)))
  (set! posts
        (list* (make-post title body empty)
               posts))
  (send/suspend
   (lambda (k-url)
     (template "Posted" (include-template "blog-posted.html"))))
  (display-posts))

(define (display-posts)
  (extract-post
   (send/suspend
    (lambda (k-url)
      (template "Posts" (include-template "blog-posts.html"))))))

(define (start req)
  (display-posts))

(serve/servlet start)
