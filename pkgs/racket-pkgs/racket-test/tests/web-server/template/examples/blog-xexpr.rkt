#lang racket
(require web-server/servlet
         xml
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
  `(html
    (head (title "Alonzo's Church: " ,section)
          (style ([type "text/css"])
                 ,(make-cdata #f #f "
   body {
    margin: 0px;
    padding: 10px;
   }

   #main {
    background: #dddddd;
   }")))
    (body
     (script ([type "text/javascript"])
             ,(make-cdata #f #f "
   var gaJsHost = ((\"https:\" == document.location.protocol) ?
     \"https://ssl.\" : \"http://www.\");
   document.write(unescape(\"%3Cscript src='\" + gaJsHost +
     \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
"))
     (script ([type "text/javascript"])
             ,(make-cdata #f #f "
   var pageTracker = _gat._getTracker(\"UA-YYYYYYY-Y\");
   pageTracker._trackPageview();
"))
     
     (h1 "Alonzo's Church: " ,section)
     (div ([id "main"])
          ,@body))))

(define (blog-posted title body k-url)
  `((h2 ,title)
    (p ,body)
    (h1 (a ([href ,k-url]) "Continue"))))

(define (extract-post req)
  (define title (extract-binding/single 'title (request-bindings req)))
  (define body (extract-binding/single 'body (request-bindings req)))
  (set! posts
        (list* (make-post title body empty)
               posts))
  (send/suspend
   (lambda (k-url)
     (template "Posted" (blog-posted title body k-url))))
  (display-posts))

(define (blog-posts k-url)
  (append
   (apply append 
          (for/list ([p posts])
            `((h2 ,(post-title p))
              (p ,(post-body p))
              (ul
               ,@(for/list ([c (post-comments p)])
                   `(li ,c))))))
   `((h1 "New Post")
     (form ([action ,k-url])
           (input ([name "title"]))
           (input ([name "body"]))
           (input ([type "submit"]))))))

(define (display-posts)
  (extract-post
   (send/suspend
    (lambda (k-url)
      (template "Posts" (blog-posts k-url))))))

(define (start req)
  (display-posts))

(serve/servlet start)
