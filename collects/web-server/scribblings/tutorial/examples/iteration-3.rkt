#lang web-server/insta

;; A blog is a (listof post)
;; and a post is a (post title body)
(struct post (title body))

;; BLOG: blog
;; The static blog.
(define BLOG 
  (list (post "Second Post" "This is another post")
        (post "First Post" "This is my first post")))

;; start: request -> doesn't return
;; Consumes a request and produces a page that displays all of the
;; web content.
(define (start request)
  (render-blog-page BLOG request))

;; parse-post: bindings -> post
;; Extracts a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

;; render-blog-page: blog request -> doesn't return
;; Consumes a blog and a request, and produces an HTML page
;; of the content of the blog.
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)  
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body 
                     (h1 "My Blog")
                     ,(render-posts a-blog)
                     (form ((action 
                             ,(embed/url insert-post-handler)))
                           (input ((name "title")))
                           (input ((name "body")))
                           (input ((type "submit"))))))))
          
          (define (insert-post-handler request)
            (render-blog-page 
             (cons (parse-post (request-bindings request))
                   a-blog)
             request))]
    
    (send/suspend/dispatch response-generator)))

;; render-post: post -> xexpr
;; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post")) 
        ,(post-title a-post)
        (p ,(post-body a-post))))

;; render-posts: blog -> xexpr
;; Consumes a blog, produces an xexpr fragment
;; of all its posts.
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))
