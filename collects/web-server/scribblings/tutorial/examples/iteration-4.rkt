#lang web-server/insta

;; A blog is a (blog posts)
;; where posts is a (listof post)
(struct blog (posts) #:mutable)

;; and post is a (post title body)
;; where title is a string, and body is a string
(struct post (title body))

;; BLOG: blog
;; The initial BLOG.
(define BLOG 
  (blog
   (list (post "Second Post" "This is another post")
         (post "First Post" "This is my first post"))))

;; blog-insert-post!: blog post -> void
;; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

;; start: request -> doesn't return
;; Consumes a request and produces a page that displays
;; all of the web content.
(define (start request)
  (render-blog-page request))

;; parse-post: bindings -> post
;; Extracts a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

;; render-blog-page: request -> doesn't return
;; Produces an HTML page of the content of the BLOG.
(define (render-blog-page request)
  (local [(define (response-generator embed/url)  
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body 
                     (h1 "My Blog")
                     ,(render-posts)
                     (form ((action 
                             ,(embed/url insert-post-handler)))
                           (input ((name "title")))
                           (input ((name "body")))
                           (input ((type "submit"))))))))
          
          (define (insert-post-handler request)
            (blog-insert-post! 
             BLOG (parse-post (request-bindings request)))
            (render-blog-page request))]
    
    (send/suspend/dispatch response-generator)))

;; render-post: post -> xexpr
;; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post")) 
        ,(post-title a-post)
        (p ,(post-body a-post))))

;; render-posts: -> xexpr
;; Consumes a blog, produces an xexpr fragment
;; of all its posts.
(define (render-posts)
  `(div ((class "posts"))
        ,@(map render-post (blog-posts BLOG))))
