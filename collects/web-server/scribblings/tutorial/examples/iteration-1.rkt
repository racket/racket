#lang web-server/insta

;; A blog is a (listof post)
;; and a post is a (make-post title body)
(define-struct post (title body))

;; BLOG: blog
;; The static blog.
(define BLOG 
  (list (make-post "First Post" "This is my first post")
        (make-post "Second Post" "This is another post")))

;; start: request -> html-response
;; Consumes a request, and produces a page that displays all of the
;; web content.
(define (start request)
  (render-blog-page BLOG request))
  
;; render-blog-page: blog request -> html-response
;; Consumes a blog and a request, and produces an html-response page
;; of the content of the blog.
(define (render-blog-page a-blog request)
  `(html (head (title "My Blog"))
         (body (h1 "My Blog")
               ,(render-posts a-blog))))
  
;; render-post: post -> html-response
;; Consumes a post, produces an html-response fragment of the post.
(define (render-post a-post)
  `(div ((class "post")) 
        ,(post-title a-post)
        (p ,(post-body a-post))))

;; render-posts: blog -> html-response
;; Consumes a blog, produces an html-response fragment
;; of all its posts.
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))
