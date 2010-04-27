#lang web-server/insta

;; A blog is a (make-blog posts)
;; where posts is a (listof post)
(define-struct blog (posts) #:mutable)

;; and post is a (make-post title body)
;; where title is a string, and body is a string
(define-struct post (title body))

;; BLOG: blog
;; The initial BLOG.
(define BLOG 
  (make-blog
   (list (make-post "First Post" "This is my first post")
         (make-post "Second Post" "This is another post"))))

;; blog-insert-post!: blog post -> void
;; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))
  
;; start: request -> html-response
;; Consumes a request and produces a page that displays
;; all of the web content.
(define (start request)
  (render-blog-page request))

;; parse-post: bindings -> post
;; Extracts a post out of the bindings.
(define (parse-post bindings)
  (make-post (extract-binding/single 'title bindings)
             (extract-binding/single 'body bindings)))

;; render-blog-page: request -> html-response
;; Produces an html-response page of the content of the BLOG.
(define (render-blog-page request)
  (local [(define (response-generator make-url)        
            `(html (head (title "My Blog"))
                   (body 
                    (h1 "My Blog")
                    ,(render-posts)
                    (form ((action 
                            ,(make-url insert-post-handler)))
                     (input ((name "title")))
                     (input ((name "body")))
                     (input ((type "submit")))))))
          
          (define (insert-post-handler request)
            (blog-insert-post! 
             BLOG (parse-post (request-bindings request)))
            (render-blog-page request))]

    (send/suspend/dispatch response-generator)))

;; render-post: post -> html-response
;; Consumes a post, produces an html-response fragment of the post.
(define (render-post a-post)
  `(div ((class "post")) 
        ,(post-title a-post)
        (p ,(post-body a-post))))

;; render-posts: -> html-response
;; Consumes a blog, produces an html-response fragment
;; of all its posts.
(define (render-posts)
  `(div ((class "posts"))
        ,@(map render-post (blog-posts BLOG))))
