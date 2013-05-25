#lang racket/base
(require racket/local
         racket/list)

;; A blog is a (blog home posts)
;; where home is a string, posts is a (listof post)
(struct blog (home posts) #:mutable #:prefab)

;; and post is a (post blog title body comments)
;; where title is a string, body is a string,
;; and comments is a (listof string)
(struct post (title body comments) #:mutable #:prefab)

;; initialize-blog! : path? -> blog
;; Reads a blog from a path, if not present, returns default
(define (initialize-blog! home)
  (local [(define (log-missing-exn-handler exn)
            (blog
             (path->string home)
             (list (post "Second Post"
                         "This is another post"
                         (list))
                   (post "First Post"
                         "This is my first post"
                         (list "First comment!")))))
          (define the-blog
            (with-handlers ([exn? log-missing-exn-handler])
              (with-input-from-file home read)))]
    (set-blog-home! the-blog (path->string home))
    the-blog))

;; save-blog! : blog -> void
;; Saves the contents of a blog to its home
(define (save-blog! a-blog)
  (local [(define (write-to-blog)
            (write a-blog))]
    (with-output-to-file (blog-home a-blog) 
      write-to-blog
      #:exists 'replace)))

;; blog-insert-post!: blog string string -> void
;; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog title body)
  (set-blog-posts! 
   a-blog
   (cons (post title body empty) (blog-posts a-blog)))
  (save-blog! a-blog))

;; post-insert-comment!: blog post string -> void
;; Consumes a blog, a post and a comment string.  As a side-efect, 
;; adds the comment to the bottom of the post's list of comments.
(define (post-insert-comment! a-blog a-post a-comment)
  (set-post-comments!
   a-post
   (append (post-comments a-post) (list a-comment)))
  (save-blog! a-blog))

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
