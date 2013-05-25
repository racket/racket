#lang racket/base
(require racket/list
         racket/local
         "../dummy-sqlite.rkt")

;; A blog is a (blog db)
;; where db is an sqlite database handle
(struct blog (db))

;; A post is a (post blog id)
;; where blog is a blog and id is an integer?
(struct post (blog id))

;; initialize-blog! : path? -> blog?
;; Sets up a blog database (if it doesn't exist)
(define (initialize-blog! home)
  (define db (sqlite:open home))
  (define the-blog (blog db))
  (with-handlers ([exn? void])
    (sqlite:exec/ignore db
                        (string-append
                         "CREATE TABLE posts "
                         "(id INTEGER PRIMARY KEY,"
                         "title TEXT, body TEXT)"))
    (blog-insert-post!
     the-blog "First Post" "This is my first post")
    (blog-insert-post!
     the-blog "Second Post" "This is another post")
    (sqlite:exec/ignore
     db "CREATE TABLE comments (pid INTEGER, content TEXT)")
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)

;; blog-posts : blog -> (listof post?)
;; Queries for the post ids
(define (blog-posts a-blog)
  (local [(define (row->post a-row)
            (post a-blog (string->number (vector-ref a-row 0))))
          (define rows (sqlite:select
                        (blog-db a-blog)
                        "SELECT id FROM posts"))]
    (cond [(empty? rows)
           empty]
          [else
           (map row->post (rest rows))])))

;; post-title : post -> string?
;; Queries for the title
(define (post-title a-post)
  (vector-ref 
   (second 
    (sqlite:select 
     (blog-db (post-blog a-post))
     (format "SELECT title FROM posts WHERE id = '~a'"
             (post-id a-post))))
   0))

;; post-body : post -> string?
;; Queries for the body
(define (post-body p)
  (vector-ref 
   (second 
    (sqlite:select 
     (blog-db (post-blog p))
     (format "SELECT body FROM posts WHERE id = '~a'"
             (post-id p))))
   0))

;; post-comments : post -> (listof string?)
;; Queries for the comments
(define (post-comments p)
  (local [(define (row->comment a-row)
            (vector-ref a-row 0))
          (define rows 
            (sqlite:select
             (blog-db (post-blog p))
             (format "SELECT content FROM comments WHERE pid = '~a'"
                     (post-id p))))]
    (cond
      [(empty? rows) empty]
      [else (map row->comment (rest rows))])))

;; blog-insert-post!: blog? string? string? -> void
;; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog title body)
  (sqlite:insert
   (blog-db a-blog)
   (format "INSERT INTO posts (title, body) VALUES ('~a', '~a')"
           title body)))

;; post-insert-comment!: blog? post string -> void
;; Consumes a blog, a post and a comment string.  As a side-efect, 
;; adds the comment to the bottom of the post's list of comments.
(define (post-insert-comment! a-blog p a-comment)
  (sqlite:insert 
   (blog-db a-blog)
   (format
    "INSERT INTO comments (pid, content) VALUES ('~a', '~a')"
    (post-id p) a-comment)))

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
