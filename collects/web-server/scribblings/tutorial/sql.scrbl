
@section{Using an SQL database}
@declare-exporting[#:use-sources ("iteration-10.ss" 
                                  "model-3.ss")]
@(require (prefix-in sqlite: (for-label (planet jaymccarthy/sqlite:3/sqlite))))

Our next task is to employ an SQL database for the blog model. We'll be using SQLite with the @schememodname[(planet jaymccarthy/sqlite:3/sqlite)] PLaneT package. We add the following to the top of our model:

@schemeblock[
(require (prefix-in sqlite: (planet jaymccarthy/sqlite:3/sqlite)))
]

We now have the following bindings:

@defthing[sqlite:open (path? . -> . db?)]
@defthing[sqlite:exec/ignore (db? string? . -> . void)]
@defthing[sqlite:select (db? string? . -> . (listof vector?))]
@defthing[sqlite:insert (db? string? . -> . integer?)]


The first thing we should do is decide on the relational structure of our model. We will use the following tables:

@verbatim{
 CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT, body TEXT)
 CREATE TABLE comments (pid INTEGER, content TEXT)
}

Each post will have an identifier, a title, and a body. This is the same as our old Scheme structure,
except we've added the identifier. (Actually, there was always an identifier---the memory pointer---but now
we have to make it explicit in the database.)

Each comment is tied to a post by the post's identifier and has textual content. We could have chosen to
serialize comments with @scheme[write] and add a new TEXT column to the posts table to store the value.
By adding a new comments table, we are more in accord with the relational style.

A @scheme[blog] structure will simply be a container for the database handle:

@defstruct[blog ([db db?])]

@bold{Exercise.} Write the @scheme[blog] structure definition. (It does not need to be mutable or serializable.)

We can now write the code to initialize a @scheme[blog] structure:
@schemeblock[
@code:comment{initialize-blog! : path? -> blog?}
@code:comment{Sets up a blog database (if it doesn't exist)}
(define (initialize-blog! home)
  (define db (sqlite:open home))
  (define the-blog (make-blog db))
  (with-handlers ([exn? (lambda (exn) (void))])
    (sqlite:exec/ignore db
      (string-append
       "CREATE TABLE posts"
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
]

@scheme[sqlite:open] will create a database if one does not already exist at the @scheme[home] path. But, we still need
to initialize the database with the table definitions and initial data. 

We used @scheme[blog-insert-post!] and @scheme[post-insert-comment!] to initialize the database. Let's see their implementation:

@schemeblock[
@code:comment{blog-insert-post!: blog? string? string? -> void}
@code:comment{Consumes a blog and a post, adds the post at the top of the blog.}
(define (blog-insert-post! a-blog title body)
  (sqlite:insert
   (blog-db a-blog)
   (format "INSERT INTO posts (title, body) VALUES ('~a', '~a')"
           title body)))

@code:comment{post-insert-comment!: blog? post string -> void}
@code:comment{Consumes a blog, a post and a comment string.  As a side-effect,}
@code:comment{adds the comment to the bottom of the post's list of comments.}
(define (post-insert-comment! a-blog p a-comment)
  (sqlite:insert 
   (blog-db a-blog)
   (format 
    "INSERT INTO comments (pid, content) VALUES ('~a', '~a')"
    (post-id p) a-comment)))
]

@bold{Exercise.} Find the security hole common to these two functions.

@centerline{------------}

A user could submit a post with a title like, @scheme{null', 'null') and INSERT INTO accounts (username, password) VALUES ('ur','hacked} and get our simple @scheme[sqlite:insert] to make two INSERTs instead of one. 

 This is called an SQL injection attack. It can be resolved by using
 prepared statements that let SQLite do the proper quoting for us. Refer
 to the SQLite package documentation for usage.

@centerline{------------}

In @scheme[post-insert-comment!], we used @scheme[post-id], but we have not yet defined the new @scheme[post] structure.
It @emph{seems} like a @scheme[post] should be represented by an integer id, because the post table contains an integer as the identifying value.

However, we cannot tell from this structure
what blog this posts belongs to, and therefore, what database; so, we could not extract the title or body values,
since we do not know what to query. Therefore, we should associate the blog with each post:

@defstruct[post ([blog blog?] [id integer?])]

@bold{Exercise.} Write the structure definition for posts.

The only function that creates posts is @scheme[blog-posts]:

@schemeblock[
@code:comment{blog-posts : blog -> (listof post?)}
@code:comment{Queries for the post ids}
(define (blog-posts a-blog)
  (map (compose (lambda (n) (make-post a-blog n))
                string->number
                (lambda (v) (vector-ref v 0)))
       (rest (sqlite:select (blog-db a-blog)
                            "SELECT id FROM posts"))))
]

@scheme[sqlite:select] returns a list of vectors. The first element of the list is the name of the columns.
Each vector has one element for each column. Each element is a string representation of the value.

At this point we can write the functions that operate on posts:
@schemeblock[
@code:comment{post-title : post -> string?}
@code:comment{Queries for the title}
(define (post-title a-post)
  (vector-ref 
   (second 
    (sqlite:select 
     (blog-db (post-blog a-post))
     (format 
      "SELECT title FROM posts WHERE id = '~a'"
      (post-id a-post))))
   0))
]

@bold{Exercise.} Write the definition of @scheme[post-body].
             
@bold{Exercise.} Write the definition of @scheme[post-comments].
(Hint: Use @scheme[blog-posts] as a template, not @scheme[post-title].)

@centerline{------------}

The only change that we need to make to the application is to require the new model. The interface is exactly the same!

@centerline{------------}

Our model is now:

@external-file["model-3.ss"]

And our application is:

@schememod[
web-server/insta

(require "model-3.ss")

....
]

