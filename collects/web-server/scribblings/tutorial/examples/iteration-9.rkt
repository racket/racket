#lang web-server/insta

(require "model-2.rkt")

;; start: request -> doesn't return
;; Consumes a request and produces a page that displays
;; all of the web content.
(define (start request)  
  (render-blog-page 
   (initialize-blog! 
    (build-path (current-directory)
                "the-blog-data.db"))
   request))

;; render-blog-page: blog request -> doesn't return
;; Produces an HTML page of the content of the
;; blog.
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url) 
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body 
                     (h1 "My Blog")
                     ,(render-posts a-blog embed/url)
                     (form ((action 
                             ,(embed/url insert-post-handler)))
                           (input ((name "title")))
                           (input ((name "body")))
                           (input ((type "submit"))))))))          
          
          (define (insert-post-handler request)
            (define bindings (request-bindings request))
            (blog-insert-post! 
             a-blog
             (extract-binding/single 'title bindings)
             (extract-binding/single 'body bindings))
            (render-blog-page a-blog (redirect/get)))]

    (send/suspend/dispatch response-generator)))

;; render-post-detail-page: post request -> doesn't return
;; Consumes a post and produces a detail page of the post.
;; The user will be able to either insert new comments
;; or go back to render-blog-page.
(define (render-post-detail-page a-blog a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Post Details"))
                    (body
                     (h1 "Post Details")
                     (h2 ,(post-title a-post))
                     (p ,(post-body a-post))
                     ,(render-as-itemized-list 
                       (post-comments a-post))
                     (form ((action 
                             ,(embed/url insert-comment-handler)))
                           (input ((name "comment")))
                           (input ((type "submit"))))
                     (a ((href ,(embed/url back-handler)))
                        "Back to the blog")))))
          
          (define (parse-comment bindings)
            (extract-binding/single 'comment bindings))
          
          (define (insert-comment-handler request)
            (render-confirm-add-comment-page
             a-blog
             (parse-comment (request-bindings request))
             a-post
             request))

          (define (back-handler request)
            (render-blog-page a-blog request))]

    (send/suspend/dispatch response-generator)))

;; render-confirm-add-comment-page :
;; blog comment post request -> doesn't return
;; Consumes a comment that we intend to add to a post, as well
;; as the request. If the user follows through, adds a comment 
;; and goes back to the display page. Otherwise, goes back to 
;; the detail page of the post.
(define (render-confirm-add-comment-page a-blog a-comment
                                         a-post request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Add a Comment"))
                    (body
                     (h1 "Add a Comment")
                     "The comment: " (div (p ,a-comment))
                     "will be added to "                    
                     (div ,(post-title a-post))
                     
                     (p (a ((href ,(embed/url yes-handler)))
                           "Yes, add the comment."))
                     (p (a ((href ,(embed/url cancel-handler)))
                           "No, I changed my mind!"))))))
          
          (define (yes-handler request)
            (post-insert-comment! a-blog a-post a-comment)
            (render-post-detail-page a-blog a-post (redirect/get)))
          
          (define (cancel-handler request)
            (render-post-detail-page a-blog a-post request))]

    (send/suspend/dispatch response-generator)))

;; render-post: post (handler -> string) -> xexpr
;; Consumes a post, produces an xexpr fragment of the post.
;; The fragment contains a link to show a detailed view of the post.
(define (render-post a-blog a-post embed/url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-blog a-post request))]
    `(div ((class "post")) 
          (a ((href ,(embed/url view-post-handler)))
             ,(post-title a-post))
          (p ,(post-body a-post))        
          (div ,(number->string (length (post-comments a-post)))
               " comment(s)"))))

;; render-posts: blog (handler -> string) -> xexpr
;; Consumes a embed/url, produces an xexpr fragment
;; of all its posts.
(define (render-posts a-blog embed/url)
  (local [(define (render-post/embed/url a-post)
            (render-post a-blog a-post embed/url))]
    `(div ((class "posts"))
          ,@(map render-post/embed/url (blog-posts a-blog)))))

;; render-as-itemized-list: (listof xexpr) -> xexpr
;; Consumes a list of items, and produces a rendering as
;; an unorderered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

;; render-as-item: xexpr -> xexpr
;; Consumes an xexpr, and produces a rendering
;; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))
