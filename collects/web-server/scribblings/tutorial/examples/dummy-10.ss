#lang web-server/insta

(require "dummy-3.ss")

;; start: request -> html-response
;; Consumes a request and produces a page that displays
;; all of the web content.
(define (start request)  
  (render-blog-page 
   (initialize-blog! 
    (build-path (current-directory)
                "the-blog-data.sqlite"))
   request))

;; render-blog-page: blog request -> html-response
;; Produces an html-response page of the content of the
;; blog.
(define (render-blog-page a-blog request)
  (local [(define (response-generator make-url)        
            `(html (head (title "My Blog"))
                   (body 
                    (h1 "My Blog")
                    ,(render-posts a-blog make-url)
                    (form ((action 
                            ,(make-url insert-post-handler)))
                     (input ((name "title")))
                     (input ((name "body")))
                     (input ((type "submit")))))))          
          
          (define (insert-post-handler request)
            (define bindings (request-bindings request))
            (blog-insert-post!
             a-blog
             (extract-binding/single 'title bindings)
             (extract-binding/single 'body bindings))
            (render-blog-page a-blog (redirect/get)))]

    (send/suspend/dispatch response-generator)))

;; render-post-detail-page: post request -> html-response
;; Consumes a post and produces a detail page of the post.
;; The user will be able to either insert new comments
;; or go back to render-blog-page.
(define (render-post-detail-page a-blog a-post request)
  (local [(define (response-generator make-url)
            `(html (head (title "Post Details"))
                   (body
                    (h1 "Post Details")
                    (h2 ,(post-title a-post))
                    (p ,(post-body a-post))
                    ,(render-as-itemized-list
                      (post-comments a-post))
                    (form ((action 
                            ,(make-url insert-comment-handler)))
                          (input ((name "comment")))
                          (input ((type "submit"))))
                    (a ((href ,(make-url back-handler)))
                       "Back to the blog"))))
          
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
;; blog comment post request -> html-response
;; Consumes a comment that we intend to add to a post, as well
;; as the request. If the user follows through, adds a comment 
;; and goes back to the display page. Otherwise, goes back to 
;; the detail page of the post.
(define (render-confirm-add-comment-page a-blog a-comment
                                         a-post request)
  (local [(define (response-generator make-url)
            `(html (head (title "Add a Comment"))
                   (body
                    (h1 "Add a Comment")
                    "The comment: " (div (p ,a-comment))
                    "will be added to "                    
                    (div ,(post-title a-post))
                    
                    (p (a ((href ,(make-url yes-handler)))
                          "Yes, add the comment."))
                    (p (a ((href ,(make-url cancel-handler)))
                          "No, I changed my mind!")))))
          
          (define (yes-handler request)
            (post-insert-comment! a-blog a-post a-comment)
            (render-post-detail-page a-blog a-post (redirect/get)))
          
          (define (cancel-handler request)
            (render-post-detail-page a-blog a-post request))]

    (send/suspend/dispatch response-generator)))

;; render-post: post (handler -> string) -> html-response
;; Consumes a post, produces an html-response fragment of the post.
;; The fragment contains a link to show a detailed view of the post.
(define (render-post a-blog a-post make-url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-blog a-post request))]
    `(div ((class "post")) 
          (a ((href ,(make-url view-post-handler)))
             ,(post-title a-post))
          (p ,(post-body a-post))        
          (div ,(number->string (length (post-comments a-post)))
               " comment(s)"))))

;; render-posts: blog (handler -> string) -> html-response
;; Consumes a make-url, produces an html-response fragment
;; of all its posts.
(define (render-posts a-blog make-url)
  (local [(define (render-post/make-url a-post)
            (render-post a-blog a-post make-url))]
    `(div ((class "posts"))
          ,@(map render-post/make-url (blog-posts a-blog)))))

;; render-as-itemized-list: (listof html-response) -> html-response
;; Consumes a list of items, and produces a rendering as
;; an unorderered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

;; render-as-item: html-response -> html-response
;; Consumes an html-response, and produces a rendering
;; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))
