#lang web-server/insta

(define (start req)
  (match (request->basic-credentials req)
    [(cons user pass)
     `(html (head (title "Basic Auth Test"))
            (body (h1 "User: " ,(bytes->string/utf-8 user))
                  (h1 "Pass: " ,(bytes->string/utf-8 pass))))]
    [else
     (response
      401 #"Unauthorized" (current-seconds) TEXT/HTML-MIME-TYPE
      (list (make-basic-auth-header (format "Basic Auth Test: ~a" (gensym))))
      void)]))
