#lang racket/base
(require racket/contract
         web-server/servlet
         web-server/private/xexpr
         "lib.rkt")

(provide/contract
 [send/formlet ((formlet*/c)
                (#:method (or/c "GET" "POST" "get" "post")
                 #:wrap (pretty-xexpr/c . -> . pretty-xexpr/c))
                . ->* .
                any)])

(define (send/formlet f
                      #:method
                      [method "POST"]
                      #:wrap 
                      [wrapper
                       (lambda (form-xexpr)
                         `(html (head (title "Form Entry"))
                                (body ,form-xexpr)))])
  (formlet-process 
   f
   (send/suspend
    (lambda (k-url)
      (response/xexpr
       (wrapper
        `(form ([action ,k-url] [method ,method])
               ,@(formlet-display f))))))))

(provide/contract
 [embed-formlet 
  (((request? . -> . any) . -> . string?)
   formlet*/c
   . -> .
   pretty-xexpr/c)])

(define (embed-formlet embed/url f)
  `(form ([action ,(embed/url
                    (lambda (r)
                      (formlet-process f r)))])
         ,@(formlet-display f)))
